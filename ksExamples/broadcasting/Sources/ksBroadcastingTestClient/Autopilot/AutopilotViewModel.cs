using ksBroadcastingNetwork;
using ksBroadcastingNetwork.Structs;
using ksBroadcastingTestClient.Broadcasting;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ksBroadcastingTestClient.Autopilot
{
    /// <summary>
    /// Similar to the Broadcasting view model, but is meant to debug and understand decisions made by the autopilot.
    /// Is a bit of code duplication, but I wanted this to act stand-alone so it can serve as a base for a good auto-cam mod
    /// </summary>
    public class AutopilotViewModel : KSObservableObject
    {
        public AutopilotWeightsViewModel WeightsVM { get; } = new AutopilotWeightsViewModel();
        public ObservableCollection<AutopilotCarViewModel> Cars { get; } = new ObservableCollection<AutopilotCarViewModel>();

        public TrackViewModel TrackVM { get => Get<TrackViewModel>(); private set => Set(value); }
        public CameraManagementViewModel CamManagerVM { get; } = new CameraManagementViewModel();
        public KSRelayCommand RequestFocusedCarCommand { get; }
        public KSRelayCommand ToggleAutopilotCommand { get; }
        public bool IsAutopilotActive { get => Get<bool>(); private set { Set(value); NotifyUpdate(nameof(AutopilotStateText)); } }
        public string AutopilotStateText { get => IsAutopilotActive ? "Stop" : "Auto"; }

        private List<BroadcastingNetworkProtocol> _clients = new List<BroadcastingNetworkProtocol>();
        public List<CarWeightCategoryViewModel> PressureCategoryWeights { get; } = new List<CarWeightCategoryViewModel>();
        public DateTime LastFocusChange { get => Get<DateTime>(); private set => Set(value); }
        public DateTime LastCameraSetChange { get => Get<DateTime>(); private set => Set(value); }
        public DateTime LastCameraChange { get => Get<DateTime>(); private set => Set(value); }
        public int FocusedCarIndex { get; private set; }
        public string ActiveCameraSet { get; private set; }
        public string ActiveCamera { get; private set; }
        public RaceSessionType SessionType { get => Get<RaceSessionType>(); set => Set(value); }
        public string CurrentHudPage { get; private set; }
        public string AutopilotState { get => Get<string>(); private set => Set(value); }
        public ObservableCollection<string> MiniLog { get; } = new ObservableCollection<string>();

        public AutopilotViewModel()
        {
            RequestFocusedCarCommand = new KSRelayCommand(RequestFocusedCar);
            ToggleAutopilotCommand = new KSRelayCommand((o) => IsAutopilotActive = !IsAutopilotActive);
            LastFocusChange = DateTime.Now.AddSeconds(-10);

            foreach (var item in Enum.GetValues(typeof(CarWeightCategoryEnum)).Cast<CarWeightCategoryEnum>())
            {
                var weight = new CarWeightCategoryViewModel(item);
                weight.RawValue = GetDefaultCategoryWeight(item);
                PressureCategoryWeights.Add(weight);
            }
        }

        private float GetDefaultCategoryWeight(CarWeightCategoryEnum item)
        {
            switch (item)
            {
                // Add cases for overrides, leave them out for a default of 100%
                default:
                    return 1f;
            }
        }

        int cycles = 0;
        private void CalcCarRanks()
        {
            cycles++;
            // This is the one big responsibility. Once each realtime update cycle, we will walk through all the cars and try to figure out
            // what is the best car (and camera and everything) to focus right now

            // The obvious stuff is that we want a priority mix of
            // - close action
            // - watching the front guys
            // - distribute screentime across the field
            // - contacts and accidents

            // but also take more sophisticated stuff into consideration
            // - camera clipping, avoid too furious jumps
            // - action clipping - once we watch something, we want to stick there for moment at least
            // - camera selection: we should offer a healthy mix and shuffle in onboards, bonus points for understanding which camera would be nice for a given situations
            // - instant replays - in theory we should be able to jump to any contact happening, and selectively offer a replay of the seconds before. Same for overtakes we didn't cover

            // I do see multiple ways to implement this, the regular way would be to use any default decision making method - in the easiest way we could stack up categories and weights.
            // Due to the nature of the amount solutions we have, I spot a loophole and will (try to) abuse the big trick in genetic algorithms, just without the CPU intensive use. 
            #region deeper thoughts about this
            // GA's have a black magic component where you only need to describe the fitness function of a given solution. If you do this well, you do not need to understand/implement tactics,
            // the GA will explore this. But as we only have a number of cars = number of solutions, we can simply bruteforce all scenarios (= amount of cars) and decide by fitness function.
            // That one has to express either the gain or pain if we focus that car right now. Sounds a lot more fancy than it is, but it's important to understand the perspective and feed the
            // data we need
            #endregion

            var trackPositionCarList = Cars.OrderByDescending(x=>x.TrackPosition).ToList();
            // for the ease of use, we'll set the lead car to the front
            
            var maxRank = 0f;
            AutopilotCarViewModel maxRankCar = null;
            var focusChangedBeforeSeconds = (float)(DateTime.Now - LastFocusChange).TotalSeconds;
            var cameraChangedBeforeSeconds = (float)(DateTime.Now - LastCameraChange).TotalSeconds;

            var weightDict = PressureCategoryWeights.ToDictionary(x => x.Category, x => x.RawValue);
            var avgPressure = 0f;
            foreach (var item in trackPositionCarList)
            {
                item.CalcPressure(trackPositionCarList, TrackVM.TrackMeters, focusChangedBeforeSeconds, weightDict, CamManagerVM);

                if(maxRank < item.Pressure)
                {
                    maxRank = item.Pressure;
                    maxRankCar = item;
                }

                avgPressure += item.Pressure;
            }

            if (trackPositionCarList.Count > 0)
                avgPressure /= trackPositionCarList.Count;

            if (IsAutopilotActive)
            {
                // additionally, we will see what cam we want to use
                // In the first approach, we will ask if there is something to force - eg. TV cams we need to learn
                var forcedCamSet = CamManagerVM.GetForcedCameraSet();
                if (forcedCamSet != null)
                { 
                    RequestCameraChange(forcedCamSet.Set, forcedCamSet.Name);
                }
                else if(CamManagerVM.TVCamLearningProgress == 1f)
                {
                    AnyCamera preferredCamera;
                    float rawWeight;
                    maxRankCar.CalcPreferredCamera(trackPositionCarList, TrackVM.TrackMeters, cameraChangedBeforeSeconds, CamManagerVM, out preferredCamera, out rawWeight);
                    var stateStr = "";
                    if (maxRankCar != null && !maxRankCar.HasFocus)
                        stateStr = $"Want: {maxRankCar.CurrentDriverName} ({maxRank:P0})";
                    stateStr += preferredCamera != null ? $" Cam {rawWeight:P0}" : $"-";
                    AutopilotState = stateStr;

                    var isFocusChange = maxRankCar != null && !maxRankCar.HasFocus && maxRankCar.Pressure > avgPressure * 0.15f && rawWeight > 0.2f;
                    var isCameraChange = preferredCamera != null && rawWeight > 0.2f;
                    var isHudChange = false;
                    var targetHUD = "";
                    if(isCameraChange)
                    {
                        switch (preferredCamera.CamType)
                        {
                            // Onboards will get the basic hud, with rpm and such
                            case CameraTypeEnum.RearWing:
                            case CameraTypeEnum.Onboard:
                                targetHUD = "Basic HUD";
                                isHudChange = true;
                                break;

                            // Default is the broadcasting hud
                            case CameraTypeEnum.Tv1:
                            case CameraTypeEnum.Tv2:
                            case CameraTypeEnum.Helicam:
                            case CameraTypeEnum.Pitlane:
                            case CameraTypeEnum.Unknown:
                            default:
                                targetHUD = "Broadcasting";
                                isHudChange = true;
                                break;
                        }

                        if (targetHUD == CurrentHudPage)
                            isHudChange = false;
                    }

                    if(isFocusChange && isHudChange)
                    {
                        RequestFocusedCarAndCamera(maxRankCar, preferredCamera.Set, preferredCamera.Name);
                        Log($"Requested Focus: {maxRankCar.CurrentDriverName} ({maxRank:P0}) with cam {preferredCamera.Set}/{preferredCamera.Name} ({rawWeight:P0})");
                    }
                    else if(isFocusChange)
                    {
                        RequestFocusedCar(maxRankCar);
                        Log($"Requested Focus: {maxRankCar.CurrentDriverName} ({maxRank:P0})");
                    }
                    else if(isCameraChange)
                    {
                        RequestCameraChange(preferredCamera.Set, preferredCamera.Name);
                    }

                    if(false && isHudChange)
                    {
                        RequestHudPageChange(targetHUD);
                        Log($"Requested HUD change: {targetHUD}");
                    }
                }
            }
        }

        private void RequestFocusedCarAndCamera(object obj, string camSet, string camera)
        {
            var car = obj as AutopilotCarViewModel;
            if (car != null)
            {
                foreach (var client in _clients)
                {
                    // mssing readonly check, will skip this as the ACC client has to handle this as well
                    client.SetFocus(Convert.ToUInt16(car.CarIndex), camSet, camera);
                }

                LastFocusChange = DateTime.Now;
                LastCameraChange = DateTime.Now;
            }
        }

        private void RequestFocusedCar(object obj)
        {
            var car = obj as AutopilotCarViewModel;
            if (car != null)
            {
                foreach (var client in _clients)
                {
                    // mssing readonly check, will skip this as the ACC client has to handle this as well
                    client.SetFocus(Convert.ToUInt16(car.CarIndex));
                }

                LastFocusChange = DateTime.Now;
            }
        }

        private void RequestHudPageChange(string requestedHudPage)
        {
            if(requestedHudPage != CurrentHudPage)
            { 
                foreach (var client in _clients)
                {
                    // mssing readonly check, will skip this as the ACC client has to handle this as well
                    client.RequestHUDPage(requestedHudPage);
                }
            }
        }

        private void RequestCameraChange(string camSet, string camera)
        {
            foreach (var client in _clients)
            {
                // mssing readonly check, will skip this as the ACC client has to handle this as well
                client.SetCamera(camSet, camera);
            }

            LastCameraChange = DateTime.Now;
        }

        internal void RegisterNewClient(ACCUdpRemoteClient newClient)
        {
            if (newClient.MsRealtimeUpdateInterval > 0)
            {
                // This client will send realtime updates, we should listen
                newClient.MessageHandler.OnTrackDataUpdate += MessageHandler_OnTrackDataUpdate;
                newClient.MessageHandler.OnEntrylistUpdate += MessageHandler_OnEntrylistUpdate;
                newClient.MessageHandler.OnRealtimeUpdate += MessageHandler_OnRealtimeUpdate;
                newClient.MessageHandler.OnRealtimeCarUpdate += MessageHandler_OnRealtimeCarUpdate;
            }

            _clients.Add(newClient.MessageHandler);
        }

        private void MessageHandler_OnTrackDataUpdate(string sender, TrackData trackUpdate)
        {
            if (TrackVM?.TrackId != trackUpdate.TrackId)
            {
                if (TrackVM != null)
                {
                    TrackVM.OnRequestCameraChange -= RequestCameraChange;
                    TrackVM.OnRequestHudPageChange -= RequestHudPageChange;
                }

                TrackVM = new TrackViewModel(trackUpdate.TrackId, trackUpdate.TrackName, trackUpdate.TrackMeters);
                TrackVM.OnRequestCameraChange += RequestCameraChange;
                TrackVM.OnRequestHudPageChange += RequestHudPageChange;
            }

            // The track cams may update in between
            TrackVM.Update(trackUpdate);
            CamManagerVM.Update(trackUpdate);
        }

        private void MessageHandler_OnEntrylistUpdate(string sender, CarInfo carUpdate)
        {
            AutopilotCarViewModel vm = Cars.SingleOrDefault(x => x.CarIndex == carUpdate.CarIndex);
            if (vm == null)
            {
                vm = new AutopilotCarViewModel(carUpdate.CarIndex);
                Cars.Add(vm);
            }

            vm.Update(carUpdate);
        }

        private void MessageHandler_OnRealtimeUpdate(string sender, RealtimeUpdate update)
        {
            if (TrackVM != null)
                TrackVM.Update(update);

            SessionType = update.SessionType;

            foreach (var carVM in Cars)
            {
                carVM.SetFocused(update.FocusedCarIndex);
            }

            if(FocusedCarIndex != update.FocusedCarIndex)
            { 
                FocusedCarIndex = update.FocusedCarIndex;
                LastFocusChange = DateTime.Now;
            }

            if(ActiveCameraSet != update.ActiveCameraSet)
            {
                ActiveCameraSet = update.ActiveCameraSet;
                LastCameraSetChange = DateTime.Now;
            }

            if(ActiveCamera != update.ActiveCamera || ActiveCameraSet != update.ActiveCameraSet)
            {
                ActiveCamera = update.ActiveCamera;
                LastCameraChange = DateTime.Now;
            }

            CurrentHudPage = update.CurrentHudPage;

            CamManagerVM.RealtimeUpdate(update);
            try
            {
                if (TrackVM?.TrackMeters > 0)
                {
                    var sortedCars = Cars.OrderByDescending(x => x.SplinePosition).ToArray();
                    if(sortedCars.Count() > 1)
                    {
                        for (int i = 1; i < sortedCars.Length; i++)
                        {
                            var carAhead = sortedCars[i - 1];
                            var carBehind = sortedCars[i];

                            var distance = calcMetersDistance(carAhead, carBehind, TrackVM.TrackMeters);
                            carBehind.GapFrontMeters = distance;
                            carAhead.GapRearMeters = distance;

                            var combinedSpeedMS = (carAhead.Kmh + carBehind.Kmh) / 2f / 3.6f;
                            if(combinedSpeedMS > 0.0001f)
                            {
                                carBehind.GapFrontSeconds = distance / combinedSpeedMS;
                                carAhead.GapRearSeconds = distance / combinedSpeedMS;
                            }
                            else
                            {
                                carBehind.GapFrontSeconds = 999;
                                carAhead.GapRearSeconds = 999;
                            }
                        }

                        // then the first and last cars
                        var distance2 = calcMetersDistance(sortedCars.First(), sortedCars.Last(), TrackVM.TrackMeters);
                        sortedCars.First().GapFrontMeters = distance2;
                        sortedCars.Last().GapRearMeters = distance2;
                    }
                    else
                    {
                        foreach (var item in sortedCars)
                        {
                            item.GapFrontMeters = TrackVM.TrackMeters;
                            item.GapRearMeters = TrackVM.TrackMeters;
                        }
                    }
                }

                // A bit unfortunate, the RealtimeUpdate is happening before the carUpdates so we will lag a bit
                // but still it's the one place we know the carupdates are synched and the states are the most comparable
                // more sophisticated solution would be better, like resetting a counter here and push the calculation once we received all cars
                CalcCarRanks();
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }
        }

        private static float calcMetersDistance(AutopilotCarViewModel carAhead, AutopilotCarViewModel carBehind, float trackMeters)
        {
            var splineDistance = carAhead.SplinePosition - carBehind.SplinePosition;
            while (splineDistance < 0f)
                splineDistance += 1f;

            return splineDistance * trackMeters;
        }

        private void MessageHandler_OnRealtimeCarUpdate(string sender, RealtimeCarUpdate carUpdate)
        {
            var vm = Cars.FirstOrDefault(x => x.CarIndex == carUpdate.CarIndex);
            if (vm == null)
            {
                // Oh, we don't have this car yet. In this implementation, the Network protocol will take care of this
                // so hopefully we will display this car in the next cycles
                return;
            }

            vm.Update(carUpdate, TrackVM.TrackMeters);
            if(vm.HasFocus)
                CamManagerVM.RealtimeUpdateFocusedCar(vm.CarIndex, vm.SplinePosition, vm.Kmh);
        }

        private void Log(string msg)
        {
            MiniLog.Insert(0, msg);
            while (MiniLog.Count > 15)
                MiniLog.RemoveAt(MiniLog.Count - 1);
        }
    }
}
