using ksBroadcastingNetwork;
using ksBroadcastingNetwork.Structs;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;

namespace ksBroadcastingTestClient.Autopilot
{
    public class AutopilotCarViewModel : KSObservableObject
    {
        public float Pressure { get => Get<float>(); set { if (Set(value)) NotifyUpdate(nameof(PressureWidth)); } }
        public GridLength PressureWidth => new GridLength(Pressure, GridUnitType.Star);
        public GridLength PressureWidthRight => new GridLength(1f - Pressure / 2f, GridUnitType.Star);
        public int CarIndex { get; }
        public int RaceNumber { get => Get<int>(); private set => Set(value); }
        public int CarModelEnum { get => Get<int>(); private set => Set(value); }
        public string TeamName { get => Get<string>(); private set => Set(value); }
        public int CupCategoryEnum { get => Get<int>(); private set => Set(value); }
        public CarLocationEnum CarLocation { get => Get<CarLocationEnum>(); private set => Set(value); }
        public bool CrossedTheLineWithFocus { get; private set; }
        public int Delta { get => Get<int>(); private set => Set(value); }
        public int Gear { get => Get<int>(); private set => Set(value); }
        public int Kmh { get => Get<int>(); private set => Set(value); }
        public int Position { get => Get<int>(); private set => Set(value); }
        public int CupPosition { get => Get<int>(); private set => Set(value); }
        public int TrackPosition { get => Get<int>(); private set => Set(value); }
        public float SplinePosition { get => Get<float>(); private set => Set(value); }
        public float WorldX { get => Get<float>(); private set => Set(value); }
        public float WorldY { get => Get<float>(); private set => Set(value); }
        public float Yaw { get => Get<float>(); private set => Set(value); }
        public int Laps { get => Get<int>(); private set => Set(value); }
        public string LocationHint { get => Get<string>(); private set => Set(value); }
        public float GapFrontMeters { get => Get<float>(); set => Set(value); }
        public float GapRearMeters { get => Get<float>(); set => Set(value); }
        public float GapFrontSeconds { get => Get<float>(); set => Set(value); }
        public float GapRearSeconds { get => Get<float>(); set => Set(value); }
        public string CurrentDriverName { get => Get<string>(); private set => Set(value); }
        public bool HasFocus { get; internal set; }
        public List<CarWeightCategoryViewModel> PressureCategories { get; } = new List<CarWeightCategoryViewModel>();
        public int SessionPersonalBestLap { get => Get<int>(); private set => Set(value); }
        public int PredictedLaptime { get => Get<int>(); private set => Set(value); }
        public static int SessionBestLap { get; set; }
        public int CarsAroundMe30m { get; private set; }

        const float OFFSET = 0.001f;

        public AutopilotCarViewModel(ushort carIndex)
        {
            CarIndex = carIndex;

            foreach (var item in Enum.GetValues(typeof(CarWeightCategoryEnum)).Cast<CarWeightCategoryEnum>())
            {
                PressureCategories.Add(new CarWeightCategoryViewModel(item));
            }
        }

        internal void SetFocused(int focusedCarIndex)
        {
            if (CarIndex == focusedCarIndex)
            {
                HasFocus = true;
                //                 RowForeground = Brushes.Yellow;
                //                 RowBackground = Brushes.Black;
            }
            else
            {
                HasFocus = false;
                //                 RowForeground = Brushes.Black;
                //                 RowBackground = null;
            }
        }

        internal void Update(CarInfo carUpdate)
        {
            RaceNumber = carUpdate.RaceNumber;
            CarModelEnum = carUpdate.CarModelType;
            TeamName = carUpdate.TeamName;
            CupCategoryEnum = carUpdate.CupCategory;

            CurrentDriverName = carUpdate.GetCurrentDriverName();
        }

        internal void Update(RealtimeCarUpdate carUpdate, float trackMeters)
        {
            if (carUpdate.CarIndex != CarIndex)
            {
                System.Diagnostics.Debug.WriteLine($"Wrong {nameof(RealtimeCarUpdate)}.CarIndex {carUpdate.CarIndex} for {nameof(AutopilotCarViewModel)}.CarIndex {CarIndex}");
                return;
            }

            CarLocation = carUpdate.CarLocation;
            CrossedTheLineWithFocus = false;
            if (carUpdate.SplinePosition * trackMeters < 100 && HasFocus)
                CrossedTheLineWithFocus = true;
            if (CrossedTheLineWithFocus && carUpdate.SplinePosition * trackMeters > 100)
                CrossedTheLineWithFocus = false;

            if (carUpdate.SplinePosition * trackMeters > 500)
                Delta = carUpdate.Delta;

            Kmh = carUpdate.Kmh;
            Position = carUpdate.Position;
            CupPosition = carUpdate.CupPosition;
            TrackPosition = carUpdate.TrackPosition;
            SplinePosition = carUpdate.SplinePosition;
            WorldX = carUpdate.WorldPosX;
            WorldY = carUpdate.WorldPosY;
            Yaw = carUpdate.Yaw;
            Laps = carUpdate.Laps;

            SessionPersonalBestLap = carUpdate.BestSessionLap.LaptimeMS ?? -1;
            if (SessionPersonalBestLap > 0 && (SessionPersonalBestLap < SessionBestLap || SessionBestLap <= 0))
                SessionBestLap = SessionPersonalBestLap;

            if (SessionBestLap > 0)
                PredictedLaptime = SessionBestLap + Delta;


            // The location hint will combine stuff like pits, in/outlap
            if (CarLocation == CarLocationEnum.PitEntry)
                LocationHint = "IN";
            else if (CarLocation == CarLocationEnum.Pitlane)
                LocationHint = "PIT";
            else if (CarLocation == CarLocationEnum.PitExit)
                LocationHint = "OUT";
            else
                LocationHint = "OUT";
        }

        internal void CalcPressure(IList<AutopilotCarViewModel> trackPositionCarList, float trackMeters, float currentFocusSeconds, IDictionary<CarWeightCategoryEnum, float> categoryWeights, ICamManager camManager)
        {
            var OFFSET = 0.001f; // we'll use a (tiny) offset so even a zero doesn't eliminate our information by a zero division; 
            var ONE = 1f + OFFSET;
            // this way even a multiple "bad, bad, very bad" combination will be greater than "bad, very bad, very bad"

            Pressure = 1f;
            foreach (var category in PressureCategories)
            {
                // Special case: Sitting in the pits or so
                if (CarLocation == CarLocationEnum.Pitlane)
                {
                    category.RawValue = OFFSET;
                    category.Hint = $"Pitlane";
                }
                else
                {
                    switch (category.Category)
                    {
                        case CarWeightCategoryEnum.Proximity:
                            {
                                // 1 Proximity - how close is this car to the next one, linearly scaled to 2.5 seconds
                                // Now this is tricky for rear/front, as there are always two cars for the same distance. If we look at the front
                                // or rear car has major impact on the pressure of rearwing or onboard cams, so we'll need a bit of variance here
                                float frontBias = 1f;
                                float rearBias = 0.95f;
                                if (DateTime.Now.Minute % 2 == 0)
                                {
                                    frontBias = 0.95f;
                                    rearBias = 1f;
                                }

                                var closestDistance = Math.Min(GapFrontSeconds * frontBias, GapRearSeconds * rearBias);
                                category.RawValue = ONE - Math.Min(closestDistance, 2.5f) / 2.5f;
                                category.Hint = $"{closestDistance:F1}";
                            }
                            break;
                        case CarWeightCategoryEnum.Pack:
                            {
                                // 2 PackFactor - how many cars are within ~10 and 30 meters? Linearly scaled to 5 cars
                                CarsAroundMe30m = trackPositionCarList.Where(x => Math.Abs(x.SplinePosition - SplinePosition) * trackMeters < 30 && x != this).Count();
                                var carsAroundMe10m = trackPositionCarList.Where(x => Math.Abs(x.SplinePosition - SplinePosition) * trackMeters < 10 && x != this).Count();

                                // 10m counts twice
                                category.RawValue = Math.Min(CarsAroundMe30m + carsAroundMe10m, 7) / 7f + OFFSET;
                                category.Hint = $"{CarsAroundMe30m}|{carsAroundMe10m}";

                            }
                            break;
                        case CarWeightCategoryEnum.Position:
                            {
                                // 3 Race Position - leaders may be more interesting than anybody else
                                // we have 2 positions; official and track - let's combine them
                                var pos = (Position + TrackPosition) / 2f;
                                category.RawValue = ONE - pos / (float)trackPositionCarList.Count;
                                if (Position != TrackPosition)
                                    category.Hint = $"{Position}|{TrackPosition}";
                                else
                                    category.Hint = $"{TrackPosition}";
                            }
                            break;
                        case CarWeightCategoryEnum.FocusFast:
                            {
                                // 10 Focus seconds - how long since the last focus switch? This is the short time thing which allows (or denies) to quickly jump
                                // action if there is something happening like a contact, closing in or whatever
                                // First 5s are super critical, we won't really allow a jump there so we scale to 10f - then it's neutral to jump over
                                if (!HasFocus)
                                {
                                    category.RawValue = Math.Min(currentFocusSeconds, 10f) / 10f;
                                    category.Hint = $"{currentFocusSeconds:F1}s";
                                }
                                else
                                {
                                    // While we have focus, we signal that's it's uncritical to stick with us
                                    category.RawValue = 1f;
                                    category.Hint = $"Focus";
                                }
                            }
                            break;
                        case CarWeightCategoryEnum.FocusSlow:
                            {
                                // Opposed to the "Fast" variant, this is about how long we want to generally stick to a car.
                                // To not confuse the user, we basically want to aim for at least 30 seconds = 50%
                                if (!HasFocus)
                                {
                                    category.RawValue = Math.Min(currentFocusSeconds, 60f) / 60f;
                                    category.Hint = $"{currentFocusSeconds:F1}s";
                                }
                                else
                                {
                                    // While we have focus, we signal that's it's uncritical to stick with us.
                                    // though we could gradually lower the value when it's becoming way too long
                                    if (currentFocusSeconds > 5 * 60)
                                    {
                                        // we'll sloooowly reduce this so other cars may get into the focus. After a total of 15minutes the focus will forcibly go away (but most likely earlier)
                                        category.RawValue = ONE - Math.Min((Math.Max(currentFocusSeconds - 5f, 0f) * 60f) / 10f * 60f, 0f);
                                        category.Hint = $"Focus ({(currentFocusSeconds / 60f):F1}min)";
                                    }
                                    else
                                    {
                                        category.RawValue = 1f;
                                        category.Hint = $"Focus";
                                    }
                                }
                            }
                            break;
                        case CarWeightCategoryEnum.Pace:
                            {
                                // For pace, we either look at the delta (to express how much we're pushing and maybe hunting) or at the predicted laptime - which is gold for the P/Q modes
                                // this way we should be able to focus cars that may end up on pole or high positions while they do so - something quite hard to achieve manually
                                if (PredictedLaptime <= 0)
                                {
                                    // No lap set yet, doesn't matter
                                    category.RawValue = OFFSET;
                                    category.Hint = $"No laptime";
                                }
                                else
                                {
                                    var splinePosFactor = SplinePosition;
                                    if (CrossedTheLineWithFocus)
                                        splinePosFactor = 1f;
                                    splinePosFactor = (splinePosFactor + 1f) / 2f;
                                    if (PredictedLaptime < SessionBestLap)
                                    {
                                        // oh on the way to purple!
                                        category.RawValue = 1f * splinePosFactor;
                                        category.Hint = $"{Delta:N0} (!)";
                                    }
                                    else
                                    {
                                        category.RawValue = (1f - Math.Min(PredictedLaptime - SessionBestLap, 1500) / 1500) * splinePosFactor;
                                        category.Hint = $"{Delta:N0}";
                                    }
                                }

                            }
                            break;
                        // Add handling for new enum types here
                        default:
                            {
                                // Using a 1f default for unhandled categories will make them do (and break) nothing
                                category.RawValue = 1f;
                                category.Hint = $"Unmapped";
                            }
                            break;
                    }
                }

                category.WeightedValue = (float)Math.Pow(category.RawValue, categoryWeights[category.Category]);
                Pressure *= category.WeightedValue;
            }
        }

        public void CalcPreferredCamera(IList<AutopilotCarViewModel> trackPositionCarList, float trackMeters, float cameraChangedBeforeSeconds, ICamManager camManager, out AnyCamera preferredCamera, out float rawWeight)
        {
            // This is going to be a bigger one. We will select the most interesting/suitable camera for this car
            var tvCam1Pressure = camManager.GetTVCamPressure(CameraTypeEnum.Tv1, this);
            var tvCam2Pressure = camManager.GetTVCamPressure(CameraTypeEnum.Tv2, this);

            // Additionally, we will look for cool other cams, like 
            // a) If there is nothing in front, but lots of action in the rear, we like the rear view cam
            var carsBehindMe = trackPositionCarList.Where(x => Math.Abs(x.SplinePosition - SplinePosition) * trackMeters < 50 && x != this).Count();
            var rearViewPressure = 0f;
            if (GapFrontSeconds > 1 && GapRearSeconds < 2.0f && GapRearMeters > 4)
                rearViewPressure = 1f - Math.Min(Math.Max(GapRearSeconds - 1f, 0f), 1.0f) / 1.0f;

            // b) reversed, if there is action in front of me we can do an onboard
            var onboardPressure = 0f;
            if (GapFrontSeconds < 0.5 && GapRearSeconds > 2f && GapFrontMeters > 4f)
                onboardPressure = 1f - Math.Min(Math.Max(GapFrontSeconds - 1f, 0f), 1.5f) / 1.5f;
            var pitlaneFactor = CarLocation == CarLocationEnum.Pitlane ? 0f : 1f;
            var camPressures = new Dictionary<CameraTypeEnum, float>()
                                {
                                    { CameraTypeEnum.Tv1, tvCam1Pressure * pitlaneFactor },
                                    { CameraTypeEnum.Tv2, tvCam2Pressure * pitlaneFactor },
                                    { CameraTypeEnum.RearWing, rearViewPressure * pitlaneFactor },
                                    { CameraTypeEnum.Onboard, onboardPressure * pitlaneFactor},
                                    { CameraTypeEnum.Pitlane, -pitlaneFactor },
                                    { CameraTypeEnum.Helicam, 0.8f * pitlaneFactor },
                                };

            // Any of the weights will go down by the time since the last camera switch, which makes hopping unlikely
            var camClippingPrevention = Math.Min(Math.Max(cameraChangedBeforeSeconds - 3f, 0f), 7f) / 7f + OFFSET;

            foreach (var camType in camPressures.Keys.ToArray())
            {
                switch (camType)
                {
                    // Additionally we have some edits to do
                    case CameraTypeEnum.Tv1:
                        // TV1 is suitable for close combat, but we shouldn't use it too much for packs
                        if (CarsAroundMe30m > 1)
                            camPressures[camType] *= 1f - Math.Min(CarsAroundMe30m - 1, 5) / 5f;
                        break;
                    case CameraTypeEnum.Tv2:
                        // TV2 is always nice, but shouldn't be overused for low car numbers
                        if (CarsAroundMe30m == 0)
                            camPressures[camType] *= 0.5f;
                        if (CarsAroundMe30m == 1)
                            camPressures[camType] *= 0.75f;
                        break;
                    case CameraTypeEnum.Helicam:
                        // helicam is great to catch many cars, and we also like that this will push them automatically for the start
                        if (CarsAroundMe30m > 2)
                            camPressures[camType] = Math.Min(CarsAroundMe30m, 5) / 5f;
                        else
                            // Otherwise this isn't too fancy and should be reserved to replays
                            camPressures[camType] *= 0.002f;
                        break;
                    case CameraTypeEnum.Onboard:
                        // we need to have a fallback if TV cams suck, otherwise the Helicam is too much of an option most of the time
                        camPressures[camType] = Math.Max(camPressures[camType], 0.21f);
                        break;
                }

                // Clipping prevention: we don't like jumping too quickly
                if (camClippingPrevention < 1f)
                    camPressures[camType] *= camClippingPrevention;
            }

            camManager.GetPreferredCameraWithWeight(out preferredCamera, out rawWeight, camPressures, HasFocus);
        }
    }
}
