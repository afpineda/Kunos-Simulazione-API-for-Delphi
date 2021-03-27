using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ksBroadcastingNetwork.Structs;

namespace ksBroadcastingTestClient.Autopilot
{
    public enum CameraTypeEnum { Tv1, Tv2, RearWing, Onboard, Helicam, Pitlane, Unknown }

    public interface ICamManager
    {
        float GetTVCamPressure(CameraTypeEnum cameraType, AutopilotCarViewModel car);
        void GetPreferredCameraWithWeight(out AnyCamera preferredCamera, out float rawValue, Dictionary<CameraTypeEnum, float> camPressures, bool isFocusedCar);
    }

    public class AnyCamera : KSObservableObject
    {
        public string Set { get; }
        public string Name { get; }
        public CameraTypeEnum CamType { get; set; }
        public bool IsActive { get => Get<bool>(); set { Set(value); NotifyUpdate(nameof(IndicatorColor)); } }
        public virtual System.Windows.Media.Brush IndicatorColor
        {
            get
            {
                if (IsActive)
                    return System.Windows.Media.Brushes.Green;
                return System.Windows.Media.Brushes.Gray;
            }
        }

        public AnyCamera(string set, string name)
        {
            Set = set;
            Name = name;
        }
    }

    class TVCamJsonData
    {
        public string SetName { get; set; }
        public string CamName { get; set; }
        public float SplinePosStart { get; set; }
        public float SplinePosEnd { get; set; }
        public int EntrySpeed { get; set; }
        public int ExitSpeed { get; set; }

        public static TVCamJsonData FromCam(TVCam cam)
        {
            return new TVCamJsonData()
            {
                SetName = cam.Set,
                CamName = cam.Name,
                SplinePosStart = cam.SplinePosStart,
                SplinePosEnd = cam.SplinePosEnd,
                EntrySpeed = cam.EntrySpeed,
                ExitSpeed = cam.ExitSpeed
            };
        }

        public void UpdateTVCam(TVCam cam)
        {
            if (string.Equals(SetName, cam.Set) && string.Equals(CamName, cam.Name))
            {
                if (cam.SplinePosStart < 0f)
                    cam.SplinePosStart = SplinePosStart;
                if (cam.SplinePosEnd < 0f)
                    cam.SplinePosEnd = SplinePosEnd;
                if (cam.EntrySpeed <= 0f)
                    cam.EntrySpeed = EntrySpeed;
                if (cam.ExitSpeed <= 0f)
                    cam.ExitSpeed = ExitSpeed;
            }
        }
    }

    public class TVCam : AnyCamera
    {
        public float SplinePosStart { get; internal set; } = -1f;
        public float SplinePosEnd { get; internal set; } = -1f;

        public override System.Windows.Media.Brush IndicatorColor
        {
            get
            {
                if (SplinePosStart < 0f)
                    return IsActive ? System.Windows.Media.Brushes.Brown : System.Windows.Media.Brushes.Red;
                if (SplinePosEnd < 0f)
                    return IsActive ? System.Windows.Media.Brushes.RosyBrown : System.Windows.Media.Brushes.Orange;
                return base.IndicatorColor;
            }
        }

        public int EntrySpeed { get; internal set; }
        public int ExitSpeed { get; internal set; }
        public TVCam PreviousCam { get; internal set; }

        public TVCam(string set, string name)
            : base(set, name)
        {
        }

        internal void SetEntry(float splinePosition, int kmh)
        {
            SplinePosStart = splinePosition;
            EntrySpeed = kmh;
            NotifyUpdate(nameof(IndicatorColor));
        }

        internal void SetExit(float splinePosition, int kmh)
        {
            SplinePosEnd = splinePosition;
            ExitSpeed = kmh;
            NotifyUpdate(nameof(IndicatorColor));
        }

        public override string ToString()
        {
            var startIsSet = SplinePosStart < 0f ? "-" : "|";
            var endIsSet = SplinePosStart < 0f ? "|" : "|";
            return $"{startIsSet}{Name}{endIsSet}";
        }
    }

    public class CameraManagementViewModel : KSObservableObject, ICamManager
    {
        public Dictionary<string, List<TVCam>> TVCameraSets { get; private set; }
        public Dictionary<string, List<AnyCamera>> OtherCameraSets { get; private set; }
        public Dictionary<string, List<AnyCamera>> AllCameraSets { get; private set; }

        TVCam OldTVCam = null;
        float LastFocusedCarSplinePosition = -1f;
        int LastFocusedCarSpeed = -1;
        int LastFocusedCarId = -1;
        private AnyCamera CurrentCam;

        public DateTime CurrentCamSetActiveSince { get; private set; }

        private Random R = new Random();

        private Dictionary<CameraTypeEnum, DateTime> CamTypeLastActive { get; } = new Dictionary<CameraTypeEnum, DateTime>();

        public float TVCamLearningProgress { get => Get<float>(); private set { if (Set(value)) NotifyUpdate(nameof(CameraState)); } }
        public string CameraState
        {
            get
            {
                if (TVCameraSets == null || TVCameraSets.Any())
                    return $"Waiting for camera definitions";
                else if (TVCamLearningProgress < 1f)
                    return $"Learning TV cams {TVCamLearningProgress:P0}";
                else
                {
                    return $"Current camera xy";
                }
            }
        }

        public string TrackName { get; private set; }
        public float TrackMeters { get; private set; }

        internal void Update(TrackData trackUpdate)
        {
            TrackName = trackUpdate.TrackName;
            TrackMeters = trackUpdate.TrackMeters;
            if (TVCameraSets == null)
            {
                OldTVCam = null;
                TVCameraSets = new Dictionary<string, List<TVCam>>();
                OtherCameraSets = new Dictionary<string, List<AnyCamera>>();
                AllCameraSets = new Dictionary<string, List<AnyCamera>>();

                foreach (var tvCamSet in trackUpdate.CameraSets.Where(x => x.Key.StartsWith("set")))
                {
                    // we'll exclude the VR sets, usually can't use them but confuses the learning of TV cams
                    if (tvCamSet.Key.EndsWith("vr", StringComparison.InvariantCultureIgnoreCase))
                        continue;

                    TVCameraSets.Add(tvCamSet.Key, new List<TVCam>());
                    AllCameraSets.Add(tvCamSet.Key, new List<AnyCamera>());

                    var lastTvCam = (TVCam)null;
                    foreach (var tvCamName in tvCamSet.Value)
                    {
                        var cam = new TVCam(tvCamSet.Key, tvCamName);
                        if (lastTvCam != null)
                            cam.PreviousCam = lastTvCam;
                        lastTvCam = cam;
                        TVCameraSets[tvCamSet.Key].Add(cam);
                        AllCameraSets[tvCamSet.Key].Add(cam);
                    }

                    if (TVCameraSets[tvCamSet.Key].Count > 1)
                        TVCameraSets[tvCamSet.Key].First().PreviousCam = TVCameraSets[tvCamSet.Key].Last();
                }

                foreach (var camSet in trackUpdate.CameraSets.Where(x => !x.Key.StartsWith("set")))
                {
                    OtherCameraSets.Add(camSet.Key, new List<AnyCamera>());
                    AllCameraSets.Add(camSet.Key, new List<AnyCamera>());

                    foreach (var otherCamName in camSet.Value)
                    {
                        var cam = new AnyCamera(camSet.Key, otherCamName);
                        OtherCameraSets[camSet.Key].Add(cam);
                        AllCameraSets[camSet.Key].Add(cam);
                    }
                }

                foreach (var camera in AllCameraSets.SelectMany(x => x.Value))
                {
                    camera.CamType = EvaluateCameraType(trackUpdate.TrackName, camera.Set, camera.Name);
                }

                foreach (var camType in AllCameraSets.SelectMany(x => x.Value).Select(x => x.CamType).Distinct())
                {
                    CamTypeLastActive.Add(camType, DateTime.Now.AddMinutes(-10));
                }

                TryLoadTVCameraDefs(TVCameraSets.SelectMany(x => x.Value), TrackName);
                UpdateTVCamLearningProgress();

                NotifyUpdate(nameof(AllCameraSets));
                NotifyUpdate(nameof(CameraState));
            }
        }

        private CameraTypeEnum EvaluateCameraType(string trackName, string set, string name)
        {
            // This should go to a cfg file or so
            if (set.Contains("set1"))
                return CameraTypeEnum.Tv1;
            if (set.Contains("set2"))
                return CameraTypeEnum.Tv2;
            if (set.Contains("heli") || set.Contains("Heli"))
                return CameraTypeEnum.Helicam;
            if (set == "pitlane")
                return CameraTypeEnum.Pitlane;

            // the rest should be some kind of onboard, we only look for the rear wing one precisely
            if (name == "Onboard3")
                return CameraTypeEnum.RearWing;

            // aeh nobody wants to see chasecams in br
            if (name.Contains("chase") || name.Contains("chase"))
                return CameraTypeEnum.Unknown;

            return CameraTypeEnum.Onboard;
        }

        internal void RealtimeUpdate(RealtimeUpdate update)
        {
            try
            {
                if (AllCameraSets.ContainsKey(update.ActiveCameraSet))
                {
                    var cam = AllCameraSets[update.ActiveCameraSet].Single(x => x.Name == update.ActiveCamera);
                    if (!cam.IsActive)
                    {
                        // Cam changed!
                        var lastCams = AllCameraSets.SelectMany(x => x.Value).Where(x => x.IsActive);
                        if (lastCams.Count() == 1)
                        {
                            // regular case, we have a last and new cam
                            var oldCam = lastCams.Single();

                            // To learn the stuff, we need to memorize the old cam
                            OldTVCam = (oldCam as TVCam);
                        }
                        else if (lastCams.Count() > 1)
                            Debug.WriteLine($"There are {lastCams.Count()} active cams, something went horribly wrong");

                        foreach (var item in lastCams)
                            item.IsActive = false;

                        cam.IsActive = true;
                        if (cam.CamType != CurrentCam?.CamType)
                            CurrentCamSetActiveSince = DateTime.Now;
                        CurrentCam = cam;
                    }
                }

                if (CurrentCam != null)
                    CamTypeLastActive[CurrentCam.CamType] = DateTime.Now;
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }
        }

        internal TVCam GetForcedCameraSet()
        {
            if (TVCamLearningProgress < 1f)
            {
                foreach (var item in TVCameraSets.SelectMany(x => x.Value))
                {
                    if (item.SplinePosEnd < 0f || item.SplinePosStart < 0f)
                        if (item.Set == CurrentCam?.Set)
                            return null;
                        else
                            return item;
                }
            }

            return null;
        }

        internal AnyCamera GetTVCamByIndex(int camIndex)
        {
            foreach (var set in TVCameraSets?.Skip(camIndex))
                return set.Value.FirstOrDefault();
            return null;
        }

        internal void RealtimeUpdateFocusedCar(int focusedCarId, float splinePosition, int kmh)
        {
            if (OldTVCam != null && !OldTVCam.IsActive && TVCamLearningProgress < 1f && LastFocusedCarId == focusedCarId)
            {
                var currentTVCam = CurrentCam as TVCam;
                // Looks like we can update the end of the old cam
                // BUT we need to make sure it's the one before the current cam
                if (currentTVCam != null)
                {
                    if (currentTVCam.PreviousCam == OldTVCam)
                    {
                        // Cool, now we can learn where the new one begins
                        if (currentTVCam.SplinePosStart < 0f)
                            currentTVCam.SetEntry(splinePosition, kmh);

                        // Additionally, the old cam may learn the exit
                        if (OldTVCam.SplinePosEnd < 0f)
                            OldTVCam.SetExit(LastFocusedCarSplinePosition, LastFocusedCarSpeed);

                        var oldProgress = TVCamLearningProgress;
                        UpdateTVCamLearningProgress();
                        if (oldProgress != TVCamLearningProgress && TVCamLearningProgress == 1f)
                            SaveTVCameraDefs(TVCameraSets.SelectMany(x => x.Value), TrackName);
                    }
                }
            }

            LastFocusedCarSplinePosition = splinePosition;
            LastFocusedCarSpeed = kmh;
            LastFocusedCarId = focusedCarId;
        }

        private void UpdateTVCamLearningProgress()
        {
            // Now we may have an update to the LearningProcess
            int tvCams = 0;
            int finalizedTvCams = 0;
            foreach (var item in TVCameraSets.SelectMany(x => x.Value))
            {
                if (item.SplinePosEnd > -1f && item.SplinePosStart > -1f)
                {
                    // ready to go
                    finalizedTvCams++;
                }
                tvCams++;
            }
            if (tvCams > 0)
                TVCamLearningProgress = finalizedTvCams / (float)tvCams;
            else
                TVCamLearningProgress = 0;
        }

        public float GetTVCamPressure(CameraTypeEnum cameraType, AutopilotCarViewModel car)
        {
            if (TVCameraSets == null)
            {
                Debug.WriteLine($"There are no TV cams (yet?)");
                return 1f;
            }

            if (TVCamLearningProgress < 1f)
            {
                // while we learn, it's better to stick to a car and let us see all the cams as quick as possible
                return 0.1f;
            }

            var setName = TVCameraSets.SelectMany(x => x.Value).FirstOrDefault(x => x.CamType == cameraType)?.Set;

            // Complicated stuff. So the big targets are
            // 1) avoid changing focus to a car inside the same TV cam, that looks just weird
            // 2) avoid changing focus to a car that is at the end of the given camera zone
            // Third is avoid changing focus from a car that just entered a new cam, but this is handled in the car view model

            // let's find the camera this car would be in
            var potentialTVCam = TVCameraSets[setName].FirstOrDefault(x => x.SplinePosStart < car.SplinePosition && x.SplinePosEnd > car.SplinePosition);
            if (potentialTVCam == null)
            {
                // mh what's this?
                return 0.1f;
            }

            var timeToEnd = (potentialTVCam.SplinePosEnd - car.SplinePosition) * TrackMeters / (car.Kmh / 3.6f);
            // then the pressure is a function of "at least 2s", then we'll blend into full green light at 8s
            var pressure = Math.Min(Math.Max(timeToEnd - 2, 0), 6) / 6f;

            // Now 1): is this cars not focusing, but we would use the same cam when focused?
            if (CurrentCam == potentialTVCam)
                // We are the focused car, always - even if this is called during a pre-step
                return Math.Max(0.8f, pressure);

            // 2) How much time will we have until this camera ends?
            if (car.Kmh < 20)
                return 0.1f;


            // TODO: we could edit the pressure to have a healthy ratio between the cam sets, otherwise it'd be random 
            // and set2 may win over proportionally often due to the longer scenes

            return pressure;
        }

        public void GetPreferredCameraWithWeight(out AnyCamera preferredCamera, out float rawValue, Dictionary<CameraTypeEnum, float> camPressures, bool isFocusedCar)
        {
            // First we'll edit the camPressures based on settings, recent cameras and strict rules - the car didn't know shit about cams after all
            foreach (var item in camPressures)
            {
                // apply modifiers
            }

            // now strict rules
            var camSetActiveSinceMinutes = Convert.ToSingle(Math.Max((DateTime.Now - CurrentCamSetActiveSince).TotalMinutes, 0.5) - 0.5);
            var lastCamSetSwitchSeconds = Convert.ToSingle((DateTime.Now - CurrentCamSetActiveSince).TotalSeconds);
            foreach (var camType in camPressures.Keys.ToArray())
            {
                var isCurrentCam = camType == CurrentCam?.CamType;
                var thisCamLastActiveMinutes = (DateTime.Now - CamTypeLastActive[camType]).TotalMinutes;
                if (thisCamLastActiveMinutes < 5.0 && !isCurrentCam)
                    camPressures[camType] *= Math.Min(Convert.ToSingle(thisCamLastActiveMinutes / 5.0), 1f);

                switch (camType)
                {
                    case CameraTypeEnum.RearWing:
                    case CameraTypeEnum.Onboard:
                        {
                            // Additionally these are nice, but shouldn't be in too often, so we'll doubledip this calculation
                            if (thisCamLastActiveMinutes < 5.0 && !isCurrentCam)
                            {
                                camPressures[camType] *= Convert.ToSingle(thisCamLastActiveMinutes / 5.0);
                            }

                        }
                        break;
                    default:
                        break;
                }

                if (isCurrentCam && isFocusedCar)
                {
                    var camIsGettingOldFactor = 1f;
                    if (camType == CameraTypeEnum.Tv1 || camType == CameraTypeEnum.Tv2)
                        camIsGettingOldFactor = 1f - Math.Min(Math.Max(camSetActiveSinceMinutes - 0.5f, 0f) / 3f, 1f);
                    else
                        camIsGettingOldFactor = 1f - Math.Min(Math.Max(camSetActiveSinceMinutes - 0.3f, 0f) / 2f, 2f);

                    camPressures[camType] *= camIsGettingOldFactor;
                }

                if (!isCurrentCam && isFocusedCar)
                {
                    // on the other hand we don't want to switch too quickly;
                    if (lastCamSetSwitchSeconds < 20)
                    {
                        var camIsYoungFactor = 1f;
                        if (CurrentCam?.CamType == CameraTypeEnum.Tv1 || CurrentCam?.CamType == CameraTypeEnum.Tv2)
                            camIsYoungFactor = Math.Min(Math.Max(lastCamSetSwitchSeconds - 10f, 0f) / 30f, 1f);
                        else
                            camIsYoungFactor = Math.Min(Math.Max(lastCamSetSwitchSeconds - 5f, 0f) / 15f, 1f);
                        camPressures[camType] = Math.Min(camIsYoungFactor + camPressures[camType], 1f);
                    }
                }

                // In any way we do not want to focus a new car in the same camera, except for TV cams - but here we don't want to jump
                // into the same or one we recently saw
                if (!isFocusedCar)
                {
                    if (camType == CurrentCam?.CamType)
                        camPressures[camType] = 0f;
                }
            }

            var winner = camPressures.OrderByDescending(x => x.Value).First();

            // now we'll locate the camera for that, if it's not the same
            if (winner.Key == CurrentCam?.CamType || winner.Value < 0.1)
                preferredCamera = null; // null means stick witch it
            else
            {
                // otherwise, we'll select a random cam that matches the type
                var candidates = AllCameraSets.SelectMany(x => x.Value).Where(x => x.CamType == winner.Key).ToArray();
                if (!candidates.Any())
                {
                    Debug.WriteLine($"Requested cam type {winner.Key} isn't available");
                    preferredCamera = null;
                }
                else
                {
                    preferredCamera = candidates[R.Next(candidates.Length)];
                }
            }

            rawValue = winner.Value;
        }

        private static void SaveTVCameraDefs(IEnumerable<TVCam> tVCameraSets, string track)
        {
            var defs = tVCameraSets.Select(x => TVCamJsonData.FromCam(x));

            var json = Newtonsoft.Json.JsonConvert.SerializeObject(defs);
            if (!System.IO.Directory.Exists("camDefs"))
                System.IO.Directory.CreateDirectory("camDefs");
            System.IO.File.WriteAllText($"camDefs/{track}.json", json);
        }

        private static void TryLoadTVCameraDefs(IEnumerable<TVCam> tVCameraSets, string track)
        {
            if (!System.IO.File.Exists($"camDefs/{track}.json"))
                return;

            var json = System.IO.File.ReadAllText($"camDefs/{track}.json");
            try
            {
                var camDefs = Newtonsoft.Json.JsonConvert.DeserializeObject<IEnumerable<TVCamJsonData>>(json);
                foreach (var item in tVCameraSets)
                {
                    var camDef = camDefs.FirstOrDefault(x => string.Equals(x.SetName, item.Set) && string.Equals(x.CamName, item.Name));
                    if (camDef != null)
                        camDef.UpdateTVCam(item);
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex.Message);
            }
        }
    }
}
