using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ksBroadcastingNetwork.Structs;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class TrackViewModel : KSObservableObject
    {
        public int TrackId { get; }
        public float TrackMeters { get; }
        public string TrackName { get; }

        public ObservableCollection<CameraSetViewModel> CameraSets { get; } = new ObservableCollection<CameraSetViewModel>();
        public KSRelayCommand RequestCameraSwitchCommand { get; }
        public KSRelayCommand RequestHudSwitchCommand { get;}

        public ObservableCollection<HudPageViewModel> HudPages { get; } = new ObservableCollection<HudPageViewModel>();

        public delegate void RequestCameraChangeDelegate(string camSet, string camera);
        public event RequestCameraChangeDelegate OnRequestCameraChange;

        public delegate void RequestHudChangeDelegate(string requestedHudPage);
        public event RequestHudChangeDelegate OnRequestHudPageChange;

        public TrackViewModel(int trackId, string trackName, float trackMeters)
        {
            TrackId = trackId;
            TrackName = trackName;
            TrackMeters = trackMeters;

            RequestCameraSwitchCommand = new KSRelayCommand(RequestCameraSwitch);
            RequestHudSwitchCommand = new KSRelayCommand(RequestHudPageSwitch);
        }

        private void RequestHudPageSwitch(object obj)
        {
            var hudPageVM = obj as HudPageViewModel;
            if (hudPageVM != null)
                OnRequestHudPageChange?.Invoke(hudPageVM.Caption);
        }

        private void RequestCameraSwitch(object obj)
        {
            var cameraVM = obj as CameraViewModel;
            if(cameraVM != null)
                OnRequestCameraChange?.Invoke(cameraVM.CameraSetName, cameraVM.CameraName);
        }

        internal void Update(TrackData trackUpdate)
        {
            foreach (var camSetUpdate in trackUpdate.CameraSets)
            {
                var camSetVM = CameraSets.SingleOrDefault(x => x.CameraSetName == camSetUpdate.Key);
                if(camSetVM == null)
                { 
                    camSetVM = new CameraSetViewModel(camSetUpdate.Key);
                    CameraSets.Add(camSetVM);
                }

                camSetVM.Update(camSetUpdate.Value);
            }

            // Now we check if we have to remove one of our cam sets
            var toRemove = new List<CameraSetViewModel>();
            foreach (var camSetVM in CameraSets)
            {
                if (!trackUpdate.CameraSets.ContainsKey(camSetVM.CameraSetName))
                    toRemove.Add(camSetVM);
            }

            foreach (var item in toRemove)
                CameraSets.Remove(item);


            // The same (but 1d) for the HUD pages
            foreach (var hudPageCaption in trackUpdate.HUDPages)
            {
                var hudPageVM = HudPages.SingleOrDefault(x => x.Caption == hudPageCaption);
                if(hudPageVM == null)
                {
                    hudPageVM = new HudPageViewModel(hudPageCaption);
                    HudPages.Add(hudPageVM);
                }
            }
        }

        internal void Update(RealtimeUpdate update)
        {
            foreach (var camSet in CameraSets)
                camSet.SetActive(update.ActiveCameraSet, update.ActiveCamera);

            foreach (var hudPage in HudPages)
            {
                hudPage.SetActive(update.CurrentHudPage);
            }
        }
    }
}
