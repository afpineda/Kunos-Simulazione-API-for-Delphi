using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class CameraSetViewModel : KSObservableObject
    {
        public string CameraSetName { get; }
        public ObservableCollection<CameraViewModel> Cameras { get; } = new ObservableCollection<CameraViewModel>();

        public Brush CamsetForeground { get => Get<Brush>(); private set => Set(value); }
        public Brush CamsetBackground { get => Get<Brush>(); private set => Set(value); }

        public CameraSetViewModel(string camSetName)
        {
            CameraSetName = camSetName;
            SetActive("nope", "nope");
        }

        public void SetActive(string camSet, string camera)
        {
            var isActive = CameraSetName == camSet;
            if(isActive)
            {
                CamsetForeground = Brushes.Yellow;
                CamsetBackground = Brushes.Black;
            }
            else
            {
                CamsetForeground = Brushes.Black;
                CamsetBackground = null;
            }

            foreach (var item in Cameras)
                item.SetActive(isActive, camera);
        }

        internal void Update(List<string> cameraNames)
        {
            foreach (var cameraUpdateName in cameraNames)
            {
                var camVM = Cameras.SingleOrDefault(x => x.CameraName == cameraUpdateName);
                if (camVM == null)
                { 
                    camVM = new CameraViewModel(CameraSetName, cameraUpdateName);
                    Cameras.Add(camVM);
                }
            }

            // Now we check if we have to remove one of our cameras
            var toRemove = new List<CameraViewModel>();
            foreach (var camVM in Cameras)
            {
                if (!cameraNames.Contains(camVM.CameraName))
                    toRemove.Add(camVM);
            }

            foreach (var item in toRemove)
                Cameras.Remove(item);
        }
    }
}
