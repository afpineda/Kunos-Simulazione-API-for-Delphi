using System;
using System.Windows.Media;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class CameraViewModel : KSObservableObject
    {
        public string CameraSetName { get; }
        public string CameraName { get; }
        public Brush CameraForeground { get => Get<Brush>(); private set => Set(value); }
        public Brush CameraBackground { get => Get<Brush>(); private set => Set(value); }


        public CameraViewModel(string cameraSetName, string cameraName)
        {
            CameraSetName = cameraSetName;
            CameraName = cameraName;
            SetActive(false, "nope");
        }

        internal void SetActive(bool camSetActive, string activeCamera)
        {
            if (camSetActive && CameraName == activeCamera)
            {
                CameraForeground = Brushes.Black;
                CameraBackground = Brushes.Yellow;
            }
            else
            {
                CameraForeground = Brushes.Black;
                CameraBackground = null;
            }
        }
    }
}