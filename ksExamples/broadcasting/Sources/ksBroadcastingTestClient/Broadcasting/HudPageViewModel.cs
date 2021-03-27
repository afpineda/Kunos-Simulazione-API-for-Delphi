using System.Windows.Media;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class HudPageViewModel : KSObservableObject
    {
        public string Caption { get; }
        public Brush HudPageForeground { get => Get<Brush>(); private set => Set(value); }
        public Brush HudPageBackground { get => Get<Brush>(); private set => Set(value); }

        public HudPageViewModel(string hudPageCaption)
        {
            Caption = hudPageCaption;
        }

        public void SetActive(string activeHudPage)
        {
            if(Caption == activeHudPage)
            {
                HudPageForeground = Brushes.Black;
                HudPageBackground = Brushes.Yellow;
            }
            else
            {
                HudPageForeground = Brushes.Black;
                HudPageBackground = null;
            }
        }
    }
}