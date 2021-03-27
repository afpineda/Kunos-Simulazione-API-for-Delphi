using ksBroadcastingNetwork.Structs;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class BroadcastingEventViewModel : KSObservableObject
    {
        public BroadcastingEvent Evt { get; }
        public KSRelayCommand PlayReplayCommand { get; }

        public BroadcastingEventViewModel(BroadcastingEvent evt, Action<BroadcastingEvent, float, float> onHighlightReplayDelegate)
        {
            Evt = evt;
            PlayReplayCommand = new KSRelayCommand((o) =>
            {
                float secondsBack = 5f;
                float duration = 10f;
                switch (Evt.Type)
                {
                    case ksBroadcastingNetwork.BroadcastingCarEventType.GreenFlag:
                        secondsBack = 7f;
                        duration = 25f;
                        break;
                    case ksBroadcastingNetwork.BroadcastingCarEventType.PenaltyCommMsg:
                        secondsBack = 10f;
                        duration = 6f;
                        break;
                    case ksBroadcastingNetwork.BroadcastingCarEventType.Accident:
                        secondsBack = 10f;
                        duration = 10f;
                        break;
                    default:
                        break;
                }

                float requestedTime = Evt.TimeMs - (secondsBack * 1000);
                
                onHighlightReplayDelegate(Evt, requestedTime, duration);
            });
        }
    }
}
