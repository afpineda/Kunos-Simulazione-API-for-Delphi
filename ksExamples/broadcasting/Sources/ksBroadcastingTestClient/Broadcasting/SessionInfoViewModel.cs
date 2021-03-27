using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ksBroadcastingNetwork;
using ksBroadcastingNetwork.Structs;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class SessionInfoViewModel : KSObservableObject
    {
        public TimeSpan SessionTime { get => Get<TimeSpan>(); private set => Set(value); }
        public string SessionTimeDisplay { get => Get<string>(); private set => Set(value); }
        public string RemainingTime { get => Get<string>(); private set => Set(value); }
        public string TrackDisplayName { get => Get<string>(); private set => Set(value); }
        public SessionPhase Phase { get => Get<SessionPhase>(); private set => Set(value); }
        public RaceSessionType SessionType { get => Get<RaceSessionType>(); private set => Set(value); }

        public string TimeOfDayInfo { get => Get<string>(); private set => Set(value); }
        public string TempInfo { get => Get<string>(); private set => Set(value); }
        public string RainInfo { get => Get<string>(); private set => Set(value); }
        public string CloudInfo { get => Get<string>(); private set => Set(value); }

        internal void RegisterNewClient(ACCUdpRemoteClient newClient)
        {
            if (newClient.MsRealtimeUpdateInterval > 0)
            {
                // This client will send realtime updates, we should listen
                newClient.MessageHandler.OnTrackDataUpdate += MessageHandler_OnTrackDataUpdate;
                newClient.MessageHandler.OnRealtimeUpdate += MessageHandler_OnRealtimeUpdate;
            }
        }

        private void MessageHandler_OnRealtimeUpdate(string sender, RealtimeUpdate update)
        {
            SessionTime = update.SessionTime;
            SessionTimeDisplay = SessionTime.ToString("hh\\:mm\\:ss");
            if(update.SessionEndTime.TotalMilliseconds < 0)
            {
                RemainingTime = "∞";
            }
            else
            {
                var remaining = update.SessionEndTime - update.SessionTime;

                if (update.SessionTime.TotalMilliseconds < 0)
                    RemainingTime = "00:00";
                else if (remaining.TotalHours > 2)
                    RemainingTime = $"{remaining.TotalHours:F0}h";
                else if (remaining.TotalMinutes > 60)
                    RemainingTime = $"{remaining.TotalMinutes:F0}m";
                else
                    RemainingTime = $"{remaining.TotalMinutes:2,F0}:{remaining.Seconds,2}";
            }

            Phase = update.Phase;
            SessionType = update.SessionType;

            TimeOfDayInfo = update.TimeOfDay.ToString("hh\\:mm");

            if (update.Clouds < 0.2)
                CloudInfo = "Sunny";
            else if (update.Clouds < 0.40)
                CloudInfo = "Light clouds";
            else if(update.Clouds < 0.65)
                CloudInfo = "Cloudy";
            else
                CloudInfo = "World's end";

            TempInfo = $"{CloudInfo}        {update.AmbientTemp} °C - Track {update.TrackTemp} °C";


            if (update.RainLevel < 0.1 && update.Wetness < 0.1)
                RainInfo = $"Clear";
            else
                RainInfo = $"{update.RainLevel:P0} rain, {update.Wetness:P0} wet";

        }

        private void MessageHandler_OnTrackDataUpdate(string sender, TrackData trackUpdate)
        {
            TrackDisplayName = trackUpdate.TrackName;
        }
    }
}
