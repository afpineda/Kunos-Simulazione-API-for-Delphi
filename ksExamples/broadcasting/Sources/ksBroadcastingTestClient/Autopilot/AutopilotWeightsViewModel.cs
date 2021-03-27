using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ksBroadcastingTestClient.Autopilot
{
    public class AutopilotWeightsViewModel : KSObservableObject
    {
        public string JumpinessHint { get; } = "How willingly will the autopilot jump between cars based on recent action.\nReduce this if it's becoming confusing for the viewer, increase it if we it sticks too long with one car and we miss other things";
        public float Jumpiness { get => Get<float>(); private set => Set(value); }

        public string RacePositionHint { get; } = "How likely the autopilot will focus lead cars. If this is too high, it would follow a solo leader while the pack behind is fighting for it's life; increase this if you start to see insignificant stuff";
        public float RacePosition { get => Get<float>(); private set => Set(value); }
    }
}
