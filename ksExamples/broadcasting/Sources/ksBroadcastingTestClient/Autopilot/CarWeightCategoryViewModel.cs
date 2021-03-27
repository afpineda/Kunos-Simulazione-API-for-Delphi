using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ksBroadcastingTestClient.Autopilot
{
    public enum CarWeightCategoryEnum { Proximity, Pack, Position, FocusFast, FocusSlow, Pace }
    public class CarWeightCategoryViewModel : KSObservableObject
    {
        public CarWeightCategoryEnum Category { get; }
        public float RawValue { get => Get<float>(); set => Set(value); }
        public float WeightedValue { get => Get<float>(); set => Set(value); }
        public string Hint { get => Get<string>(); set => Set(value); }

        public CarWeightCategoryViewModel(CarWeightCategoryEnum category)
        {
            Category = category;
        }
    }
}
