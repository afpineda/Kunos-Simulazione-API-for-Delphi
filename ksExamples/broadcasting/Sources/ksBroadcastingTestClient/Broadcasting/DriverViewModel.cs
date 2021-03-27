using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ksBroadcastingNetwork;
using ksBroadcastingNetwork.Structs;

namespace ksBroadcastingTestClient.Broadcasting
{
    public class DriverViewModel : KSObservableObject
    {
        public int DriverIndex { get; }
        public string FirstName { get => Get<string>(); private set => Set(value); }
        public string LastName { get => Get<string>(); private set => Set(value); }
        public string ShortName { get => Get<string>(); private set => Set(value); }
        public string DisplayName { get => Get<string>(); private set => Set(value); }
        public DriverCategory Category { get => Get<DriverCategory>(); private set => Set(value); }


        public DriverViewModel(DriverInfo driverUpdate, int driverIndex)
        {
            DriverIndex = driverIndex;
            FirstName = driverUpdate.FirstName;
            LastName = driverUpdate.LastName;
            ShortName = driverUpdate.ShortName;
            Category = driverUpdate.Category;

            var displayName = $"{FirstName} {LastName}".Trim();
            if (displayName.Length > 35)
                displayName = $"{FirstName?.First()}. {LastName}".TrimStart('.').Trim();
            if (displayName.Length > 35)
                displayName = $"{LastName}".Trim();
            if (displayName.Length > 35)
                displayName = $"{LastName.Substring(0, 33)}...".Trim();

            if (string.IsNullOrEmpty(displayName))
                displayName = "NO NAME";

            DisplayName = displayName;
        }
    }
}
