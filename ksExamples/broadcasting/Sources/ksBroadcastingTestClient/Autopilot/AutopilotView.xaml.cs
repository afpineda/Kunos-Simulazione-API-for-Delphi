using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace ksBroadcastingTestClient.Autopilot
{
    /// <summary>
    /// Interaction logic for AutopilotView.xaml
    /// </summary>
    public partial class AutopilotView : UserControl
    {
        public AutopilotView()
        {
            InitializeComponent();
            AutorefreshSorting();
        }

        private async void AutorefreshSorting()
        {
            var sorter = FindResource("src") as CollectionViewSource;

            while (sorter != null)
            {
                sorter?.View?.Refresh();
                await Task.Delay(1000);
            }
        }
    }
}
