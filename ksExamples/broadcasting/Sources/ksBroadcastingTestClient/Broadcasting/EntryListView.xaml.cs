using System;
using System.Collections.Generic;
using System.ComponentModel;
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
using System.Windows.Shapes;

namespace ksBroadcastingTestClient.Broadcasting
{
    /// <summary>
    /// Interaction logic for EntryListView.xaml
    /// </summary>
    public partial class EntryListView : UserControl
    {
        public EntryListView()
        {
            InitializeComponent();

            AutorefreshSorting();
        }

        /// <summary>
        /// Just a lazy hack to have the EntryList always sorted by position
        /// </summary>
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
