using ksBroadcastingNetwork;
using ksBroadcastingTestClient.Autopilot;
using ksBroadcastingTestClient.Broadcasting;
using ksBroadcastingTestClient.ClientConnections;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ksBroadcastingTestClient
{
    public class MainViewModel : KSObservableObject
    {
        public int ActiveIndexPage { get => Get<int>(); set => Set(value); }

        public ClientPanelViewModel ClientPanelVM { get; }
        public BroadcastingViewModel BroadcastingVM { get; }
        public AutopilotViewModel AutopilotVM { get; }
        public SessionInfoViewModel SessionInfoVM { get; }
        public ReplayControlViewModel ReplayVM { get; }

        public KSRelayCommand OnCloseWindowCommand { get; }

        public MainViewModel()
        {
            ClientPanelVM = new ClientPanelViewModel(OnClientConnected);
            BroadcastingVM = new BroadcastingViewModel();
            AutopilotVM = new AutopilotViewModel();
            SessionInfoVM = new SessionInfoViewModel();
            ReplayVM = new ReplayControlViewModel();

            OnCloseWindowCommand = new KSRelayCommand(Shutdown);
        }
        public void OnClientConnected(ACCUdpRemoteClient newClient)
        {
            BroadcastingVM.RegisterNewClient(newClient);
            AutopilotVM.RegisterNewClient(newClient);
            SessionInfoVM.RegisterNewClient(newClient);
            ReplayVM.RegisterNewClient(newClient);

            // in case we are still on the first page and this was a realtime updating client,
            // we can move on
            if (newClient.MsRealtimeUpdateInterval > 0 && ActiveIndexPage == 0)
                ActiveIndexPage = 1;
        }

        private void Shutdown(object obj = null)
        {
            ClientPanelVM.Shutdown();
        }
    }
}
