using ksBroadcastingNetwork;
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ksBroadcastingTestClient.ClientConnections
{
    public class ClientPanelViewModel : KSObservableObject
    {
        public string IP { get => Get<string>(); set => Set(value); }
        public int Port { get => Get<int>(); set => Set(value); }
        public string DisplayName { get => Get<string>(); set => Set(value); }
        public string ConnectionPw { get => Get<string>(); set => Set(value); }
        public string CommandPw { get => Get<string>(); set => Set(value); }
        public int RealtimeUpdateIntervalMS { get => Get<int>(); set => Set(value); }
        public KSRelayCommand ConnectCmd { get; }
        public KSRelayCommand DisconnectCmd { get; }

        public ObservableCollection<ClientConnectionViewModel> Clients { get; } = new ObservableCollection<ClientConnectionViewModel>();
        public Action<ACCUdpRemoteClient> OnClientConnectedCallback { get; }
        public ClientConnectionViewModel SelectedClient { get => Get<ClientConnectionViewModel>(); set => Set(value); }

        public ClientPanelViewModel(Action<ACCUdpRemoteClient> onClientConnectedCallback)
        {
            ConnectCmd = new KSRelayCommand(DoConnect);
            DisconnectCmd = new KSRelayCommand(DoDisconnect);

            IP = "127.0.0.1";
            Port = 9000;
            DisplayName = "Your name";
            ConnectionPw = "asd";
            CommandPw = "";
            RealtimeUpdateIntervalMS = 250;
            OnClientConnectedCallback = onClientConnectedCallback;
        }

        public void DoConnect(object parameter = null)
        {
            var c = new ACCUdpRemoteClient(IP, Port, DisplayName, ConnectionPw, CommandPw, RealtimeUpdateIntervalMS);
            Clients.Add(new ClientConnectionViewModel(c, OnClientConnectedCallback));
        }

        private void DoDisconnect(object obj)
        {
            if(SelectedClient != null)
            {
                SelectedClient.Disconnect();
                Clients.Remove(SelectedClient);
                SelectedClient = null;
            }
        }

        internal void Shutdown()
        {
            foreach (var client in Clients)
            {
                client.Disconnect();
            }
            Clients.Clear();
        }
    }
}
