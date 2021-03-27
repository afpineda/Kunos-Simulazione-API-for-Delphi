using System;
using ksBroadcastingNetwork;

namespace ksBroadcastingTestClient.ClientConnections
{
    public class ClientConnectionViewModel : KSObservableObject
    {
        public string IPort => Client.IpPort;
        public ACCUdpRemoteClient Client { get; }
        public Action<ACCUdpRemoteClient> OnClientConnectedCallback { get; }
        public int ConnectionId { get => Get<int>(); set => Set(value); }
        public bool Connected { get => Get<bool>(); set => Set(value); }
        public bool IsReadonly { get => Get<bool>(); set => Set(value); }
        public string ErrorMessage { get => Get<string>(); set => Set(value); }

        public ClientConnectionViewModel(ACCUdpRemoteClient c, Action<ACCUdpRemoteClient> onClientConnectedCallback)
        {
            Client = c;
            OnClientConnectedCallback = onClientConnectedCallback;
            c.MessageHandler.OnConnectionStateChanged += ConnectionStateChanged;
        }

        private void ConnectionStateChanged(int connectionId, bool connectionSuccess, bool isReadonly, string error)
        {
            ConnectionId = connectionId;
            Connected = connectionSuccess;
            IsReadonly = IsReadonly;
            ErrorMessage = error;

            if (Connected)
                OnClientConnectedCallback?.Invoke(Client);

        }

        public void Disconnect()
        {
            Client.MessageHandler.OnConnectionStateChanged -= ConnectionStateChanged;
            Client.Shutdown();
        }
    }
}