The testclient solution is split into 2 projects:

ksBroadcastingNetwork only contains the UDP network API and can be directly used in your own .Net application.
The BroadcastingNetworkProtocol.cs is a single file showcasing all the communication between the broadcasting app and the ACC client (=the netcode); The ACCUdpRemoteClient.cs handles the UDP socket.

The ksBroadcastingTestClient is the example WPF UI used for our testclient. It's meant to be a simple interface using the functions and displaying the data, without really trying to do an actually usable broadcasting application.
Use it to see how the ksBroadcastingNetwork can be used for your own implementation, and watch the official forums for a thread dedicated to this SDK.

To setup ACC to accept UDP connections, please refer to the SDK\Testclient\readme.txt