unit ksBroadcasting.UDP;

{ *******************************************************

  Implementation of Kunos Simulazione's broadcasting
  client API for Delphi

  *******************************************************

  This Delphi/pascal translation:
  (C) 2021. Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-03-28] First implementation

  [2021-04-03] Reworked

  [2021-04-10] Added "ACCEnpointOnThisComputer"

  ******************************************************* }

interface

uses
  Classes,
  Winapi.Winsock2,
  System.Net.Socket,
  System.SysUtils,
  ksBroadcasting.Protocol,
  ksBroadcasting;

type
  TksUDPDelegate = class(TInterfacedObject, IksMessageDelegate)
    {
      PURPOUSE:
      To implement UDP/IPv4 datagrams for the broadcasting protocol

      GENERAL USAGE:
      - Create
      - Assign "RemoteEndPoint"
      - Pass to TksBroadcastingMsgHandler's constructor

      NOTES:
      Implemented by the means of Wnapi.Winsock2. However, network addresses are
      implemented as System.Net.Socket.TNetEndPoint instances.
      RemoteEndPoint is NOT allowed to change while broadcasting protocol is running.
    }
  strict private
    FSocket: Winapi.Winsock2.TSocket;
    FRemoteEndPoint: TNetEndPoint;
    class procedure CheckSocketResult(ResultCode: integer; const Op: string);
    procedure SetRemoteEndPoint(const ep: TNetEndPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SendTo(Data: TBytesStream);
    function ReceiveFrom: TBytesStream;
    property RemoteEndPoint: TNetEndPoint read FRemoteEndPoint
      write SetRemoteEndPoint;
  end;

type
  TksUDPv4BroadcastingProtocol = class(TksBroadcastingProtocol)
    {
      PURPOUSE:
      Implementation of UDP/IPV4 broadcasting protocol
    }
  private
    FDelegate: TksUDPDelegate;
    FRemoteEndPoint: TNetEndPoint;
    function GetRemoteEndPoint: TNetEndPoint;
  protected
    procedure BeforeRegister; override;
  public
    class procedure ACCEnpointOnThisComputer(out ep: TNetEndPoint;
      out connectionPwd, commandPwd: string);
    constructor Create(const synchronizedEvents: boolean = true);
    procedure Start(const RemoteEndPoint: TNetEndPoint;
      const displayName: string; const connectionPassword: string;
      const msUpdateInterval: Int32 = 1000; const commandPassword: string = '');
    property RemoteEndPoint: TNetEndPoint read GetRemoteEndPoint;
  end;

type
  EksAlreadyRegistered = class(Exception);

implementation

// --------------------------------------------------------------------------
// TksUDPDelegate
// --------------------------------------------------------------------------

uses
  SysUtils,
  System.IOUtils,
  System.JSON,
  Winapi.ShlObj,
  Winapi.Windows,
  System.Netconsts;

class procedure TksUDPDelegate.CheckSocketResult(ResultCode: integer;
  const Op: string);
begin
  if ResultCode < 0 then
    if WSAGetLastError <> WSAEWOULDBLOCK then
      raise ESocketError.CreateResFmt(@sSocketError,
        [SysErrorMessage(WSAGetLastError), WSAGetLastError, Op]);
end;

constructor TksUDPDelegate.Create;

begin
  inherited Create;
  FSocket := INVALID_SOCKET;
end;

destructor TksUDPDelegate.Destroy;
begin
  Stop;
  inherited;
end;

procedure TksUDPDelegate.SendTo(Data: TBytesStream);
var
  addr: sockaddr_in;
  Result: integer;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    addr := FRemoteEndPoint;
    Result := Winapi.Winsock2.SendTo(FSocket, Data.Bytes[0], Data.Size, 0,
      @addr, SizeOf(addr));
    CheckSocketResult(Result, 'sendto');
  end
  else
    raise ESocketError.Create('Socket not open');
end;

function TksUDPDelegate.ReceiveFrom: TBytesStream;
var
  Buffer: TBytes;
  Sender: sockaddr_in;
  aux: integer;
  receivedSize: integer;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    SetLength(Buffer, 4096);
    aux := SizeOf(Sender);
    // repeat
    FillChar(Sender, aux, 0);
    receivedSize := Winapi.Winsock2.recvfrom(FSocket, Buffer[0], Length(Buffer),
      0, Psockaddr(@Sender)^, aux);
    CheckSocketResult(receivedSize, 'recvfrom');
    // until (Sender.sin_addr.S_addr = FRemoteEndPoint.Address.addr.S_addr);
    Result := TBytesStream.Create;
    Result.WriteBuffer(Buffer, receivedSize);
    Result.Position := 0;
  end
  else
    raise ESocketError.Create('Socket not open');
end;

procedure TksUDPDelegate.Start;
var
  addr: sockaddr_in;
begin
  if FSocket = INVALID_SOCKET then
  begin
    FSocket := Socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if (FSocket = INVALID_SOCKET) then
      raise ESocketError.Create('socket creation failed with code ' +
        WSAGetLastError.ToString);
    try
      addr.sin_family := AF_INET;
      addr.sin_port := 0;
      addr.sin_addr.S_addr := INADDR_ANY;
      CheckSocketResult(bind(FSocket, sockaddr(addr), SizeOf(addr)), 'bind');
    except
      closesocket(FSocket);
      FSocket := INVALID_SOCKET;
      raise;
    end;
  end;
end;

procedure TksUDPDelegate.Stop;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET
  end;
end;

procedure TksUDPDelegate.SetRemoteEndPoint(const ep: TNetEndPoint);
begin
  if FSocket = INVALID_SOCKET then
  begin
    FRemoteEndPoint := ep;
  end
  else
    raise ESocketError.Create
      ('Remote end point not allowed to change while socket is in use');
end;

// --------------------------------------------------------------------------
// TksUDPv4BroadcastingProtocol
// --------------------------------------------------------------------------

const
  BROADCASTING_FILE = 'Assetto Corsa Competizione\Config\broadcasting.json';

class procedure TksUDPv4BroadcastingProtocol.ACCEnpointOnThisComputer
  (out ep: TNetEndPoint; out connectionPwd, commandPwd: string);
var
  pidl: PItemIDList;
  aux: array [1 .. MAX_PATH] of Char;
  shresult: integer;
  mydocs, filename: string;
  Data: TBytes;
  text: string;
  l: integer;
  encoding: TEncoding;
  root: TJSONObject;
  item: TJSONValue;
begin
  shresult := SHGetFolderLocation(0, CSIDL_MYDOCUMENTS, 0, 0, pidl);
  if (shresult = S_OK) then
  begin
    ep.Family := AF_INET;
    ep.SetAddress('127.0.0.1');

    // Put "My Documents" path into "mydocs" var
    FillChar(aux, SizeOf(aux), 0);
    SHGetPathFromIDList(pidl, PChar(@aux));
    SetString(mydocs, PChar(@aux), Length(aux));
    mydocs := Trim(mydocs);
    ILFree(pidl);

    // Read "Broadcasting.json"
    filename := IncludeTrailingPathDelimiter(mydocs) + BROADCASTING_FILE;
    Data := TFile.ReadAllBytes(filename);
    l := TEncoding.GetBufferEncoding(Data, encoding);
    if (l = 0) and (Length(Data) > 1) and (Char(Data[1]) = #0) then
      // UTF-16 encoding without BOM
      text := TEncoding.Unicode.GetString(Data)
    else
      // Use BOM
      text := encoding.GetString(Data, l, Length(Data) - l);

    // Parse and read JSON data
    root := TJSONObject.ParseJSONValue(text, false, true);
    item := root.FindValue('updListenerPort');
    if (item <> nil) and (item is TJSONNumber) then
      ep.port := (item as TJSONNumber).AsInt
    else
      raise EJSONException.Create('field "updListenerPort" not found');
    item := root.FindValue('connectionPassword');
    if (item <> nil) and (item is TJSONString) then
      connectionPwd := (item as TJSONString).Value
    else
      raise EJSONException.Create('field "connectionPassword" not found');
    item := root.FindValue('commandPassword');
    if (item <> nil) and (item is TJSONString) then
      commandPwd := (item as TJSONString).Value
    else
      raise EJSONException.Create('field "commandPassword" not found');
  end
  else
    raise EDirectoryNotFoundException.Create('"My Documents" folder not found');
end;

constructor TksUDPv4BroadcastingProtocol.Create(const synchronizedEvents
  : boolean);
begin
  // Note: interfaced object. No need to call FDelegate.Free.
  FDelegate := TksUDPDelegate.Create;
  inherited Create(FDelegate, synchronizedEvents);
end;

function TksUDPv4BroadcastingProtocol.GetRemoteEndPoint: TNetEndPoint;
begin
  Result := FDelegate.RemoteEndPoint;
end;

procedure TksUDPv4BroadcastingProtocol.Start(const RemoteEndPoint: TNetEndPoint;
  const displayName: string; const connectionPassword: string;
  const msUpdateInterval: Int32 = 1000; const commandPassword: string = '');
begin
  FRemoteEndPoint := RemoteEndPoint;
  inherited Start(displayName, connectionPassword, msUpdateInterval,
    commandPassword);
end;

procedure TksUDPv4BroadcastingProtocol.BeforeRegister;
begin
  FDelegate.RemoteEndPoint := FRemoteEndPoint;
end;

// -------------------------------------------------------------------------

var
  WSAerr: integer;
  wsaInitData: TWSAData;

initialization

WSAerr := WSAStartup(514, wsaInitData);
if (WSAerr <> 0) then
  raise ESocketError.Create('Sockets library unavailable');

finalization

WSACleanup;

end.
