unit ksBroadcasting.UDP;

interface

uses
  Classes,
  System.Net.Socket,
  System.Threading,
  System.Generics.Collections,
  System.SysUtils,
  ksBroadcasting,
  ksBroadcasting.Data;

type
  TksBroadcastingProtocolUDPImpl = class(TksBroadcastingProtocol)
  private
    FSocket: TSocket;
    FRemoteEndPoint: TNetEndPoint;
    listener: ITask;
    procedure listenerWork;
  protected
{$IFDEF DEBUG}
    procedure debugInvalidMessage(strm: TBytesStream); override;
{$ENDIF}
    procedure SendMessage(const outStrm: TBytesStream); override;
    function ReceiveMessage: TBytesStream; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    procedure Open(const endPoint: TNetEndPoint;
      const displayName, connectionPassword, commandPassword: string;
      msUpdateInterval: Integer = 1000);
    property isConnected;
  end;

type
  TksUDPProtocol = class(TksBroadcastingProtocolUDPImpl)
  public type
    TOnConnectionEvent = procedure(Sender: TksUDPProtocol;
      const connected, isReadOnly: boolean; const errMsg: string) of object;
    TOnEntryListEvent = procedure(Sender: TksUDPProtocol;
      NewList: TObjectList<TKsCarInfo>) of object;
    TOnEntryListUpdateEvent = procedure(Sender: TksUDPProtocol;
      const carInfo: TKsCarInfo) of object;
    TOnCarDataUpdateEvent = procedure(Sender: TksUDPProtocol;
      const carInfo: TKsCarData) of object;
    TOnSessionDataUpdateEvent = procedure(Sender: TksUDPProtocol;
      const sessionData: TKsSessionData) of object;
    TOnTrackDataEvent = procedure(Sender: TksUDPProtocol;
      const trackData: TksTrackData) of object;
    TOnBroadcastingEventEvent = procedure(Sender: TksUDPProtocol;
      const event: TksBroadcastingEvent) of object;
  private
    FsynchronizedEvents: boolean;
    FOnConnection: TOnConnectionEvent;
    FOnEntryList: TOnEntryListEvent;
    FOnEntryListUpdate: TOnEntryListUpdateEvent;
    FOnCarDataUpdate: TOnCarDataUpdateEvent;
    FOnSessionDataUpdate: TOnSessionDataUpdateEvent;
    FOnTrackData: TOnTrackDataEvent;
    FEntryListIsComplete: boolean;
    FOnBroadcastingEvent: TOnBroadcastingEventEvent;

  protected
    procedure doOnBroadcastingEvent(const event: TksBroadcastingEvent);
      override;
    procedure doOnEntryListCar(const carInfo: TKsCarInfo); override;
    procedure doOnEntryListComplete; override;
    procedure doOnNewEntryList(const entryCount: WORD); override;
    procedure doOnRealTimeCarUpdate(const carData: TKsCarData); override;
    procedure doOnRealTimeUpdate(const sessionData: TKsSessionData); override;
    procedure doOnRegistrationResult(const connectionId: Int32;
      const success, isReadOnly: boolean; const errMsg: string); override;
    procedure doOnTrackData(const trackData: TksTrackData); override;
  public
    procedure RequestEntryList;
    constructor Create(const synchronizedEvents: boolean = true);
    // destructor Destroy; override;
    property OnBroadcastingEvent: TOnBroadcastingEventEvent
      read FOnBroadcastingEvent write FOnBroadcastingEvent;
    property OnCarDataUpdate: TOnCarDataUpdateEvent read FOnCarDataUpdate
      write FOnCarDataUpdate;
    property OnConnection: TOnConnectionEvent read FOnConnection
      write FOnConnection;
    property OnEntryList: TOnEntryListEvent read FOnEntryList
      write FOnEntryList;
    property OnEntryListUpdate: TOnEntryListUpdateEvent read FOnEntryListUpdate
      write FOnEntryListUpdate;
    property OnSessionDataUpdate: TOnSessionDataUpdateEvent
      read FOnSessionDataUpdate write FOnSessionDataUpdate;
    property OnTrackData: TOnTrackDataEvent read FOnTrackData
      write FOnTrackData;
  end;

implementation

// uses
// System.SysUtils;

// ----------------------------------------------------------------------------
// TksBroadcastingProtocolUDPImpl
// ----------------------------------------------------------------------------

constructor TksBroadcastingProtocolUDPImpl.Create;
begin
  inherited Create;
  FSocket := TSocket.Create(TSocketType.UDP);
  listener := nil;
end;

destructor TksBroadcastingProtocolUDPImpl.Destroy;
begin
  Close;
  FSocket.Free;
  inherited;
end;

procedure TksBroadcastingProtocolUDPImpl.listenerWork;
var
  active: boolean;
begin
  active := true;
  while (active) do
    try
      ProcessMessage;
    except
      on ESocketError do
        active := false;
      else;
    end;
end;

procedure TksBroadcastingProtocolUDPImpl.Open(const endPoint: TNetEndPoint;
  const displayName, connectionPassword, commandPassword: string;
  msUpdateInterval: Integer);
begin
  // if (not(TSocketState.connected in FSocket.state)) then
  // begin
  FRemoteEndPoint := endPoint;
  FSocket.Bind(0);
  RequestConnection(displayName, connectionPassword, msUpdateInterval,
    commandPassword);
  listener := TTask.Create(listenerWork);
  listener.Start;
  // end
  // else
  // raise Exception.Create('Already open');
end;

procedure TksBroadcastingProtocolUDPImpl.Close;
begin
  if (TSocketState.connected in FSocket.state) then
  begin
    RequestDisconnect;
    FSocket.Close;
  end;
end;

procedure TksBroadcastingProtocolUDPImpl.SendMessage(const outStrm
  : TBytesStream);
begin
  FSocket.SendTo(outStrm.Bytes, FRemoteEndPoint, 0, outStrm.Size);
end;

function TksBroadcastingProtocolUDPImpl.ReceiveMessage: TBytesStream;
var
  Data: TBytes;
  ep: TNetEndPoint;
  count: Integer;
begin
  repeat
    Data := FSocket.ReceiveFrom;
  until (Length(Data) > 0);

  Result := TBytesStream.Create(Data);
end;

{$IFDEF DEBUG}

procedure TksBroadcastingProtocolUDPImpl.debugInvalidMessage
  (strm: TBytesStream);
begin
  if (strm = nil) then
    raise Exception.Create('NIL Datagram received')
  else
    raise Exception.Create('Invalid datagram: L=' + strm.Size.ToString + ' Pos='
      + strm.Position.ToString);
end;
{$ENDIF}
// ----------------------------------------------------------------------------
// TksUDPProtocol
// ----------------------------------------------------------------------------

constructor TksUDPProtocol.Create(const synchronizedEvents: boolean = true);
begin
  inherited Create;
  FsynchronizedEvents := synchronizedEvents;
  FEntryListIsComplete := false;
  FOnConnection := nil;
  FOnEntryList := nil;
  FOnEntryListUpdate := nil;
  FOnCarDataUpdate := nil;
  FOnSessionDataUpdate := nil;
  FOnTrackData := nil;
  FOnBroadcastingEvent := nil;
end;

// destructor TksUDPProtocol.Destroy;
// begin
// FCurrentEntryList.Free;
// inherited;
// end;

procedure TksUDPProtocol.RequestEntryList;
begin
  inherited RequestEntryList(true);
end;

procedure TksUDPProtocol.doOnRegistrationResult(const connectionId: Int32;
  const success, isReadOnly: boolean; const errMsg: string);
begin
  if (not success) then
    FSocket.Close;
  if (Assigned(FOnConnection)) then
  begin
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnConnection(self, success, isReadOnly, errMsg)
        end)
    else
      try
        FOnConnection(self, success, isReadOnly, errMsg);
      except
      end;
  end;
end;

procedure TksUDPProtocol.doOnNewEntryList(const entryCount: WORD);
begin
  FEntryListIsComplete := false;
  if (Assigned(FOnEntryList)) then
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnEntryList(self, nil);
        end)
    else
      try
        FOnEntryList(self, nil);
      except
      end;
end;

procedure TksUDPProtocol.doOnEntryListComplete;
var
  NewList: TObjectList<TKsCarInfo>;
begin
  FEntryListIsComplete := true;
  if (Assigned(FOnEntryList)) then
  begin
    // Copy internal entry list
    NewList := TObjectList<TKsCarInfo>.Create;
    NewList.OwnsObjects := true;
    TKsCarInfo.Copy(InternalEntryList, NewList);

    // send event
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnEntryList(self, NewList);
        end)
    else
      try
        FOnEntryList(self, NewList);
      except
      end;
  end;
end;

procedure TksUDPProtocol.doOnEntryListCar(const carInfo: TKsCarInfo);
begin
  if (FEntryListIsComplete) and (Assigned(FOnEntryListUpdate)) then
  begin
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnEntryListUpdate(self, carInfo);
        end)
    else
      try
        FOnEntryListUpdate(self, carInfo);
      except
      end;
  end;
end;

procedure TksUDPProtocol.doOnRealTimeCarUpdate(const carData: TKsCarData);
begin
  if (Assigned(FOnCarDataUpdate)) then
  begin
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnCarDataUpdate(self, carData);
        end)
    else
      try
        FOnCarDataUpdate(self, carData);
      except
      end;
  end;
end;

procedure TksUDPProtocol.doOnRealTimeUpdate(const sessionData: TKsSessionData);
begin
  if (Assigned(FOnSessionDataUpdate)) then
  begin
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnSessionDataUpdate(self, sessionData);
        end)
    else
      try
        FOnSessionDataUpdate(self, sessionData);
      except
      end;
  end;
end;

procedure TksUDPProtocol.doOnTrackData(const trackData: TksTrackData);
begin
  if (Assigned(FOnTrackData)) then
  begin
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnTrackData(self, trackData);
        end)
    else
      try
        FOnTrackData(self, trackData);
      except
      end;
  end;
end;

procedure TksUDPProtocol.doOnBroadcastingEvent(const event
  : TksBroadcastingEvent);
begin
  if (Assigned(FOnBroadcastingEvent)) then
  begin
    if (FsynchronizedEvents) then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnBroadcastingEvent(self, event);
        end)
    else
      try
        FOnBroadcastingEvent(self, event);
      except
      end;
  end;
end;

end.
