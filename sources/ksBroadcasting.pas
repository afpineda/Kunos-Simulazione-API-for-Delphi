unit ksBroadcasting;

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

  [2021-03-10] First implementation

  [2021-04-03] Added TksThreadedBroadcastingMsgHandler

  ******************************************************* }

interface

uses
  System.Classes,
  System.Threading,
  System.SyncObjs,
  ksBroadcasting.Data;

{
  SUMMARY:

  - IksMessageDelegate: interface to implementa data transmision
  - TksBroadcastingMsgHandler: Message handler for broadcasting protocol
  - TksThreadedBroadcastingMsgHandler: same as above, but threaded.
}

type
  IksMessageDelegate = interface
    {
      PURPOUSE:
      To delegate actual message sending and receiving. The broadcasting
      protocol is independant from the transport layer, but datagram-oriented.

      GENERAL USAGE:
      Implement this interface in order to perform actual message sending/
      receiving. Messages/datagrams are TBytesStream instances.

      START:
      Called one or more times before any message is sent or received.

      STOP:
      Called one or more times when no more messages are to be sent or received.
      Should unblock any pending thread at "ReceiveFrom" by raising an exception
      at such thread. For example, by closing a socket.

      SendTo:
      Send datagram. Non-blocking.

      ReceiveFrom:
      Receive datagram. Blocking. Caller should destroy the received stream.
    }
    procedure Start;
    procedure Stop;
    procedure SendTo(Data: TBytesStream);
    function ReceiveFrom: TBytesStream;
  end;

type
  TksBroadcastingMsgHandler = class
    {
      PURPOUSE:
      To handle broadcasting protocol messages. Both sending and receiving.

      GENERAL USAGE:
      Abstract class. Actual message processing is left to descendant classes.

      ProcessMessage:
      Should be called at regular intervals. Waits for a datagram,
      decodes a message, and calls the corresponding "Msg" method.

      MSG:
      Any object passed to "Msg" (TKsCarInfo or TksTrackData) should be
      destroyed by the descendant class when not used.
    }
  public const
    BROADCASTING_PROTOCOL_VERSION = 4;
  strict private
    FConnectionID: Int32;
    FMessageDelegate: IksMessageDelegate;
    FMsgTimestamp: TDateTime;
    FRegReqTimestamp: TDateTime;
    FUpdateInterval: integer;
    lastEntrylistRequest: TDateTime;
    RegRequestPending: boolean;
    function GetRegistered: boolean;
  protected
    function GetRegistrationRequestTimedOut: boolean;
    procedure ProcessMessage;
    procedure Msg(const result: TKsRegistrationResult); overload;
      virtual; abstract;
    procedure Msg(const sessionData: TksSessionData); overload;
      virtual; abstract;
    procedure Msg(const carData: TksCarData); overload; virtual; abstract;
    procedure Msg(const carInfo: TKsCarInfo); overload; virtual; abstract;
    procedure Msg(const carEntryCount: integer); overload; virtual; abstract;
    procedure Msg(const trackData: TksTrackData); overload; virtual; abstract;
    procedure Msg(const event: TksBroadcastingEvent); overload;
      virtual; abstract;
    function Register(const displayName: string;
      const connectionPassword: string; const msUpdateInterval: Int32 = 1000;
      const commandPassword: string = ''): boolean;
    function Unregister: boolean;
    property connectionId: Int32 read FConnectionID;
    property MessageDelegate: IksMessageDelegate read FMessageDelegate;
  public
    constructor Create(msgDelegate: IksMessageDelegate);
    destructor Destroy; override;
    function IsServerInactive(timeMs: Int64): boolean;
    procedure RequestEntryList(const force: boolean = false);
    procedure RequestFocus(const carIndex: UInt16; const cameraSet: string = '';
      const camera: string = ''); overload;
    procedure RequestFocus(const cameraSet, camera: string); overload;
    procedure RequestInstantReplay(const startSessionTime, durationMS: Single;
      const initialFocusedCarIndex: Int32 = -1;
      const initialCameraSet: string = ''; const initialCamera: string = '');
    procedure RequestHUDPage(const HUDPage: string);
    procedure RequestTrackData;
    property Registered: boolean read GetRegistered;
    property RegistrationRequestTimedOut: boolean
      read GetRegistrationRequestTimedOut;
    property LastMsgTimestamp: TDateTime read FMsgTimestamp;
    property LastRegRequestTimestamp: TDateTime read FRegReqTimestamp;
    property UpdateIntervalMs: integer read FUpdateInterval;
  end;

type
  TksThreadedBroadcastingMsgHandler = class(TksBroadcastingMsgHandler)
    {
      PURPOUSE:
      To handle broadcasting protocol messages.
      Incoming messages are handled in a separate thread.
      Note: abstract class.

      START:
      Send a registration request and keep trying (in a separate thread)
      until attended (succesful or not) or "Stop" is called.
      If already registered to any server, "Stop" is called previously.

      STOP:
      If registered, send an unregistration request.
      In not registered, cancel any pending registration request.

      METHODS CALLED FROM A SEPARATE THREAD:
      - Msg
      - RetryRegistrationRequest
      - NotifyNoServerActivity
    }
  strict private
    ongoing: TEvent;
    listener: ITask;
    registrationSpotter: ITask;
    cancelBackgroundTasks: boolean;
    serverActivityFlag: boolean;
    FConnectionPassword: string;
    FCommandPassword: string;
    FDisplayName: string;
    FMaxServerInactivityMs: Int64;

    procedure ReceiveTask;
    procedure RegistrationTask;
  protected
    procedure NotifyNoServerActivity; virtual;
    procedure AfterUnregister; virtual;
    procedure BeforeRegister; virtual;
    procedure RetryRegistrationRequest; virtual;
    property displayName: string read FDisplayName;
    property connectionPassword: string read FConnectionPassword;
    property commandPassword: string read FDisplayName;
  public
    constructor Create(msgDelegate: IksMessageDelegate);
    destructor Destroy; override;
    procedure Start(const displayName: string; const connectionPassword: string;
      const msUpdateInterval: Int32 = 1000; const commandPassword: string = '');
    procedure Stop;
    property MaxServerInactivityMs: Int64 read FMaxServerInactivityMs
      write FMaxServerInactivityMs;
  end;

implementation

uses
  System.DateUtils,
  System.SysUtils;

// ----------------------------------------------------------------------------

type
{$Z1}
  TksOutboundMT = (REGISTER_COMMAND_APPLICATION = 1,
    UNREGISTER_COMMAND_APPLICATION = 9, REQUEST_ENTRY_LIST = 10,
    REQUEST_TRACK_DATA = 11, CHANGE_HUD_PAGE = 49, CHANGE_FOCUS = 50,
    INSTANT_REPLAY_REQUEST = 51, PLAY_MANUAL_REPLAY_HIGHLIGHT = 52,
    SAVE_MANUAL_REPLAY_HIGHLIGHT = 60);

  TksInboundMT = (REGISTRATION_RESULT = 1, REALTIME_UPDATE = 2,
    REALTIME_CAR_UPDATE = 3, ENTRY_LIST = 4, ENTRY_LIST_CAR = 6, TRACK_DATA = 5,
    BROADCASTING_EVENT = 7);

  // --------------------------------------------------------------------------
  // TksBroadcastingMsgHandler
  // --------------------------------------------------------------------------

constructor TksBroadcastingMsgHandler.Create(msgDelegate: IksMessageDelegate);
begin
  if (msgDelegate <> nil) then
  begin
    FConnectionID := -1;
    lastEntrylistRequest := Now;
    FMessageDelegate := msgDelegate;
    FMsgTimestamp := 0;
    FRegReqTimestamp := FMsgTimestamp;
    FUpdateInterval := 0;
    RegRequestPending := false;
  end
  else
    raise Exception.Create('TksBroadcastingProtocol: message delegate is null');
end;

destructor TksBroadcastingMsgHandler.Destroy;
begin
  Unregister;
end;

// ---- INBOUND MESSAGES

procedure TksBroadcastingMsgHandler.ProcessMessage;
var
  inStrm: TStream;

  procedure Process_REGISTRATION_RESULT;
  var
    result: TKsRegistrationResult;
  begin
    result.readFromStream(inStrm);
    if (result.Success) then
      FConnectionID := result.connectionId
    else
      FConnectionID := -1;
    RegRequestPending := false;
    Msg(result);
  end;

  procedure Process_REALTIME_UPDATE;
  var
    sessionData: TksSessionData;
  begin
    sessionData.readFromStream(inStrm);
    Msg(sessionData);
  end;

  procedure Process_REALTIME_CAR_UPDATE;
  var
    carData: TksCarData;
  begin
    carData.readFromStream(inStrm);
    Msg(carData);
  end;

  procedure Process_ENTRY_LIST;
  var
    connId: Int32;
    carEntryCount: UInt16;
  begin
    inStrm.ReadBuffer(connId, sizeof(connId));
    if (connId = FConnectionID) then
    begin
      inStrm.ReadBuffer(carEntryCount, sizeof(carEntryCount));
      Msg(carEntryCount);
    end;
  end;

  procedure Process_ENTRY_LIST_CAR;
  var
    carInfo: TKsCarInfo;
  begin
    carInfo := TKsCarInfo.Create;
    carInfo.readFromStream(inStrm);
    Msg(carInfo);
  end;

  procedure Process_TRACK_DATA;
  var
    trackData: TksTrackData;
    connId: integer;
  begin
    inStrm.ReadBuffer(connId, sizeof(connId));
    if (connId = FConnectionID) then
    begin
      trackData := TksTrackData.Create;
      trackData.readFromStream(inStrm);
      Msg(trackData);
    end;
  end;

  procedure Process_BROADCASTING_EVENT;
  var
    evt: TksBroadcastingEvent;
  begin
    evt.readFromStream(inStrm);
    Msg(evt);
  end;

var
  msgType: TksInboundMT;
begin
  inStrm := MessageDelegate.ReceiveFrom;
  FMsgTimestamp := Now;
  try
    if (inStrm <> nil) and (inStrm.Size > 0) then
    begin
      inStrm.ReadBuffer(msgType, sizeof(msgType));
      case msgType of
        TksInboundMT.REGISTRATION_RESULT:
          Process_REGISTRATION_RESULT;
        TksInboundMT.REALTIME_UPDATE:
          Process_REALTIME_UPDATE;
        TksInboundMT.REALTIME_CAR_UPDATE:
          Process_REALTIME_CAR_UPDATE;
        TksInboundMT.ENTRY_LIST:
          Process_ENTRY_LIST;
        TksInboundMT.ENTRY_LIST_CAR:
          Process_ENTRY_LIST_CAR;
        TksInboundMT.TRACK_DATA:
          Process_TRACK_DATA;
        TksInboundMT.BROADCASTING_EVENT:
          Process_BROADCASTING_EVENT;
      end;
    end
  finally
    inStrm.Free;
  end;
end;

// ---- AUXILIARY

function TksBroadcastingMsgHandler.GetRegistered: boolean;
begin
  result := (FConnectionID >= 0);
end;

function TksBroadcastingMsgHandler.GetRegistrationRequestTimedOut: boolean;
begin
  result := (FConnectionID < 0) and RegRequestPending and
    (MillisecondsBetween(Now, FRegReqTimestamp) >= (4 * UpdateIntervalMs));
end;

function TksBroadcastingMsgHandler.IsServerInactive(timeMs: Int64): boolean;{
  PURPOUSE:
  Check if server has been inactive (no message received) in the given amount
  of time.

  PARAMETERS:
  timeMs: amount of time in milliseconds.

  RESULT:
  True if no message has been received in the last "timeMs" milliseconds.
  False otherwise.
}
begin
  result := (FConnectionID >= 0) and
    (MillisecondsBetween(Now, FMsgTimestamp) >= timeMs);
end;

// ---- OUTBOUND MESSAGES

function TksBroadcastingMsgHandler.Register(const displayName: string;
  const connectionPassword: string; const msUpdateInterval: Int32;
  const commandPassword: string): boolean;
{
  PURPOUSE:
  To send a registration message.
  Destination is left to the message delegate by calling "Start".

  PARAMETERS:
  Those required by the API

  RESULT:
  False if already registered, true otherwise.
}
const
  version: BYTE = BROADCASTING_PROTOCOL_VERSION;
  Msg: TksOutboundMT = TksOutboundMT.REGISTER_COMMAND_APPLICATION;
var
  outStrm: TBytesStream;
begin
  result := (FConnectionID < 0);
  if (result) then
  begin
    outStrm := TBytesStream.Create;
    try
      FMessageDelegate.Start;
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(version, sizeof(version));
      WriteString(outStrm, displayName);
      WriteString(outStrm, connectionPassword);
      outStrm.WriteBuffer(msUpdateInterval, sizeof(msUpdateInterval));
      WriteString(outStrm, commandPassword);
      RegRequestPending := true;
      FRegReqTimestamp := Now;
      FUpdateInterval := msUpdateInterval;
      MessageDelegate.SendTo(outStrm);
    finally
      outStrm.Free;
    end;
  end;
end;

function TksBroadcastingMsgHandler.Unregister: boolean;
{
  PURPOUSE:
  Ask server to stop sending messages.

  RESULT:
  False if not registered, true otherwise.
}
const
  Msg: TksOutboundMT = TksOutboundMT.UNREGISTER_COMMAND_APPLICATION;
var
  outStrm: TBytesStream;
begin
  RegRequestPending := false;
  result := (FConnectionID >= 0);
  if (result) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
      MessageDelegate.SendTo(outStrm);
      FConnectionID := -1;
      FRegReqTimestamp := 0;
      FUpdateInterval := 0;
    finally
      outStrm.Free;
    end;
  end;
  FMessageDelegate.Stop;
end;

procedure TksBroadcastingMsgHandler.RequestEntryList(const force: boolean);
const
  Msg: TksOutboundMT = TksOutboundMT.REQUEST_ENTRY_LIST;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID >= 0) then
    if (force) or (SecondsBetween(Now, lastEntrylistRequest) > 1) then
    begin
      outStrm := TBytesStream.Create;
      try
        outStrm.WriteBuffer(Msg, sizeof(Msg));
        outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
        MessageDelegate.SendTo(outStrm);
        lastEntrylistRequest := Now;
      finally
        outStrm.Free;
      end;
    end;
end;

procedure TksBroadcastingMsgHandler.RequestTrackData;
const
  Msg: TksOutboundMT = TksOutboundMT.REQUEST_TRACK_DATA;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID >= 0) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
      MessageDelegate.SendTo(outStrm);
    finally
      outStrm.Free;
    end;
  end;
end;

procedure TksBroadcastingMsgHandler.RequestFocus(const carIndex: UInt16;
  const cameraSet: string; const camera: string);
const
  Msg: TksOutboundMT = TksOutboundMT.CHANGE_FOCUS;
  aux0: BYTE = 0;
  aux1: BYTE = 1;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID >= 0) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
      outStrm.WriteBuffer(carIndex, sizeof(carIndex));
      if (carIndex < High(carIndex)) then
      begin
        outStrm.WriteBuffer(aux1, sizeof(aux1));
        outStrm.WriteBuffer(carIndex, sizeof(carIndex));
      end
      else
        outStrm.WriteBuffer(aux0, sizeof(aux0));
      if (cameraSet <> '') and (camera <> '') then
      begin
        outStrm.WriteBuffer(aux1, sizeof(aux1));
        WriteString(outStrm, cameraSet);
        WriteString(outStrm, camera);
      end
      else
        outStrm.WriteBuffer(aux0, sizeof(aux0));
      MessageDelegate.SendTo(outStrm);
    finally
      outStrm.Free;
    end;
  end;
end;

procedure TksBroadcastingMsgHandler.RequestFocus(const cameraSet,
  camera: string);
begin
  RequestFocus(High(UInt16), cameraSet, camera);
end;

procedure TksBroadcastingMsgHandler.RequestInstantReplay(const startSessionTime,
  durationMS: Single; const initialFocusedCarIndex: Int32 = -1;
  const initialCameraSet: string = ''; const initialCamera: string = '');
const
  Msg: TksOutboundMT = TksOutboundMT.INSTANT_REPLAY_REQUEST;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID >= 0) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
      outStrm.WriteBuffer(startSessionTime, sizeof(startSessionTime));
      outStrm.WriteBuffer(durationMS, sizeof(durationMS));
      outStrm.WriteBuffer(initialFocusedCarIndex,
        sizeof(initialFocusedCarIndex));
      WriteString(outStrm, initialCameraSet);
      WriteString(outStrm, initialCamera);
      MessageDelegate.SendTo(outStrm);
    finally
      outStrm.Free;
    end;
  end;
end;

procedure TksBroadcastingMsgHandler.RequestHUDPage(const HUDPage: string);
const
  Msg: TksOutboundMT = TksOutboundMT.CHANGE_HUD_PAGE;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID >= 0) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
      WriteString(outStrm, HUDPage);
      MessageDelegate.SendTo(outStrm);
    finally
      outStrm.Free;
    end;
  end;
end;

// --------------------------------------------------------------------------
// TksThreadedBroadcastingMsgHandler
// --------------------------------------------------------------------------

constructor TksThreadedBroadcastingMsgHandler.Create
  (msgDelegate: IksMessageDelegate);
begin
  ongoing := TEvent.Create(nil, true, false, '');
  cancelBackgroundTasks := false;
  listener := TTask.Create(ReceiveTask);
  listener.Start;
  registrationSpotter := TTask.Create(RegistrationTask);
  registrationSpotter.Start;
  FDisplayName := '';
  FConnectionPassword := '';
  FCommandPassword := '';
  FMaxServerInactivityMs := High(FMaxServerInactivityMs);
  inherited Create(msgDelegate);
end;

destructor TksThreadedBroadcastingMsgHandler.Destroy;
begin
  cancelBackgroundTasks := true;
  ongoing.SetEvent;
  inherited;
  listener.Wait;
  registrationSpotter.Wait;
  ongoing.Free;
end;

procedure TksThreadedBroadcastingMsgHandler.ReceiveTask;
begin
  repeat
    try
      ongoing.WaitFor;
      if (not cancelBackgroundTasks) then
      begin
        ProcessMessage;
        serverActivityFlag := true;
      end;
    except
    end;
  until (cancelBackgroundTasks);
end;

procedure TksThreadedBroadcastingMsgHandler.RegistrationTask;
begin
  repeat
    try
      ongoing.WaitFor;
      if (not cancelBackgroundTasks) and RegistrationRequestTimedOut then
      begin
        RetryRegistrationRequest;
        sleep(UpdateIntervalMs * 3);
      end
      else if (not cancelBackgroundTasks) and serverActivityFlag and
        IsServerInactive(FMaxServerInactivityMs) then
      begin
        serverActivityFlag := false;
        NotifyNoServerActivity;
      end;
    except
    end;
  until (cancelBackgroundTasks);
end;

procedure TksThreadedBroadcastingMsgHandler.Start(const displayName: string;
  const connectionPassword: string; const msUpdateInterval: Int32;
  const commandPassword: string);
begin
  Stop;
  FDisplayName := displayName;
  FConnectionPassword := connectionPassword;
  FCommandPassword := commandPassword;
  serverActivityFlag := true;
  if (FMaxServerInactivityMs <= (msUpdateInterval * 2)) then
    FMaxServerInactivityMs := msUpdateInterval * 3;
  BeforeRegister;
  inherited Register(displayName, connectionPassword, msUpdateInterval,
    commandPassword);
  ongoing.SetEvent;
end;

procedure TksThreadedBroadcastingMsgHandler.Stop;
begin
  if (inherited Unregister) then
    AfterUnregister;
  ongoing.ResetEvent;
  FDisplayName := '';
  FConnectionPassword := '';
  FCommandPassword := '';
end;

procedure TksThreadedBroadcastingMsgHandler.BeforeRegister;
begin
  // Do nothing
end;

procedure TksThreadedBroadcastingMsgHandler.AfterUnregister;
begin
  // Do nothing
end;

procedure TksThreadedBroadcastingMsgHandler.RetryRegistrationRequest;
begin
  inherited Register(FDisplayName, FConnectionPassword, UpdateIntervalMs,
    FCommandPassword);
end;

procedure TksThreadedBroadcastingMsgHandler.NotifyNoServerActivity;
begin
  // Do nothing
end;

// ----------------------------------------------------------------------------

end.
