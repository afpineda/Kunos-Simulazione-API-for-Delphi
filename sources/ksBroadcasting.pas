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

  ******************************************************* }

interface

uses
  System.Classes,
  ksBroadcasting.Data;

{
  SUMMARY:

  - IksMessageDelegate: interface to implementa data transmision
  - TksBroadcastingMsgHandler: Message handler for broadcasting protocol
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
      Called once at protocol instantiation. Should perform some kind of
      initialization, like socket creation and binding.

      STOP:
      Called once at protocol instace destruction. Should unblock any pending
      thread at "ReceiveFrom". For example, by closing a socket.

      SendTo:
      Send datagram. Non-blocking.

      ReceiveFrom:
      Receive datagram. Blocking. Caller should destroy received stream.
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
      destroyed by the descendant class.
    }
  public const
    BROADCASTING_PROTOCOL_VERSION = 4;
  private
    FConnectionID: Int32;
    FMessageDelegate: IksMessageDelegate;
    lastEntrylistRequest: TDateTime;
    function GetRegistered: boolean;
  protected
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
    property connectionId: Int32 read FConnectionID;
    property MessageDelegate: IksMessageDelegate read FMessageDelegate;
  public
    constructor Create(msgDelegate: IksMessageDelegate);
    procedure Register(const displayName: string;
      const connectionPassword: string; const msUpdateInterval: Int32 = 1000;
      const commandPassword: string = '');
    procedure Unregister;
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
  end
  else
    raise Exception.Create('TksBroadcastingProtocol: message delegate is null');
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

// ---- OUTBOUND MESSAGES

procedure TksBroadcastingMsgHandler.Register(const displayName: string;
  const connectionPassword: string; const msUpdateInterval: Int32;
  const commandPassword: string);
const
  version: BYTE = BROADCASTING_PROTOCOL_VERSION;
  Msg: TksOutboundMT = TksOutboundMT.REGISTER_COMMAND_APPLICATION;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID < 0) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(Msg, sizeof(Msg));
      outStrm.WriteBuffer(version, sizeof(version));
      WriteString(outStrm, displayName);
      WriteString(outStrm, connectionPassword);
      outStrm.WriteBuffer(msUpdateInterval, sizeof(msUpdateInterval));
      WriteString(outStrm, commandPassword);
      MessageDelegate.SendTo(outStrm);
    finally
      outStrm.Free;
    end;
  end;
end;

procedure TksBroadcastingMsgHandler.Unregister;
const
  Msg: TksOutboundMT = TksOutboundMT.UNREGISTER_COMMAND_APPLICATION;
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
      FConnectionID := -1;
    finally
      outStrm.Free;
    end;
  end;
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


// ----------------------------------------------------------------------------

end.
