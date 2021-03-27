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
  ksBroadcasting.Data,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections;

type
  TksBroadcastingProtocol = class
  public const
    BROADCASTING_PROTOCOL_VERSION = 4;
  private
    FEntryList: TObjectList<TksCarInfo>;
    FConnectionID: Int32;
    FExpectedEntryListCount: UInt16;
    lastEntrylistRequest: TDateTime;
    function GetIsConnected: boolean;
  protected
    procedure debugInvalidMessage(stream: TBytesStream); virtual;
    function ReceiveMessage: TBytesStream; virtual; abstract;
    procedure ProcessMessage;
    procedure RequestConnection(const displayName: string;
      const connectionPassword: string; const msUpdateInterval: Int32;
      const commandPassword: string);
    procedure RequestDisconnect;
    procedure RequestInstantReplay(const startSessionTime, durationMS: Single;
      const initialFocusedCarIndex: Int32 = -1;
      const initialCameraSet: string = ''; const initialCamera: string = '');
    procedure SendMessage(const outStrm: TBytesStream); virtual; abstract;
    procedure doOnBroadcastingEvent(const event: TksBroadcastingEvent);
      virtual; abstract;
    procedure doOnEntryListCar(const carInfo: TksCarInfo); virtual;
    procedure doOnEntryListComplete; virtual;
    procedure doOnNewEntryList(const entryCount: WORD); virtual;
    procedure doOnRealTimeCarUpdate(const carData: TksCarData);
      virtual; abstract;
    procedure doOnRealTimeUpdate(const sessionData: TksSessionData);
      virtual; abstract;
    procedure doOnRegistrationResult(const connectionId: Int32;
      const success, isReadOnly: boolean; const errMsg: string);
      virtual; abstract;
    procedure doOnTrackData(const trackData: TksTrackData); virtual; abstract;
    property InternalEntryList: TObjectList<TksCarInfo> read FEntryList;
    property connectionId: Int32 read FConnectionID;
    property isConnected: boolean read GetIsConnected;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RequestEntryList(const force: boolean = false);
    procedure RequestFocus(const carIndex: UInt16; const cameraSet: string = '';
      const camera: string = ''); overload;
    procedure RequestFocus(const carRaceNumber: Integer;
      const cameraSet: string = ''; const camera: string = ''); overload;
    procedure RequestFocus(const cameraSet, camera: string); overload;
    procedure RequestHUDPage(const HUDPage: string);
    procedure RequestTrackData;
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

constructor TksBroadcastingProtocol.Create;
begin
  FEntryList := TObjectList<TksCarInfo>.Create;
  FEntryList.OwnsObjects := true;
  FConnectionID := -1;
  lastEntrylistRequest := Now;
end;

destructor TksBroadcastingProtocol.Destroy;
begin
  FEntryList.Free;
  inherited;
end;

// --------------------------------------------------------------------------
// INBOUND MESSAGES
// --------------------------------------------------------------------------

procedure TksBroadcastingProtocol.ProcessMessage;
var
  inStrm: TBytesStream;

  procedure Process_REGISTRATION_RESULT;
  var
    isReadOnly: BYTE;
    success: BYTE;
    errMsg: string;
  begin
    inStrm.ReadBuffer(FConnectionID, sizeof(FConnectionID));
    inStrm.ReadBuffer(success, sizeof(isReadOnly));
    inStrm.ReadBuffer(isReadOnly, sizeof(isReadOnly));
    errMsg := ReadString(inStrm);
    if (success <> 0) then
    begin
      RequestEntryList(true);
      RequestTrackData;
    end
    else
      FConnectionID := -1;
    doOnRegistrationResult(FConnectionID, (success <> 0),
      (isReadOnly = 0), errMsg);
  end;

  procedure Process_REALTIME_UPDATE;
  var
    sessionData: TksSessionData;
  begin
    sessionData.readFromStream(inStrm);
    doOnRealTimeUpdate(sessionData);
  end;

  procedure Process_REALTIME_CAR_UPDATE;
  var
    carInfo: TksCarInfo;
    carData: TksCarData;
  begin
    carData.readFromStream(inStrm);
    carInfo := TksCarInfo.findCarInfo(FEntryList, carData.carIndex);
    if (carInfo = nil) or (FEntryList.Count <> carData.driverCount) then
      RequestEntryList
    else
      doOnRealTimeCarUpdate(carData);
  end;

  procedure Process_ENTRY_LIST;
  var
    connId, i: Int32;
    carEntryCount: UInt16;
    carIdx: UInt16;
  begin
    inStrm.ReadBuffer(connId, sizeof(connId));
    inStrm.ReadBuffer(carEntryCount, sizeof(carEntryCount));
    FEntryList.Clear;
    for i := 0 to carEntryCount - 1 do
    begin
      inStrm.ReadBuffer(carIdx, sizeof(carIdx));
      FEntryList.Add(TksCarInfo.Create(carIdx));
    end;
    FExpectedEntryListCount := 0;
    doOnNewEntryList(carEntryCount);
  end;

  procedure Process_ENTRY_LIST_CAR;
  var
    carInfo: TksCarInfo;
    carIdx: UInt16;
  begin
    inStrm.ReadBuffer(carIdx, sizeof(carIdx));
    carInfo := TksCarInfo.findCarInfo(FEntryList, carIdx);
    if (carInfo <> nil) then
    begin
      if (carInfo.CarModelType = 0) and (carInfo.CarModelType = 0) and
        (carInfo.TeamName = '') then
        inc(FExpectedEntryListCount);
      carInfo.readFromStream(inStrm);
      doOnEntryListCar(carInfo);
      if (FExpectedEntryListCount = FEntryList.Count) then
      begin
        // avoid repetitive events
        FExpectedEntryListCount := High(FExpectedEntryListCount);
        // Notify event
        doOnEntryListComplete;
      end;
    end
    else
      RequestEntryList;
  end;

  procedure Process_TRACK_DATA;
  var
    trackData: TksTrackData;
    connId: Integer;
  begin
    inStrm.ReadBuffer(connId, sizeof(connId));
    trackData := TksTrackData.Create;
    trackData.readFromStream(inStrm);
    doOnTrackData(trackData);
  end;

  procedure Process_BROADCASTING_EVENT;
  var
    evt: TksBroadcastingEvent;
  begin
    evt.readFromStream(inStrm);
    doOnBroadcastingEvent(evt);
  end;

var
  msgType: TksInboundMT;
begin
  inStrm := ReceiveMessage;
  if (inStrm <> nil) and (inStrm.Size > 0) then
    try
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
    finally
      inStrm.Free;
    end
  else
  begin
{$IFDEF DEBUG }
    debugInvalidMessage(inStrm);
{$ENDIF}
    inStrm.Free;
  end;
end;

// --------------------------------------------------------------------------
// AUXILIARY
// --------------------------------------------------------------------------

function TksBroadcastingProtocol.GetIsConnected: boolean;
begin
  Result := (FConnectionID >= 0);
end;

// --------------------------------------------------------------------------
// OUTBOUND MESSAGES
// --------------------------------------------------------------------------

procedure TksBroadcastingProtocol.RequestConnection(const displayName: string;
  const connectionPassword: string; const msUpdateInterval: Int32;
  const commandPassword: string);
const
  version: BYTE = BROADCASTING_PROTOCOL_VERSION;
  msg: TksOutboundMT = TksOutboundMT.REGISTER_COMMAND_APPLICATION;
var
  outStrm: TBytesStream;
begin
  outStrm := TBytesStream.Create;
  try
    outStrm.WriteBuffer(msg, sizeof(msg));
    outStrm.WriteBuffer(version, sizeof(version));
    WriteString(outStrm, displayName);
    WriteString(outStrm, connectionPassword);
    outStrm.WriteBuffer(msUpdateInterval, sizeof(msUpdateInterval));
    WriteString(outStrm, commandPassword);
    SendMessage(outStrm);
  finally
    outStrm.Free;
  end;
end;

procedure TksBroadcastingProtocol.RequestDisconnect;
const
  msg: TksOutboundMT = TksOutboundMT.UNREGISTER_COMMAND_APPLICATION;
var
  outStrm: TBytesStream;
begin
  if (FConnectionID >= 0) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(msg, sizeof(msg));
      SendMessage(outStrm);
      FConnectionID := -1;
    finally
      outStrm.Free;
    end;
  end;
end;

procedure TksBroadcastingProtocol.RequestEntryList(const force: boolean);
const
  msg: TksOutboundMT = TksOutboundMT.REQUEST_ENTRY_LIST;
var
  outStrm: TBytesStream;
begin
  if (force) or (SecondsBetween(Now, lastEntrylistRequest) > 1) then
  begin
    outStrm := TBytesStream.Create;
    try
      outStrm.WriteBuffer(msg, sizeof(msg));
      outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
      SendMessage(outStrm);
      lastEntrylistRequest := Now;
    finally
      outStrm.Free;
    end;
  end;
end;

procedure TksBroadcastingProtocol.RequestTrackData;
const
  msg: TksOutboundMT = TksOutboundMT.REQUEST_TRACK_DATA;
var
  outStrm: TBytesStream;
begin
  outStrm := TBytesStream.Create;
  try
    outStrm.WriteBuffer(msg, sizeof(msg));
    outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
    SendMessage(outStrm);
  finally
    outStrm.Free;
  end;
end;

procedure TksBroadcastingProtocol.RequestFocus(const carIndex: UInt16;
  const cameraSet: string; const camera: string);
const
  msg: TksOutboundMT = TksOutboundMT.CHANGE_FOCUS;
  aux0: BYTE = 0;
  aux1: BYTE = 1;
var
  outStrm: TBytesStream;
begin
  outStrm := TBytesStream.Create;
  try
    outStrm.WriteBuffer(msg, sizeof(msg));
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
    SendMessage(outStrm);
  finally
    outStrm.Free;
  end;
end;

procedure TksBroadcastingProtocol.RequestFocus(const carRaceNumber: Integer;
  const cameraSet: string = ''; const camera: string = '');
var
  carInfo: TksCarInfo;
begin
  carInfo := TksCarInfo.findRaceNumber(InternalEntryList, carRaceNumber);
  if (carInfo <> nil) then
    RequestFocus(UInt16(carInfo.carIndex), cameraSet, camera);
end;

procedure TksBroadcastingProtocol.RequestFocus(const cameraSet, camera: string);
begin
  RequestFocus(High(UInt16), cameraSet, camera);
end;

procedure TksBroadcastingProtocol.RequestInstantReplay(const startSessionTime,
  durationMS: Single; const initialFocusedCarIndex: Int32 = -1;
  const initialCameraSet: string = ''; const initialCamera: string = '');
const
  msg: TksOutboundMT = TksOutboundMT.INSTANT_REPLAY_REQUEST;
var
  outStrm: TBytesStream;
begin
  outStrm := TBytesStream.Create;
  try
    outStrm.WriteBuffer(msg, sizeof(msg));
    outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
    outStrm.WriteBuffer(startSessionTime, sizeof(startSessionTime));
    outStrm.WriteBuffer(durationMS, sizeof(durationMS));
    outStrm.WriteBuffer(initialFocusedCarIndex, sizeof(initialFocusedCarIndex));
    WriteString(outStrm, initialCameraSet);
    WriteString(outStrm, initialCamera);
    SendMessage(outStrm);
  finally
    outStrm.Free;
  end;
end;

procedure TksBroadcastingProtocol.RequestHUDPage(const HUDPage: string);
const
  msg: TksOutboundMT = TksOutboundMT.CHANGE_HUD_PAGE;
var
  outStrm: TBytesStream;
begin
  outStrm := TBytesStream.Create;
  try
    outStrm.WriteBuffer(msg, sizeof(msg));
    outStrm.WriteBuffer(FConnectionID, sizeof(FConnectionID));
    WriteString(outStrm, HUDPage);
    SendMessage(outStrm);
  finally
    outStrm.Free;
  end;
end;

// --------------------------------------------------------------------------
// DEFAULT EVENT HANDLERS
// --------------------------------------------------------------------------

procedure TksBroadcastingProtocol.doOnNewEntryList(const entryCount: WORD);
begin
  // Do nothing. Override if needed.
end;

procedure TksBroadcastingProtocol.doOnEntryListComplete;
begin
  // Do nothing. Override if needed.
end;

procedure TksBroadcastingProtocol.doOnEntryListCar(const carInfo: TksCarInfo);
begin
  // Do nothing. Override if needed.
end;

procedure TksBroadcastingProtocol.debugInvalidMessage(stream: TBytesStream);
begin
  // Do nothing. Override if needed.
end;

// ----------------------------------------------------------------------------

end.
