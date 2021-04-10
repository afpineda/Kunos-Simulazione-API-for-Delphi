unit ksBroadcasting.Protocol;

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

  [2021-04-10] Reworked

  ******************************************************* }

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  System.SyncObjs,
  ksBroadcasting.Leaderboard,
  ksBroadcasting.Data,
  ksBroadcasting;

type
  TksBroadcastingProtocol = class(TksThreadedBroadcastingMsgHandler)
    {
      PURPOUSE:
      To implement a broadcasting client.

      GENERAL USAGE:
      - Create a message delegate (IksMessageDelegate)
      - Create TksBroadcastingProtocol instance.
      - Assign "On*" event handlers
      - Call "Start"

      EVENTS:
      Event handlers are called at the main thread if synchronizedEvents
      was set to true at instance creation. Otherwise, they are
      called at a separate thread.
      Any object (not record) passed to an event handler should be
      destroyed if not used. Take care.

      "OnEntryList" may be called with EntryList=nil. This happens
      when the entry list has been cleared and is about to change.

      MESSAGE HANDLING:
      A separate thread is spawn. It will attend the incoming messages.

      OTHER NOTES:
      Not connection-oriented. "Register" does not mean "connect to".
      Any outgoing message may have no answer, including "Register".
      Incoming messages may be received out of order.
    }
  public type
    TOnRegistrationEvent = procedure(Sender: TksBroadcastingProtocol;
      const result: TKsRegistrationResult) of object;
    TOnEntryListEvent = procedure(Sender: TksBroadcastingProtocol;
      EntryList: TKsEntryList) of object;
    TOnTrackDataEvent = procedure(Sender: TksBroadcastingProtocol;
      trackData: TksTrackData) of object;
    TOnCarDataEvent = procedure(Sender: TksBroadcastingProtocol;
      const carInfo: TKsCarData) of object;
    TOnSessionDataEvent = procedure(Sender: TksBroadcastingProtocol;
      const current, previous: TKsSessionData) of object;
    TOnBroadcastingEventEvent = procedure(Sender: TksBroadcastingProtocol;
      const event: TksBroadcastingEvent) of object;
    TOnLeaderboardUpdateEvent = procedure(Sender: TksBroadcastingProtocol;
      Leaderboard: TksLeaderboard) of object;
  private
    FCurrentLeaderBoard: TksCarDataList;
    FCurrentSessionData: TKsSessionData;
    FDoSync: boolean;
    FEntryList: TKsEntryList;
    FLastLeaderboardCount: integer;
    FOnRegistration: TOnRegistrationEvent;
    FOnEntryList: TOnEntryListEvent;
    FOnTrackData: TOnTrackDataEvent;
    FOnLeaderboardUpdate: TOnLeaderboardUpdateEvent;
    FOnCarData: TOnCarDataEvent;
    FOnSessionData: TOnSessionDataEvent;
    FOnBroadcastingEvent: TOnBroadcastingEventEvent;
    FOnServerInactivity: TNotifyEvent;
    expectedEntryCount: integer;
  protected
    function NotifyBroadcastingEvent(const event: TksBroadcastingEvent)
      : boolean; virtual;
    function NotifyCarData(const carData: TKsCarData): boolean; virtual;
    function NotifyEntryList(newEntryList: TKsEntryList): boolean; virtual;
    function NotifyLeaderBoard: boolean; virtual;
    function NotifyRegistrationResult(const regResult: TKsRegistrationResult)
      : boolean; virtual;
    function NotifySessionData(const sessionData: TKsSessionData)
      : boolean; virtual;
    function NotifyTrackData(const trackData: TksTrackData): boolean; virtual;
    procedure NotifyNoServerActivity; override;
    procedure Msg(const result: TKsRegistrationResult); overload; override;
    procedure Msg(const sessionData: TKsSessionData); overload; override;
    procedure Msg(const carData: TKsCarData); overload; override;
    procedure Msg(const carInfo: TKsCarInfo); overload; override;
    procedure Msg(const carEntryCount: integer); overload; override;
    procedure Msg(const trackData: TksTrackData); overload; override;
    procedure Msg(const event: TksBroadcastingEvent); overload; override;
  public
    constructor Create(msgDelegate: IksMessageDelegate;
      const synchronizedEvents: boolean = true);
    destructor Destroy; override;
    procedure RequestFocusOnRaceNumber(const carRaceNumber: integer;
      const cameraSet: string = ''; const camera: string = ''); overload;
    property OnBroadcastingEvent: TOnBroadcastingEventEvent
      read FOnBroadcastingEvent write FOnBroadcastingEvent;
    property OnCarData: TOnCarDataEvent read FOnCarData write FOnCarData;
    property OnEntryList: TOnEntryListEvent read FOnEntryList
      write FOnEntryList;
    property OnLeaderboardUpdate: TOnLeaderboardUpdateEvent
      read FOnLeaderboardUpdate write FOnLeaderboardUpdate;
    property OnRegistration: TOnRegistrationEvent read FOnRegistration
      write FOnRegistration;
    property OnSessionData: TOnSessionDataEvent read FOnSessionData
      write FOnSessionData;
    property OnServerInactivity: TNotifyEvent read FOnServerInactivity
      write FOnServerInactivity;
    property OnTrackData: TOnTrackDataEvent read FOnTrackData
      write FOnTrackData;
  end;

implementation

uses
  System.SysUtils;

// ----------------------------------------------------------------------------
// Create/destroy
// ----------------------------------------------------------------------------

constructor TksBroadcastingProtocol.Create(msgDelegate: IksMessageDelegate;
  const synchronizedEvents: boolean = true);
begin
  inherited Create(msgDelegate);
  FCurrentLeaderBoard := TksCarDataList.Create;
  FDoSync := synchronizedEvents;
  FOnRegistration := nil;
  FOnEntryList := nil;
  FOnTrackData := nil;
  FOnCarData := nil;
  FOnSessionData := nil;
  FOnBroadcastingEvent := nil;
  FOnServerInactivity := nil;
  FOnLeaderboardUpdate := nil;
  FCurrentSessionData.Reset;
  FLastLeaderboardCount := 0;

  FEntryList := TKsEntryList.Create;
  expectedEntryCount := 0;
end;

destructor TksBroadcastingProtocol.Destroy;
begin
  inherited;
  FEntryList.Free;
end;

// ----------------------------------------------------------------------------
// Create/destroy
// ----------------------------------------------------------------------------

procedure TksBroadcastingProtocol.NotifyNoServerActivity;
begin
  if Assigned(FOnServerInactivity) then
  begin
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnServerInactivity(self);
        end)
    else
    begin
      FOnServerInactivity(self);
    end;
  end;
end;

// ----------------------------------------------------------------------------
// overriden "Msg" methods
// ----------------------------------------------------------------------------

procedure TksBroadcastingProtocol.Msg(const result: TKsRegistrationResult);
begin
  if result.success then
  begin
    RequestEntryList(true);
    RequestTrackData;
  end;
  NotifyRegistrationResult(result);
end;

procedure TksBroadcastingProtocol.Msg(const carEntryCount: integer);
begin
  FEntryList.Clear;
  expectedEntryCount := carEntryCount;
  NotifyEntryList(nil);
end;

procedure TksBroadcastingProtocol.Msg(const carInfo: TKsCarInfo);
var
  complete: boolean;
  notFound: boolean;
  newEntryList: TKsEntryList;
begin
  notFound := not FEntryList.ContainsKey(carInfo.carIndex);
  if (expectedEntryCount > 0) and (notFound) then
  begin
    FEntryList.Add(carInfo.carIndex, carInfo);
    dec(expectedEntryCount);
    complete := (expectedEntryCount = 0);
    if (complete and Assigned(FOnEntryList)) then
    begin
      newEntryList := TKsEntryList.Create(FEntryList);
      NotifyEntryList(newEntryList);
    end;
  end
  else
  begin
    carInfo.Free;
    if (notFound) then
      RequestEntryList;
  end;
end;

procedure TksBroadcastingProtocol.Msg(const trackData: TksTrackData);
begin
  if (not NotifyTrackData(trackData)) then
    trackData.Free;
end;

procedure TksBroadcastingProtocol.Msg(const sessionData: TKsSessionData);
begin
  NotifySessionData(sessionData);
  FCurrentSessionData := sessionData;
end;

procedure TksBroadcastingProtocol.Msg(const carData: TKsCarData);
var
  carInfo: TKsCarInfo;
begin
  if (expectedEntryCount = 0) then
  begin
    carInfo := FEntryList.findIndex(carData.carIndex);
    if ((carInfo = nil) or (carInfo.Drivers.Count <> carData.DriverCount)) then
      RequestEntryList
    else
    begin
      NotifyCarData(carData);

      if (FCurrentLeaderBoard.ContainsKey(carData.carIndex)) then
      begin
        // Leaderboard completed
        if (FLastLeaderboardCount > FCurrentLeaderBoard.Count) then
          // someone left the server
          RequestEntryList;
        FLastLeaderboardCount := FCurrentLeaderBoard.Count;
        NotifyLeaderBoard;
        FCurrentLeaderBoard.Clear;
      end;
      FCurrentLeaderBoard.Add(carData.carIndex, carData);
    end;
  end;
end;

procedure TksBroadcastingProtocol.Msg(const event: TksBroadcastingEvent);
begin
  NotifyBroadcastingEvent(event);
end;

// ----------------------------------------------------------------------------
// Other requests
// ----------------------------------------------------------------------------

procedure TksBroadcastingProtocol.RequestFocusOnRaceNumber(const carRaceNumber
  : integer; const cameraSet: string = ''; const camera: string = '');
var
  carInfo: TKsCarInfo;
begin
  if (ConnectionID >= 0) then
  begin
    carInfo := FEntryList.findRaceNumber(carRaceNumber);
    if (carInfo <> nil) then
      RequestFocus(carInfo.carIndex, cameraSet, camera);
  end;
end;

// ----------------------------------------------------------------------------
// Notifications
// ----------------------------------------------------------------------------

function TksBroadcastingProtocol.NotifyBroadcastingEvent
  (const event: TksBroadcastingEvent): boolean;
begin
  result := Assigned(FOnBroadcastingEvent);
  if result then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnBroadcastingEvent(self, event);
        end)
    else
      FOnBroadcastingEvent(self, event);
end;

function TksBroadcastingProtocol.NotifyCarData(const carData
  : TKsCarData): boolean;
begin
  result := Assigned(FOnCarData);
  if result then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnCarData(self, carData);
        end)
    else
      FOnCarData(self, carData);
end;

function TksBroadcastingProtocol.NotifyTrackData(const trackData
  : TksTrackData): boolean;
begin
  result := Assigned(FOnTrackData);
  if result then
  begin
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnTrackData(self, trackData);
        end)
    else
      FOnTrackData(self, trackData);
  end;
end;

function TksBroadcastingProtocol.NotifySessionData(const sessionData
  : TKsSessionData): boolean;
begin
  result := Assigned(FOnSessionData);
  if result then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnSessionData(self, sessionData, FCurrentSessionData);
        end)
    else
      FOnSessionData(self, sessionData, FCurrentSessionData);
end;

function TksBroadcastingProtocol.NotifyEntryList(newEntryList
  : TKsEntryList): boolean;
begin
  result := Assigned(FOnEntryList);
  if FDoSync then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnEntryList(self, newEntryList);
      end)
  else
    FOnEntryList(self, newEntryList);
end;

function TksBroadcastingProtocol.NotifyRegistrationResult(const regResult
  : TKsRegistrationResult): boolean;
begin
  result := Assigned(FOnRegistration);
  if result then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnRegistration(self, regResult);
        end)
    else
      FOnRegistration(self, regResult);
end;

function TksBroadcastingProtocol.NotifyLeaderBoard: boolean;
var
  lb: TksLeaderboard;
begin
  result := Assigned(FOnLeaderboardUpdate);
  if (result) then
  begin
    lb := TksLeaderboard.Create(FEntryList, FCurrentLeaderBoard,
      FCurrentSessionData);
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnLeaderboardUpdate(self, lb);
        end)
    else
      FOnLeaderboardUpdate(self, lb);
  end;
end;

// ----------------------------------------------------------------------------

end.
