unit ksBroadcasting.Protocol;

{ *******************************************************

  Implementation of Kunos Simulazione's broadcasting
  client API for Delphi

  *******************************************************

  This Delphi/pascal translation:
  (C) 2021. �ngel Fern�ndez Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-03-28] First implementation

  ******************************************************* }

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  ksBroadcasting.Data,
  ksBroadcasting;

type
  TksBroadcastingProtocol = class(TksBroadcastingMsgHandler)
    {
      PURPOUSE:
      To implement a broadcasting client.

      GENERAL USAGE:
      - Create a message delegate (IksMessageDelegate)
      - Create TksBroadcastingProtocol instance.
      - Assign "On*" event handlers
      - Call "Register"

      EVENTS:
      Events are called in the main thread if synchronizedEvents was set to
      true at instance creation. Otherwise, they are called from a separate
      thread. Any object passed to an event handler is not automatically
      destroyed. Take care, but there is an exception:
      when synchronizedEvents=false, TOnEntryListEvent should NOT destroy
      "EntryList".

      MESSAGE HANDLING:
      A separate thread is spawn. It will attend the incoming messages.

      OTHER NOTES:
      Not connection-oriented. "Register" does not mean "connect to".
      Any outcoming message may have no answer, including "Register".
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
      const sessionData: TKsSessionData) of object;
    TOnBroadcastingEventEvent = procedure(Sender: TksBroadcastingProtocol;
      const event: TksBroadcastingEvent) of object;
  private
    FDoSync: boolean;
    FEntryList: TKsEntryList;
    FOnRegistration: TOnRegistrationEvent;
    FOnEntryList: TOnEntryListEvent;
    FOnTrackData: TOnTrackDataEvent;
    FOnCarData: TOnCarDataEvent;
    FOnSessionData: TOnSessionDataEvent;
    FOnBroadcastingEvent: TOnBroadcastingEventEvent;
    expectedEntryCount: integer;
    listener: ITask;
    cancelListener: boolean;
  protected
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
    procedure RequestFocus(const carRaceNumber: integer;
      const cameraSet: string = ''; const camera: string = ''); overload;

    property OnBroadcastingEvent: TOnBroadcastingEventEvent
      read FOnBroadcastingEvent write FOnBroadcastingEvent;
    property OnCarData: TOnCarDataEvent read FOnCarData write FOnCarData;
    property OnRegistration: TOnRegistrationEvent read FOnRegistration
      write FOnRegistration;
    property OnEntryList: TOnEntryListEvent read FOnEntryList
      write FOnEntryList;
    property OnSessionData: TOnSessionDataEvent read FOnSessionData
      write FOnSessionData;
    property OnTrackData: TOnTrackDataEvent read FOnTrackData
      write FOnTrackData;

  end;

implementation

uses
  System.SysUtils;

constructor TksBroadcastingProtocol.Create(msgDelegate: IksMessageDelegate;
  const synchronizedEvents: boolean = true);
begin
  inherited Create(msgDelegate);
  FDoSync := synchronizedEvents;
  FOnRegistration := nil;
  FOnEntryList := nil;
  FOnTrackData := nil;
  FOnCarData := nil;
  FOnSessionData := nil;
  FOnBroadcastingEvent := nil;
  FEntryList := TKsEntryList.Create;
  expectedEntryCount := 0;
  MessageDelegate.Start;
  cancelListener := false;
  listener := TTask.Create(
    procedure
    begin
      while (not cancelListener) do
        try
          ProcessMessage;
        except
        end;
    end);
  listener.Start;
end;

destructor TksBroadcastingProtocol.Destroy;
begin
  cancelListener := true;
  MessageDelegate.Stop;
  listener.Wait;
  FEntryList.Free;
  inherited;
end;

procedure TksBroadcastingProtocol.Msg(const result: TKsRegistrationResult);
begin
  if result.success then
  begin
    RequestEntryList(true);
    RequestTrackData;
  end;
  if Assigned(FOnRegistration) then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnRegistration(self, result);
        end)
    else
      FOnRegistration(self, result);
end;

procedure TksBroadcastingProtocol.Msg(const carEntryCount: integer);
begin
  FEntryList.Clear;
  expectedEntryCount := carEntryCount;
  if Assigned(FOnEntryList) then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnEntryList(self, nil);
        end)
    else
      FOnEntryList(self, nil);
end;

procedure TksBroadcastingProtocol.Msg(const carInfo: TKsCarInfo);
var
  complete: boolean;
  notFound: boolean;
begin
  notFound := (FEntryList.findIndex(carInfo.carIndex) = nil);
  if (expectedEntryCount > 0) and (notFound) then
  begin
    FEntryList.Add(carInfo);
    dec(expectedEntryCount);
    complete := (expectedEntryCount = 0);
    if (complete and Assigned(FOnEntryList)) then
    begin
      FEntryList.Sort;
      if FDoSync then
        TThread.Synchronize(nil,
          procedure
          begin
            FOnEntryList(self, FEntryList.Duplicate);
          end)
      else
        FOnEntryList(self, FEntryList);
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
  if Assigned(FOnTrackData) then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnTrackData(self, trackData);
        end)
    else
      FOnTrackData(self, trackData);
end;

procedure TksBroadcastingProtocol.Msg(const sessionData: TKsSessionData);
begin
  if Assigned(FOnSessionData) then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnSessionData(self, sessionData);
        end)
    else
      FOnSessionData(self, sessionData);
end;

procedure TksBroadcastingProtocol.Msg(const carData: TKsCarData);
var
  carInfo: TKsCarInfo;
begin
  carInfo := FEntryList.findIndex(carData.carIndex);
  if (carInfo = nil) or (carInfo.Drivers.Count <> carData.DriverCount) then
    RequestEntryList
  else if Assigned(FOnCarData) then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnCarData(self, carData);
        end)
    else
      FOnCarData(self, carData);
end;

procedure TksBroadcastingProtocol.Msg(const event: TksBroadcastingEvent);
begin
  if Assigned(FOnBroadcastingEvent) then
    if FDoSync then
      TThread.Synchronize(nil,
        procedure
        begin
          FOnBroadcastingEvent(self, event);
        end)
    else
      FOnBroadcastingEvent(self, event);
end;

procedure TksBroadcastingProtocol.RequestFocus(const carRaceNumber: integer;
const cameraSet: string = ''; const camera: string = '');
var
  carInfo: TKsCarInfo;
begin
  if (ConnectionID >= 0) then
  begin
    carInfo := FEntryList.findRaceNumber(carRaceNumber);
    if (carInfo <> nil) then
      RequestFocus(UInt16(carInfo.carIndex), cameraSet, camera);
  end;
end;

end.
