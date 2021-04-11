unit ACAutoSave_protocol;

{ *******************************************************

  Auto save replay for AC/ACC

  Broadcasting protocol implementation

  *******************************************************

  (C) 2021. Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-04-04] First implementation

  [2021-04-11] Superpole added to recordable sessions.
  Published session time for next autosave.

  ******************************************************* }

interface

uses
  ksBroadcasting,
  ksBroadcasting.UDP,
  System.Net.Socket,
  System.Classes,
  ksBroadcasting.Data;

type
  TAutosaveReplayProtocol = class(TksThreadedBroadcastingMsgHandler)
  public const
    DISPLAY_NAME = 'AutoSaveReplay';
  public type
    TState = (Inactive, Waiting, InProgress);
    TRegistrationRejectedEvent = procedure(sender: TObject; const msg: string)
      of object;
  private
    FState: TState;
    FDelegate: TksUDPDelegate;
    FSaveInterval: integer;
    FLiveSessionTime: Single;
    FLiveRemainingTime: Single;
    FLiveSessionType: TksRaceSessionType;
    FLiveSessionPhase: TksSessionPhase;
    FAutosaveSessionTime: Single;
    FOnSaveReplayEvent: TNotifyEvent;
    FOnStateChangeEvent: TNotifyEvent;
    FOnRegistrationRejectedEvent: TRegistrationRejectedEvent;
    FSaveOnEndOfSession: boolean;
    FConnectionEndPoint: TNetEndPoint;
  protected
    procedure doOnSaveReplay;
    procedure doOnStateChange;
    procedure Msg(const result: TKsRegistrationResult); overload; override;
    procedure Msg(const sessionData: TKsSessionData); overload; override;
    procedure Msg(const carData: TKsCarData); overload; override;
    procedure Msg(const carInfo: TKsCarInfo); overload; override;
    procedure Msg(const carEntryCount: integer); overload; override;
    procedure Msg(const trackData: TksTrackData); overload; override;
    procedure Msg(const event: TksBroadcastingEvent); overload; override;
    procedure BeforeRegister; override;
    procedure AfterUnregister; override;
    procedure NotifyNoServerActivity; override;
  public
    constructor Create;
    function IsRecordableSession: boolean;
    procedure Start(Port: integer; const connectionPwd: string);
    property OnRegistrationRejected: TRegistrationRejectedEvent
      read FOnRegistrationRejectedEvent write FOnRegistrationRejectedEvent;
    property OnSaveReplay: TNotifyEvent read FOnSaveReplayEvent
      write FOnSaveReplayEvent;
    property OnStateChangeEvent: TNotifyEvent read FOnStateChangeEvent
      write FOnStateChangeEvent;
    property NextAutosaveSessionTime: Single read FAutosaveSessionTime;
    property LiveSessionTime: Single read FLiveSessionTime;
    property LiveRemainingTime: Single read FLiveRemainingTime;
    property LiveSessionType: TksRaceSessionType read FLiveSessionType;
    property LiveSessionPhase: TksSessionPhase read FLiveSessionPhase
      write FLiveSessionPhase;
    property SaveIntervalSeconds: integer read FSaveInterval
      write FSaveInterval;
    property SaveOnEndOfSession: boolean read FSaveOnEndOfSession
      write FSaveOnEndOfSession;
    property State: TState read FState write FState;
  end;

implementation

uses
  Winapi.Winsock2;

constructor TAutosaveReplayProtocol.Create;
begin
  FDelegate := TksUDPDelegate.Create;
  inherited Create(FDelegate);
  FState := Inactive;
  FOnSaveReplayEvent := nil;
  FOnStateChangeEvent := nil;
  FOnRegistrationRejectedEvent := nil;
  FConnectionEndPoint.Family := 2;
  FSaveOnEndOfSession := true;
  FAutosaveSessionTime := 0.0;
{$IFDEF DEBUG}
  FConnectionEndPoint.SetAddress('192.168.1.160');
{$ELSE}
  FConnectionEndPoint.SetAddress('127.0.0.1');
{$ENDIF}
  FConnectionEndPoint.Port := 0;
end;

procedure TAutosaveReplayProtocol.Start(Port: integer;
  const connectionPwd: string);
begin
  FConnectionEndPoint.Port := Port;
  inherited Start(DISPLAY_NAME, connectionPwd);
  inherited MaxServerInactivityMs := UpdateIntervalMs * 5;
end;

procedure TAutosaveReplayProtocol.BeforeRegister;
begin
  FDelegate.RemoteEndPoint := FConnectionEndPoint;
end;

procedure TAutosaveReplayProtocol.AfterUnregister;
begin
  FState := Inactive;
  doOnStateChange;
end;

procedure TAutosaveReplayProtocol.msg(const result: TKsRegistrationResult);
begin
  if (not result.success) and Assigned(FOnRegistrationRejectedEvent) then
  begin
    TThread.Synchronize(nil,
      procedure
      begin
        FOnRegistrationRejectedEvent(self, result.ErrorMessage)
      end);
  end
  else if (FState <> Waiting) then
  begin
    FState := Waiting;
    doOnStateChange;
  end;
end;

procedure TAutosaveReplayProtocol.msg(const carEntryCount: integer);
begin
  // do nothing
end;

procedure TAutosaveReplayProtocol.msg(const carInfo: TKsCarInfo);
begin
  carInfo.Free;
end;

procedure TAutosaveReplayProtocol.msg(const trackData: TksTrackData);
begin
  trackData.Free;
end;

function ComputeTargetTime(time: int64; interval: integer): int64;
begin
  result := time div interval;
  result := (result + 1) * interval;
end;

function TAutosaveReplayProtocol.IsRecordableSession: boolean;
begin
  Result := (FLiveSessionType = TksRaceSessionType.Qualifying) or
          (FLiveSessionType = TksRaceSessionType.Race) or
          (FLiveSessionType = TksRaceSessionType.Superpole)
end;

procedure TAutosaveReplayProtocol.msg(const sessionData: TKsSessionData);
var
  mustSave: boolean;
begin
  FLiveSessionTime := sessionData.SessionTime;
  FLiveRemainingTime := sessionData.RemainingTime;
  FLiveSessionType := sessionData.SessionType;
  FLiveSessionPhase := sessionData.Phase;
  case FState of
    TState.Waiting:
      begin
        if (IsRecordableSession) then
          if (FLiveSessionPhase = TksSessionPhase.Session) then
          begin
            FState := TState.InProgress;
            FAutosaveSessionTime := ComputeTargetTime(Trunc(FLiveSessionTime),
              (FSaveInterval * 1000));
            doOnStateChange;
          end;
      end;
    TState.InProgress:
      begin
        if (not IsRecordableSession) or
          (FLiveSessionPhase < TksSessionPhase.Session) then
        begin
          // Note: session skipped or restarted
          FState := TState.Waiting;
          FAutosaveSessionTime := 0.0;
          doOnStateChange;
          Exit;
        end;
        if (FLiveSessionPhase > TksSessionPhase.SessionOver) then
        begin
          mustSave := FSaveOnEndOfSession;
          FState := TState.Waiting;
          FAutosaveSessionTime := 0.0;
          doOnStateChange;
        end
        else if (FLiveSessionPhase = TksSessionPhase.Session) then
        begin
          mustSave := (FLiveSessionTime >= FAutosaveSessionTime);
        end
        else
          mustSave := false;
        if (mustSave) then
        begin
          FAutosaveSessionTime := ComputeTargetTime(Trunc(FLiveSessionTime),
            (FSaveInterval * 1000));
          doOnSaveReplay;
        end;
      end;
  end;
end;

procedure TAutosaveReplayProtocol.msg(const carData: TKsCarData);
begin
  // do nothing
end;

procedure TAutosaveReplayProtocol.msg(const event: TksBroadcastingEvent);
begin
  // do nothing
end;

procedure TAutosaveReplayProtocol.doOnSaveReplay;
begin
  if Assigned(FOnSaveReplayEvent) then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnSaveReplayEvent(self);
      end);
end;

procedure TAutosaveReplayProtocol.doOnStateChange;
begin
  if Assigned(FOnStateChangeEvent) then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnStateChangeEvent(self);
      end);
end;

procedure TAutosaveReplayProtocol.NotifyNoServerActivity;
begin
  AfterUnregister;
end;

end.
