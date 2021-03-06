unit PoCAutosaveReplay_protocol;

interface

uses
  ksBroadcasting,
  ksBroadcasting.UDP,
  System.Net.Socket,
  System.Threading,
  System.SyncObjs,
  System.Classes,
  ksBroadcasting.Data;

type
  TAutosaveReplayProtocol = class(TksThreadedBroadcastingMsgHandler)
  public const
    DISPLAY_NAME = 'AutoSaveReplay';
  public type
    TState = (NotRegistered, Waiting, Start, InProgress);
  private
    FState: TState;
    FDelegate: TksUDPDelegate;
    FSaveInterval: integer;
    FLiveSessionTime: Single;
    FLiveRemainingTime: Single;
    FLiveSessionType: TksRaceSessionType;
    FLiveSessionPhase: TksSessionPhase;
    FReplayStartTime: Single;
    FReplaySessionType: TksRaceSessionType;
    FOnSaveReplay: TNotifyEvent;
    FSaveOnEndOfSession: boolean;
    FConnectionEndPoint: TNetEndPoint;
  protected
    procedure doOnSaveReplay;
    procedure Msg(const result: TKsRegistrationResult); overload; override;
    procedure Msg(const sessionData: TKsSessionData); overload; override;
    procedure Msg(const carData: TKsCarData); overload; override;
    procedure Msg(const carInfo: TKsCarInfo); overload; override;
    procedure Msg(const carEntryCount: integer); overload; override;
    procedure Msg(const trackData: TksTrackData); overload; override;
    procedure Msg(const event: TksBroadcastingEvent); overload; override;
    procedure BeforeRegister; override;
    procedure AfterUnregister; override;
  public
    constructor Create;
    procedure Start(Port: integer; const connectionPwd: string);
    // procedure Unregister;
    property OnSaveReplay: TNotifyEvent read FOnSaveReplay write FOnSaveReplay;
    property LiveSessionTime: Single read FLiveSessionTime;
    property LiveRemainingTime: Single read FLiveRemainingTime;
    property LiveSessionType: TksRaceSessionType read FLiveSessionType;
    property LiveSessionPhase: TksSessionPhase read FLiveSessionPhase
      write FLiveSessionPhase;
    property SaveOnEndOfSession: boolean read FSaveOnEndOfSession
      write FSaveOnEndOfSession;
    property State: TState read FState write FState;
  end;

implementation

uses
  Winapi.Winsock2,
  System.DateUtils,
  System.SysUtils;

constructor TAutosaveReplayProtocol.Create;
begin
  FDelegate := TksUDPDelegate.Create;
  inherited Create(FDelegate);
  FState := NotRegistered;
  FOnSaveReplay := nil;
  FConnectionEndPoint.Family := 2;
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
end;

procedure TAutosaveReplayProtocol.BeforeRegister;
begin
  FDelegate.RemoteEndPoint := FConnectionEndPoint;
end;

procedure TAutosaveReplayProtocol.AfterUnregister;
begin
  FState := NotRegistered;
end;

procedure TAutosaveReplayProtocol.Msg(const result: TKsRegistrationResult);
begin
  if (result.Success) then
    FState := Waiting;
end;

procedure TAutosaveReplayProtocol.Msg(const carEntryCount: integer);
begin
  // do nothing
end;

procedure TAutosaveReplayProtocol.Msg(const carInfo: TKsCarInfo);
begin
  carInfo.Free;
end;

procedure TAutosaveReplayProtocol.Msg(const trackData: TksTrackData);
begin
  trackData.Free;
end;

procedure TAutosaveReplayProtocol.Msg(const sessionData: TKsSessionData);
var
  targetTime: Single;
  mustSave: boolean;
begin
  FLiveSessionTime := sessionData.SessionTime;
  FLiveRemainingTime := sessionData.RemainingTime;
  FLiveSessionType := sessionData.SessionType;
  FLiveSessionPhase := sessionData.Phase;
  case FState of
    TState.Waiting:
      begin
        if (FLiveSessionType = TksRaceSessionType.Qualifying) or
          (FLiveSessionType = TksRaceSessionType.Race) then
          if (sessionData.Phase = TksSessionPhase.Session) then
            FState := TState.Start;
      end;
    TState.Start:
      begin
        FState := InProgress;
        FReplayStartTime := FLiveSessionTime;
        FReplaySessionType := FLiveSessionType;
      end;
    TState.InProgress:
      begin
        if (FLiveSessionType <> FReplaySessionType) then
        begin
          // Note: missed messages, protocol error or too much delay between
          // mesages. A reset is needed.
          FState := TState.Waiting;
          Exit;
        end;
        if (FLiveSessionPhase > TksSessionPhase.SessionOver) then
        begin
          mustSave := FSaveOnEndOfSession;
          FState := TState.Waiting;
        end
        else
        begin
          targetTime := FReplayStartTime + (FSaveInterval * 60000.0);
          mustSave := (FLiveSessionTime >= targetTime);
          FState := TState.Start;
        end;
        if (mustSave) then
        begin
          doOnSaveReplay;
        end;
      end;
  end;
end;

procedure TAutosaveReplayProtocol.Msg(const carData: TKsCarData);
begin
  // do nothing
end;

procedure TAutosaveReplayProtocol.Msg(const event: TksBroadcastingEvent);
begin
  // do nothing
end;

procedure TAutosaveReplayProtocol.doOnSaveReplay;
begin
  if Assigned(FOnSaveReplay) then
    TThread.Synchronize(nil,
      procedure
      begin
        FOnSaveReplay(self);
      end);
end;


end.
