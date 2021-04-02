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
  TAutosaveReplayProtocol = class(TksBroadcastingMsgHandler)
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
    FConnectionPwd: string;
    FPort: integer;
    started: boolean;
    listener: ITask;
    registrationDaemon: ITask;
    cancelBackgroundTasks: boolean;
    registrationCS: TCriticalSection;
  private
    procedure ReceiveTask;
    procedure RegistrationTask;
  protected
    procedure doOnSaveReplay;
    procedure Msg(const result: TKsRegistrationResult); overload; override;
    procedure Msg(const sessionData: TKsSessionData); overload; override;
    procedure Msg(const carData: TKsCarData); overload; override;
    procedure Msg(const carInfo: TKsCarInfo); overload; override;
    procedure Msg(const carEntryCount: integer); overload; override;
    procedure Msg(const trackData: TksTrackData); overload; override;
    procedure Msg(const event: TksBroadcastingEvent); overload; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(Port: integer; const connectionPwd: string);
    procedure Unregister;
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

procedure TAutosaveReplayProtocol.ReceiveTask;
begin
  while (not cancelBackgroundTasks) do
    try
      ProcessMessage;
    except
    end;
end;

procedure TAutosaveReplayProtocol.RegistrationTask;
begin
  while (not cancelBackgroundTasks) do
    try
      if (started) and (not Registered) and
        (MilliSecondsBetween(Now, LastMsgTimestamp) >= (UpdateIntervalMs * 5))
      then
      begin
        registrationCS.Enter;
        try
          if (not Registered) then
            inherited Register(DISPLAY_NAME, FConnectionPwd);
        finally
          registrationCS.Leave;
        end;
      end;
      sleep(UpdateIntervalMs * 5);
    except
    end;
end;

constructor TAutosaveReplayProtocol.Create;
begin
  registrationCS := TCriticalSection.Create;
  FDelegate := TksUDPDelegate.Create;
  inherited Create(FDelegate);
  FDelegate.Start;
  listener := TTask.Create(ReceiveTask);
  listener.Start;
  registrationDaemon := TTask.Create(RegistrationTask);
  registrationDaemon.Start;
  FState := NotRegistered;
  FOnSaveReplay := nil;
  FConnectionPwd := '';
  FPort := 9000;
  started := false;
end;

destructor TAutosaveReplayProtocol.Destroy;
begin
  cancelBackgroundTasks := true;
  FDelegate.Stop;
  listener.Wait;
  registrationDaemon.Wait;
  registrationCS.Destroy;
  inherited;
end;

procedure TAutosaveReplayProtocol.Register(Port: integer;
  const connectionPwd: string);
begin
  if Registered then
    Unregister;
  registrationCS.Enter;
  try
    FConnectionPwd := connectionPwd;
    FPort := Port;
    FDelegate.RemoteEndPoint.Family := 2;
{$IFDEF DEBUG}
    FDelegate.RemoteEndPoint.SetAddress('192.168.1.160');
{$ELSE}
    FDelegate.RemoteEndPoint.SetAddress('127.0.0.1');
{$ENDIF}
    FDelegate.RemoteEndPoint.Port := FPort;
    started := true;
    inherited Register(DISPLAY_NAME, FConnectionPwd);
  finally
    registrationCS.Leave;
  end;
end;

procedure TAutosaveReplayProtocol.Unregister;
begin
  registrationCS.Enter;
  try
    inherited Unregister;
    FState := NotRegistered;
    started := false;
  finally
    registrationCS.Leave;
  end;
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
    Waiting:
      begin
        if (FLiveSessionType = TksRaceSessionType.Qualifying) or
          (FLiveSessionType = TksRaceSessionType.Race) then
          if (sessionData.Phase = TksSessionPhase.Session) then
            FState := Start;
      end;
    Start:
      begin
        FState := InProgress;
        FReplayStartTime := FLiveSessionTime;
        FReplaySessionType := FLiveSessionType;
      end;
    InProgress:
      begin
        if (FLiveSessionType <> FReplaySessionType) then
        begin
          // Note: missed messages, protocol error or too much delay between
          // mesages. A reset is needed.
          FState := Waiting;
          Exit;
        end;
        if (FLiveSessionPhase <> TksSessionPhase.Session) then
        begin
          mustSave := FSaveOnEndOfSession;
          FState := Waiting;
        end
        else
        begin
          targetTime := FReplayStartTime + (FSaveInterval * 60000.0);
          mustSave := (FLiveSessionTime >= targetTime);
          FState := Start;
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
