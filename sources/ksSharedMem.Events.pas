unit ksSharedMem.Events;

{ *******************************************************

  Kunos Simulazione shared memory API
  Delphi translation

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

  [2021-03-18] First implementation

  ******************************************************* }

interface

uses
  ksSharedMem.Data,
  ksSharedMem;

type
  TksDataSpotter = class
    {
      PURPOUSE:
      To detect changes at relevant fields of TksLiveData.

      GENERAL USAGE:
      - Create instances of TksLiveData and TksDataSpotter
      - Assign an event handler to "OnChange"
      - Call "Step" at regular intervals.

      ONCHANGE event handler:
      - In order to know what changed exactly and how, you should compare
      TksLiveData with TksDataSpotter.Snapshot. For example:
      If (liveData.status=AC_LIVE) and (spotter.Snapshot.status=AC_OFF) then
      NewDrivingSessionStarted;

      OTHER NOTES:
      - In order to detect more or less changes, derive a new class and override
      "LookForChanges". However, since TChanges can not be expanded, you
      may need to set up another event handler.

      NOTES ON SOME EVENTS:
      - chgFirstRun: sent only once before any other event
    }
  public type
    TChanges = (chgFirstRun, chgPitLimiter, chgAIControlledCar, chgWaterTemp,
      chgPadLife, chgDiscLife, chgIgnition, chgStarterEngine, chgEngineRunning,
      chgStatus, chgSession, chgCompletedLaps, chgPosition, chgBestTime,
      chgInPit, chgActiveCars, chgFlag, chgPenalty, chgPenaltyTime,
      chgInPitLane, chgMandatoryPitDone, chgSetupMenuVisible, chgEngineMap,
      chgRainTyres, chgFuelEstimatedLaps, chgMissingMandatoryPits,
      chgGlobalChequered, chgNotRunning, chgAtGarage, chgCarDamage,
      chgSuspensionDamage, chgGripStatus, chgRainIntensity,
      chgRainIntensityIn10m, chgRainIntensityIn30m);

    TOnSharedDataChangeEvent = procedure(Sender: TksDataSpotter;
      const change: TksDataSpotter.TChanges) of object;
  private
    FThen: TksMemorySnapshot;
    FOnChange: TOnSharedDataChangeEvent;
    FWaterTempAlert: Single;
    FPadLifeAlert: Single;
    FDiscLifeAlert: Single;
    FFuelEstimatedLapsAlert: Single;
    FLastPhysicsPacketId: LongWord;
  protected
    procedure LookForChanges(liveData: TksLiveData); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Step(liveData: TksLiveData);

    property OnChange: TOnSharedDataChangeEvent read FOnChange write FOnChange;
    property WaterTempAlert: Single read FWaterTempAlert write FWaterTempAlert;
    property PadLifeAlert: Single read FPadLifeAlert write FPadLifeAlert;
    property DiscLifeAlert: Single read FDiscLifeAlert write FDiscLifeAlert;
    property FuelEstimatedLapsAlert: Single read FFuelEstimatedLapsAlert
      write FFuelEstimatedLapsAlert;
    property Snapshot: TksMemorySnapshot read FThen;
  end;

implementation

uses
  ksSharedMem.Utils;

constructor TksDataSpotter.Create;
begin
  FThen := TksMemorySnapshot.Create;
  FOnChange := nil;
  FWaterTempAlert := 100.0;
  FFuelEstimatedLapsAlert := 2.0;
  FPadLifeAlert := 0.1;
  FDiscLifeAlert := 0.1;
  FLastPhysicsPacketId := High(FLastPhysicsPacketId);
end;

destructor TksDataSpotter.Destroy;
begin
  FThen.Free;
end;

procedure TksDataSpotter.LookForChanges(liveData: TksLiveData);
begin
  // NOTE:
  // (liveData.isAvailable) and (FThen.isAvailable)=true
  if (not Assigned(FOnChange)) then
    Exit;

  // Detect changes at graphics file
  if (liveData.status <> FThen.status) then
    FOnChange(self, chgStatus);

  if (liveData.Session <> FThen.Session) then
    FOnChange(self, chgSession);

  if (liveData.completedLaps <> FThen.completedLaps) then
    FOnChange(self, chgCompletedLaps);

  if (liveData.position <> FThen.position) then
    FOnChange(self, chgPosition);

  if (liveData.iBestTime <> FThen.iBestTime) then
    FOnChange(self, chgBestTime);

  if (liveData.IsInPit <> FThen.IsInPit) then
    FOnChange(self, chgInPit);

  if (liveData.ActiveCars <> FThen.ActiveCars) then
    FOnChange(self, chgActiveCars);

  if (liveData.Flag <> FThen.Flag) then
    FOnChange(self, chgFlag);

  if (liveData.Penalty <> FThen.Penalty) then
    FOnChange(self, chgPenalty);

  if (liveData.penaltyTime <> FThen.penaltyTime) then
    FOnChange(self, chgPenaltyTime);

  if (liveData.isInPitLane <> FThen.isInPitLane) then
    FOnChange(self, chgInPitLane);

  if (liveData.mandatoryPitDone <> FThen.mandatoryPitDone) then
    FOnChange(self, chgMandatoryPitDone);

  if (liveData.isSetupMenuVisible <> FThen.isSetupMenuVisible) then
    FOnChange(self, chgSetupMenuVisible);

  if (liveData.EngineMap <> FThen.EngineMap) then
    FOnChange(self, chgEngineMap);

  if (liveData.rainTyres <> FThen.rainTyres) then
    FOnChange(self, chgRainTyres);

  if (liveData.fuelEstimatedLaps <= FFuelEstimatedLapsAlert) and
    (FThen.fuelEstimatedLaps > FFuelEstimatedLapsAlert) then
    FOnChange(self, chgFuelEstimatedLaps);

  if (liveData.missingMandatoryPits <> FThen.missingMandatoryPits) then
    FOnChange(self, chgMissingMandatoryPits);

  if (liveData.GlobalChequered <> FThen.GlobalChequered) then
    FOnChange(self, chgMissingMandatoryPits);

  if (liveData.trackGripStatus <> FThen.trackGripStatus) then
    FOnChange(self, chgGripStatus);

  if (liveData.rainIntensity <> FThen.rainIntensity) then
    FOnChange(self, chgRainIntensity);

  if (liveData.rainIntensityIn10min <> FThen.rainIntensityIn10min) then
    FOnChange(self, chgRainIntensityIn10m);

  if (liveData.rainIntensityIn30min <> FThen.rainIntensityIn30min) then
    FOnChange(self, chgRainIntensityIn30m);

  // Other
  if (FThen.physicsPacketId = liveData.physicsPacketId) and
    (FThen.physicsPacketId <> FLastPhysicsPacketId) and
    (FThen.elapsedTimeMs >= 200) then
  begin
    FLastPhysicsPacketId := FThen.physicsPacketId; // avoid repetitive events
    FOnChange(self, chgNotRunning);
  end;

  if (liveData.isCarAtGarage <> FThen.isCarAtGarage) then
    FOnChange(self, chgAtGarage);

  // Detect changes at physics file if car not at garage
  if (liveData.status = AC_LIVE) and (FThen.status = AC_LIVE) and
    (liveData.wheelPressure[0] <> 0.0) and (FThen.wheelPressure[0] <> 0.0) then
  begin
    if (liveData.pitLimiterOn <> FThen.pitLimiterOn) then
      FOnChange(self, chgPitLimiter);

    if (liveData.isAIControlled <> FThen.isAIControlled) then
      FOnChange(self, chgAIControlledCar);

    if (liveData.waterTemp <= FWaterTempAlert) and
      (FThen.waterTemp > FWaterTempAlert) then
      FOnChange(self, chgWaterTemp);

    if (liveData.padLife[0] <= FPadLifeAlert) and
      (FThen.padLife[0] > FPadLifeAlert) then
      FOnChange(self, chgPadLife)
    else if (liveData.padLife[1] <= FPadLifeAlert) and
      (FThen.padLife[1] > FPadLifeAlert) then
      FOnChange(self, chgPadLife)
    else if (liveData.padLife[2] <= FPadLifeAlert) and
      (FThen.padLife[2] > FPadLifeAlert) then
      FOnChange(self, chgPadLife)
    else if (liveData.padLife[3] <= FPadLifeAlert) and
      (FThen.padLife[3] > FPadLifeAlert) then
      FOnChange(self, chgPadLife);

    if (liveData.discLife[0] <= FDiscLifeAlert) and
      (FThen.discLife[0] > FDiscLifeAlert) then
      FOnChange(self, chgDiscLife)
    else if (liveData.discLife[1] <= FDiscLifeAlert) and
      (FThen.discLife[1] > FDiscLifeAlert) then
      FOnChange(self, chgDiscLife)
    else if (liveData.discLife[2] <= FDiscLifeAlert) and
      (FThen.discLife[2] > FDiscLifeAlert) then
      FOnChange(self, chgDiscLife)
    else if (liveData.discLife[3] <= FDiscLifeAlert) and
      (FThen.discLife[3] > FDiscLifeAlert) then
      FOnChange(self, chgDiscLife);

    if (liveData.ignitionOn <> FThen.ignitionOn) then
      FOnChange(self, chgIgnition);

    if (liveData.starterEngineOn <> FThen.starterEngineOn) then
      FOnChange(self, chgStarterEngine);

    if (liveData.isEngineRunning <> FThen.isEngineRunning) then
      FOnChange(self, chgEngineRunning);

    if (liveData.carDamage[0] <> FThen.carDamage[0]) or
      (liveData.carDamage[1] <> FThen.carDamage[1]) or
      (liveData.carDamage[2] <> FThen.carDamage[2]) or
      (liveData.carDamage[3] <> FThen.carDamage[3]) or
      (liveData.carDamage[4] <> FThen.carDamage[4]) then
      FOnChange(self, chgCarDamage);

    if (liveData.suspensionDamage[0] <> FThen.suspensionDamage[0]) or
      (liveData.suspensionDamage[1] <> FThen.suspensionDamage[1]) or
      (liveData.suspensionDamage[2] <> FThen.suspensionDamage[2]) or
      (liveData.suspensionDamage[3] <> FThen.suspensionDamage[3]) then
      FOnChange(self, chgSuspensionDamage);
  end;
end;

procedure TksDataSpotter.Step(liveData: TksLiveData);

begin
  if (liveData.isAvailable) then
    if (FThen.isAvailable) then
      LookForChanges(liveData)
    else if Assigned(FOnChange) then
      FOnChange(self, chgFirstRun);
  FThen.CopyFrom(liveData);
end;

end.
