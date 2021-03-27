unit ACPressureTest_state;

{ *******************************************************

  Automated pressure test for AC/ACC

  Computes correct cold pressures to achieve target
  hot pressures

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

  [2021-03-17] First implementation

  ******************************************************* }

interface

uses
  System.classes,
  ksSharedMem.Data,
  ksSharedMem;

const
  GARAGE_TIME_LIMIT = 5; // seconds

type
  TPressureTest = class
    {
      PURPOUSE:
      State machine. Stores all the information needed to compute correct cold
      pressures. A pressure test is conducted as this:
      - Set cold pressures
      - Go to track, and drive some laps.
      - Read hot pressures
      - Choose target pressures and compute correct cold pressures
      - Repeat until target hot pressures match target pressures. There is
      a rounding error about +/- 0.1 PSI.
      Note that "hotlap" sessions are not suitable for such a test.

      STATES:
      - stUnavailable
      Game is not running or not in a driving session. Initial state.

      - stOngoing
      Cold pressures are known, some laps are needed in order to read hot
      pressures.

      - stWaitingColdPressures
      Waiting for car to return to track from garage in order to read cold
      pressures. The driver must enter the setup screen and return to track
      within 5 seconds. Otherwise, cold pressures are not correctly measured.

      - stWaitingForGarage
      Waiting for car to return to garage, since cold pressures are unknown.

      - stFinished
      Both cold and hot pressures are known, so corrected cold pressures
      may be computed.
      This is a final state. In order to start another test, call "Reset".

      OTHER NOTES:
      Some other information is stored which is relevant to the driver:
      tyre compound, air and road temperatures, etc.
    }
  public type
    TState = (stUnavailable, stOngoing, stWaitingColdPressures,
      stWaitingForGarage, stFinished);
  private
    //
    FAirTemp: Single;
    FCarModel: string;
    FColdPressure: array [0 .. 3] of Single;
    FHotPressure: array [0 .. 3] of Single;
    FMinLapCount: integer;
    FNormalizeAtTrackPos: Single;
    FOnStateChange: TNotifyEvent;
    FPlayerCarID: integer;
    FRoadTemp: Single;
    FSnapshot: TksMemorySnapshot;
    FState: TState;
    FTrackName: string;
    FcurrentLapCount: integer;
    FisRainTyre: boolean;
    FstartingLap: integer;
    FtyreCompound: string;
    setupScreenTimeStamp: TDateTime;

    function GetNormalizedPressure(const index: integer;
      const target: Single): Single;
  public
    // Constructor
    constructor Create;

    // Restart pressure test
    procedure Reset;

    // Compute next state (if any). Should be called at regular intervals.
    procedure Step(live: TksLiveData);

    // car model used
    property CarModel: string read FCarModel;
    // cold and hot pressures, available at the relevant states
    property FL_cold: Single read FColdPressure[FL];
    property FL_hot: Single read FHotPressure[FL];
    property FR_cold: Single read FColdPressure[FR];
    property FR_hot: Single read FHotPressure[FR];
    property RL_cold: Single read FColdPressure[RL];
    property RL_hot: Single read FHotPressure[RL];
    property RR_cold: Single read FColdPressure[RR];
    property RR_hot: Single read FHotPressure[RR];

    // Minimun laps to drive before test is finished.
    // Zero means the driver should return to garage in order to finish
    property MinLapCount: integer read FMinLapCount write FMinLapCount;

    // track point where to read hot pressures (0.0 <= value <= 1.0)
    // 0.0 or 1.0 = finish line
    property NormalizeAtTrackPos: Single read FNormalizeAtTrackPos
      write FNormalizeAtTrackPos;

    // Computed correct cold preassures. A target pressure must be given
    property NormalizedPressure[const wheelIndex: integer;
      const targetPSI: Single]: Single read GetNormalizedPressure;

    // internal ID of current CAR, may be used to detect GT3/GT4 cars
    property PlayerCarID: integer read FPlayerCarID;

    // Current state
    property State: TState read FState;

    // Circuit name
    property TrackName: string read FTrackName;

    // Air temperature at the time cold pressures are read
    property airTemp: Single read FAirTemp;

    // Check if rain tyres are in use
    property isRainTyre: boolean read FisRainTyre;

    // Number of laps drived for current pressure test
    property lapCount: integer read FcurrentLapCount;

    // Track temperature at the time cold pressures are read
    property roadTemp: Single read FRoadTemp;

    // Name of tyre compound, which depends on car class, season and wheather
    property tyreCompound: string read FtyreCompound;

    // Notify state changes
    property OnStateChange: TNotifyEvent read FOnStateChange
      write FOnStateChange;
  end;

  // Recommended target pressures for each tyre name
function DefaultTyrePressure(const tyreName: string): Single;

// ----------------------------------------------------------------------------
implementation

uses
  System.SysUtils,
  System.DateUtils,
  ksSharedMem.Utils;

// ----------------------------------------------------------------------------

function DefaultTyrePressure(const tyreName: string): Single;
begin
  if (CompareText(tyreName, 'WH') = 0) then
    Result := 30.0
  else if (CompareText(tyreName, 'DHE') = 0) then
    Result := 27.3
  else if (CompareText(tyreName, 'DHD2') = 0) then
    Result := 27.5
  else
    Result := 26.8;
end;

// ----------------------------------------------------------------------------
// TPressureTest
// ----------------------------------------------------------------------------

constructor TPressureTest.Create;
begin
  FSnapshot := TksMemorySnapshot.Create;
  setupScreenTimeStamp := now();
  FNormalizeAtTrackPos := 0.0;
  FMinLapCount := 0;
  FCarModel := '';
  FTrackName := '';
  Reset;
end;

procedure TPressureTest.Step(live: TksLiveData);
var
  previousState: TPressureTest.TState;
  lapCount: integer;
begin
  previousState := FState;
  if (live.atDrivingSession) and (FSnapshot.isGameRunning(live)) then
  begin
    FSnapshot.CopyFrom(live);
    if (FSnapshot.status <> TACStatus.AC_LIVE) then
      Exit;
    if (FSnapshot.isSetupMenuVisible <> 0) then
      setupScreenTimeStamp := FSnapshot.timeStamp;

    case FState of
      stUnavailable:
        if (FSnapshot.isCarAtGarage) then
          FState := stWaitingColdPressures
        else
          FState := stWaitingForGarage;

      stWaitingForGarage:
        if (FSnapshot.isCarAtGarage) then
          FState := stWaitingColdPressures;

      stOngoing:
        begin
          if (FSnapshot.isCarAtTrack) then
          begin
            lapCount := FSnapshot.completedLaps - FstartingLap;
            if (lapCount > FcurrentLapCount) and
              ((FSnapshot.normalizedCarPosition >= FNormalizeAtTrackPos) or
              ((lapCount - FcurrentLapCount) > 1)) then
            begin
              FcurrentLapCount := lapCount;
              FHotPressure[FL] := FSnapshot.wheelPressure[FL];
              FHotPressure[FR] := FSnapshot.wheelPressure[FR];
              FHotPressure[RL] := FSnapshot.wheelPressure[RL];
              FHotPressure[RR] := FSnapshot.wheelPressure[RR];
              if (FMinLapCount > 0) and (FcurrentLapCount > FMinLapCount) then
                FState := stFinished
              else if Assigned(FOnStateChange) then
                FOnStateChange(self);
            end;
          end
          else if (FMinLapCount <= 0) and (FcurrentLapCount > 0) then
            FState := stFinished
          else
            FState := stUnavailable;
        end;

      stWaitingColdPressures:
        begin
          if (FSnapshot.isCarAtTrack) and
            (SecondsBetween(FSnapshot.timeStamp, setupScreenTimeStamp) <
            GARAGE_TIME_LIMIT) then
          begin
            FState := stOngoing;
            FAirTemp := FSnapshot.airTemp;
            FRoadTemp := FSnapshot.roadTemp;
            if (FSnapshot.rainTyres <> 0) then
              FtyreCompound := FSnapshot.wetTyresName
            else
              FtyreCompound := FSnapshot.dryTyresName;
            FstartingLap := FSnapshot.completedLaps;
            FcurrentLapCount := 0;
            FCarModel := FSnapshot.CarModel;
            FPlayerCarID := FSnapshot.PlayerCarID;
            FisRainTyre := (FSnapshot.rainTyres <> 0);
            FTrackName := FSnapshot.track;
            FColdPressure[FL] := FSnapshot.wheelPressure[FL];
            FColdPressure[FR] := FSnapshot.wheelPressure[FR];
            FColdPressure[RL] := FSnapshot.wheelPressure[RL];
            FColdPressure[RR] := FSnapshot.wheelPressure[RR];
          end
          else if (FSnapshot.isCarAtTrack) then
            FState := stWaitingForGarage;
        end;
    end;
  end
  else
    FState := TState.stUnavailable;
  if (previousState <> FState) and Assigned(FOnStateChange) then
    FOnStateChange(self);
end;

procedure TPressureTest.Reset;
var
  i: integer;
begin
  FState := stWaitingForGarage;
  for i := 0 to 3 do
  begin
    FColdPressure[i] := 0.0;
    FHotPressure[i] := 0.0;
  end;
  FtyreCompound := '';
  FAirTemp := 0.0;
  FRoadTemp := 0.0;
  FisRainTyre := false;
  FcurrentLapCount := 0;
  if (Assigned(FOnStateChange)) then
    FOnStateChange(self);
end;

function TPressureTest.GetNormalizedPressure(const index: integer;
  const target: Single): Single;
var
  ratio: Single;
begin
  if (FColdPressure[index] <> 0.0) then
  begin
    ratio := FHotPressure[index] / FColdPressure[index];
    Result := target / ratio;
  end
  else
    Result := 0.0;
end;

// ----------------------------------------------------------------------------

end.
