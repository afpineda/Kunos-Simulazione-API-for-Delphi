unit ksSharedMem.Data;

{ *******************************************************

  Kunos Simulazione shared memory API
  Delphi translation

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

  [2021-04-21] Up to date with other sources

  [2022-12-30] Update to API v1.8.12

  ******************************************************* }

interface

uses
  System.Types;

// Shared memory API version of this translation: 1.10
// All versions are backwards-compatible.

const
  ksPhysicsMemFile = 'Local\acpmf_physics';
  ksGraphicsMemFile = 'Local\acpmf_graphics';
  ksStaticMemFile = 'Local\acpmf_static';

const
  // Wheel order: F/R=Front/Rear, L/R=Left/Right
  FL = 0;
  FR = 1;
  RL = 2;
  RR = 3;

  // NOTE: compiler directives are required for correct interfacing.
  // Do not remove

{$Z4}

type
  TPenaltyShortcut = (None = 0, DriveThrough_Cutting, StopAndGo_10_Cutting,
    StopAndGo_20_Cutting, StopAndGo_30_Cutting, Disqualified_Cutting,
    RemoveBestLaptime_Cutting, DriveThrough_PitSpeeding,
    StopAndGo_10_PitSpeeding, StopAndGo_20_PitSpeeding,
    StopAndGo_30_PitSpeeding, Disqualified_PitSpeeding,
    RemoveBestLaptime_PitSpeeding, Disqualified_IgnoredMandatoryPit,
    PostRaceTime, Disqualified_Trolling, Disqualified_PitEntry,
    Disqualified_PitExit, Disqualified_WrongWay,
    DriveThrough_IgnoredDriverStint, Disqualified_IgnoredDriverStint,
    Disqualified_ExceededDriverStintLimit);

  TACStatus = (AC_OFF = 0, AC_REPLAY = 1, AC_LIVE = 2, AC_PAUSE = 3);

  TACSessionType = (AC_UNKNOWN = -1, AC_PRACTICE = 0, AC_QUALIFY = 1,
    AC_RACE = 2, AC_HOTLAP = 3, AC_TIME_ATTACK = 4, AC_DRIFT = 5, AC_DRAG = 6,
    AC_HOTSTINT = 7, AC_HOTLAPSUPERPOLE = 8);

  TACFlagType = (AC_NO_FLAG = 0, AC_BLUE_FLAG = 1, AC_YELLOW_FLAG = 2,
    AC_BLACK_FLAG = 3, AC_WHITE_FLAG = 4, AC_CHECKERED_FLAG = 5,
    AC_PENALTY_FLAG = 6, AC_GREEN_FLAG = 7, ACC_ORANGE_FLAG = 8);

  TACTrackGripStatus = (ACC_GREEN = 0, ACC_FAST, ACC_OPTIMUM, ACC_GREASY,
    ACC_DAMP, ACC_WET, ACC_FLOODED);

  TACRainIntensity = (ACC_NO_RAIN = 0, ACC_DRIZZLE, ACC_LIGHT_RAIN,
    ACC_MEDIUM_RAIN, ACC_HEAVY_RAIN, ACC_THUNDERSTORM);

{$ALIGN 4}

type
  SPageFilePhysics = record
    packetId: DWORD;
    gas: Single;
    brake: Single;
    fuel: Single;
    gear: DWORD;
    rpm: DWORD;
    steerAngle: Single;
    speedKmh: Single;
    velocity: array [0 .. 2] of Single;
    accG: array [0 .. 2] of Single;
    wheelSlip: array [0 .. 3] of Single;
    wheelLoad: array [0 .. 3] of Single;
    wheelPressure: array [0 .. 3] of Single;
    wheelAngularSpeed: array [0 .. 3] of Single;
    tyreWear: array [0 .. 3] of Single;
    tyreDirtyLevel: array [0 .. 3] of Single;
    tyreCoreTemp: array [0 .. 3] of Single;
    camberRAD: array [0 .. 3] of Single;
    suspensionTravel: array [0 .. 3] of Single;
    drs: Single;
    tc: Single;
    heading: Single;
    pitch: Single;
    roll: Single;
    cgHeight: Single;
    carDamage: array [0 .. 4] of Single;
    numberOfTyresOut: DWORD;
    pitLimiterOn: DWORD;
    abs: Single;
    kersCharge: Single;
    kersInput: Single;
    autoShifterOn: DWORD;
    rideHeight: array [0 .. 1] of Single;
    turboBoost: Single;
    ballast: Single;
    airDensity: Single;
    airTemp: Single;
    roadTemp: Single;
    localAngularVel: array [0 .. 2] of Single;
    finalFF: Single;
    performanceMeter: Single;
    engineBrake: DWORD;
    ersRecoveryLevel: DWORD;
    ersPowerLevel: DWORD;
    ersHeatCharging: DWORD;
    ersIsCharging: DWORD;
    kersCurrentKJ: Single;
    drsAvailable: DWORD;
    drsEnabled: DWORD;
    brakeTemp: array [0 .. 3] of Single;
    clutch: Single;
    tyreTempI: array [0 .. 3] of Single;
    tyreTempM: array [0 .. 3] of Single;
    tyreTempO: array [0 .. 3] of Single;
    isAIControlled: DWORD;
    tyreContactPoint: array [0 .. 3, 0 .. 2] of Single;
    tyreContactNormal: array [0 .. 3, 0 .. 2] of Single;
    tyreContactHeading: array [0 .. 3, 0 .. 2] of Single;
    brakeBias: Single;
    localVelocity: array [0 .. 2] of Single;
    P2PActivations: DWORD;
    P2PStatus: DWORD;
    currentMaxRpm: DWORD;
    mz: array [0 .. 3] of Single;
    fx: array [0 .. 3] of Single;
    fy: array [0 .. 3] of Single;
    slipRatio: array [0 .. 3] of Single;
    slipAngle: array [0 .. 3] of Single;
    tcinAction: DWORD;
    absInAction: DWORD;
    suspensionDamage: array [0 .. 3] of Single;
    tyreTemp: array [0 .. 3] of Single;
    waterTemp: Single;
    brakePressure: array [0 .. 3] of Single;
    frontBrakeCompound: DWORD;
    rearBrakeCompound: DWORD;
    padLife: array [0 .. 3] of Single;
    discLife: array [0 .. 3] of Single;
    ignitionOn: DWORD;
    starterEngineOn: DWORD;
    isEngineRunning: DWORD;
    kerbVibration: Single;
    slipVibrations: Single;
    gVibrations: Single;
    absVibrations: Single;
  end;

  PSPageFilePhysics = ^SPageFilePhysics;

type
  SPageFileGraphic = record
    packetId: DWORD;
    status: TACStatus;
    session: TACSessionType;
    currentTime: array [0 .. 14] of WideChar;
    lastTime: array [0 .. 14] of WideChar;
    bestTime: array [0 .. 14] of WideChar;
    split: array [0 .. 14] of WideChar;
    completedLaps: DWORD;
    position: DWORD;
    iCurrentTime: DWORD;
    iLastTime: DWORD;
    iBestTime: DWORD;
    sessionTimeLeft: Single;
    distanceTraveled: Single;
    isInPit: DWORD;
    currentSectorIndex: DWORD;
    lastSectorTime: DWORD;
    numberOfLaps: DWORD;
    tyreCompound: array [0 .. 32] of WideChar;
    replayTimeMultiplier: Single;
    normalizedCarPosition: Single;
    activeCars: DWORD;
    carCoordinates: array [0 .. 59, 0 .. 2] of Single;
    carID: array [0 .. 59] of DWORD;
    playerCarID: DWORD;
    penaltyTime: Single;
    flag: TACFlagType;
    penalty: TPenaltyShortcut;
    idealLineOn: DWORD;
    isInPitLane: DWORD;
    surfaceGrip: Single;
    mandatoryPitDone: DWORD;
    windSpeed: Single;
    windDirection: Single;
    isSetupMenuVisible: DWORD;
    mainDisplayIndex: DWORD;
    secondaryDisplayIndex: DWORD;
    tc: DWORD;
    TCCut: DWORD;
    EngineMap: DWORD;
    abs: DWORD;
    fuelXLap: DWORD;
    rainLights: DWORD;
    flashingLights: DWORD;
    lightsStage: DWORD;
    exhaustTemperature: Single;
    wiperLV: DWORD;
    DriverStintTotalTimeLeft: DWORD;
    DriverStintTimeLeft: DWORD;
    rainTyres: DWORD;
    sessionIndex: DWORD;
    usedFuel: Single;
    deltaLapTime: array [0 .. 14] of WideChar;
    iDeltaLapTime: DWORD;
    estimatedLapTime: array [0 .. 14] of WideChar;
    iEstimatedLapTime: DWORD;
    isDeltaPositive: DWORD;
    iSplit: DWORD;
    isValidLap: DWORD;
    fuelEstimatedLaps: Single;
    trackStatus: array [0 .. 32] of WideChar;
    missingMandatoryPits: DWORD;
    Clock: Single;
    directionLightsLeft: DWORD;
    directionLightsRight: DWORD;
    GlobalYellow: DWORD;
    GlobalYellow1: DWORD;
    GlobalYellow2: DWORD;
    GlobalYellow3: DWORD;
    GlobalWhite: DWORD;
    GlobalGreen: DWORD;
    GlobalChequered: DWORD;
    GlobalRed: DWORD;
    mfdTyreSet: DWORD;
    mfdFuelToAdd: Single;
    mfdTyrePressureLF: Single;
    mfdTyrePressureRF: Single;
    mfdTyrePressureLR: Single;
    mfdTyrePressureRR: Single;
    trackGripStatus: TACTrackGripStatus;
    rainIntensity: TACRainIntensity;
    rainIntensityIn10min: TACRainIntensity;
    rainIntensityIn30min: TACRainIntensity;
    currentTyreSet: integer;
    strategyTyreSet: integer;
    gapAhead: integer;
    gapBehind: integer;
  end;

  PSPageFileGraphic = ^SPageFileGraphic;

type
  SPageFileStatic = record
    smVersion: array [0 .. 14] of WideChar;
    acVersion: array [0 .. 14] of WideChar;

    // session static info
    numberOfSessions: DWORD;
    numCars: DWORD;
    carModel: array [0 .. 32] of WideChar;
    track: array [0 .. 32] of WideChar;
    playerName: array [0 .. 32] of WideChar;
    playerSurname: array [0 .. 32] of WideChar;
    playerNick: array [0 .. 32] of WideChar;
    sectorCount: DWORD;

    // car static info
    maxTorque: Single;
    maxPower: Single;
    maxRpm: DWORD;
    maxFuel: Single;
    suspensionMaxTravel: array [0 .. 3] of Single;
    tyreRadius: array [0 .. 3] of Single;
    maxTurboBoost: Single;
    deprecated_1: Single;
    deprecated_2: Single;
    penaltiesEnabled: DWORD;
    aidFuelRate: Single;
    aidTireRate: Single;
    aidMechanicalDamage: Single;
    aidAllowTyreBlankets: DWORD;
    aidStability: Single;
    aidAutoClutch: DWORD;
    aidAutoBlip: DWORD;
    hasDRS: DWORD;
    hasERS: DWORD;
    hasKERS: DWORD;
    kersMaxJ: Single;
    engineBrakeSettingsCount: DWORD;
    ersPowerControllerCount: DWORD;
    trackSPlineLength: Single;
    trackConfiguration: array [0 .. 32] of WideChar;
    ersMaxJ: Single;
    isTimedRace: DWORD;
    hasExtraLap: DWORD;
    carSkin: array [0 .. 32] of WideChar;
    reversedGridPositions: DWORD;
    PitWindowStart: DWORD;
    PitWindowEnd: DWORD;
    isOnline: DWORD;
    dryTyresName: array [0 .. 32] of WideChar;
    wetTyresName: array [0 .. 32] of WideChar;
  end;

  PSPageFileStatic = ^SPageFileStatic;

implementation

// ----------------------------------------------------------------------------

end.
