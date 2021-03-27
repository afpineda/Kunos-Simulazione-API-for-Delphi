unit ksSharedMem.Utils;

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

  ******************************************************* }

interface

uses
  ksSharedMem.Data,
  ksSharedMem;

type
  TksLiveDataHelper = class helper for TksLiveData
  private
    function getCarCoordinates(const carIndex: integer;
      const xyz: integer): Single;
    function getCarID(const carIndex: integer): integer;
    function getvelocity(const index: integer): Single;
    function getaccG(const index: integer): Single;
    function getwheelSlip(const index: integer): Single;
    function getwheelLoad(const index: integer): Single;
    function getWheelPressure(const index: integer): Single;
    function getwheelAngularSpeed(const index: integer): Single;
    function gettyreWear(const index: integer): Single;
    function gettyreDirtyLevel(const index: integer): Single;
    function gettyreCoreTemp(const index: integer): Single;
    function getcamberRAD(const index: integer): Single;
    function getsuspensionTravel(const index: integer): Single;
    function getcarDamage(const index: integer): Single;
    function getrideHeight(const index: integer): Single;
    function getlocalAngularVel(const index: integer): Single;
    function getbrakeTemp(const index: integer): Single;
    function gettyreTempI(const index: integer): Single;
    function gettyreTempM(const index: integer): Single;
    function gettyreTempO(const index: integer): Single;
    function gettyreContactPoint(const index1: integer;
      const index2: integer): Single;
    function gettyreContactNormal(const index1: integer;
      const index2: integer): Single;
    function gettyreContactHeading(const index1: integer;
      const index2: integer): Single;
    function getlocalVelocity(const index: integer): Single;
    function getmz(const index: integer): Single;
    function getfx(const index: integer): Single;
    function getfy(const index: integer): Single;
    function getslipRatio(const index: integer): Single;
    function getslipAngle(const index: integer): Single;
    function getsuspensionDamage(const index: integer): Single;
    function gettyreTemp(const index: integer): Single;
    function getbrakePressure(const index: integer): Single;
    function getpadLife(const index: integer): Single;
    function getdiscLife(const index: integer): Single;
    function getSuspensionMaxTravel(const index: integer): Single;
    function getTyreRadius(const index: integer): Single;
  public
    // session state
    function isCarAtGarage: boolean; inline;
    function isCarAtTrack: boolean; inline;
    function atDrivingSession: boolean; inline;

    // Shortcuts to physics file
    function physicsPacketId: LongWord; inline;
    function gas: Single; inline;
    function brake: Single; inline;
    function fuel: Single; inline;
    function gear: integer; inline;
    function rpm: integer; inline;
    function steerAngle: Single; inline;
    function speedKmh: Single; inline;
    property velocity[const index: integer]: Single read getvelocity;
    property accG[const index: integer]: Single read getaccG;
    property wheelSlip[const index: integer]: Single read getwheelSlip;
    property wheelLoad[const index: integer]: Single read getwheelLoad;
    property wheelPressure[const index: integer]: Single read getWheelPressure;
    property wheelAngularSpeed[const index: integer]: Single
      read getwheelAngularSpeed;
    property tyreWear[const index: integer]: Single read gettyreWear;
    property tyreDirtyLevel[const index: integer]: Single
      read gettyreDirtyLevel;
    property tyreCoreTemp[const index: integer]: Single read gettyreCoreTemp;
    property camberRAD[const index: integer]: Single read getcamberRAD;
    property suspensionTravel[const index: integer]: Single
      read getsuspensionTravel;
    function drs: Single; inline;
    function physicsTc: Single; inline;
    function heading: Single; inline;
    function pitch: Single; inline;
    function roll: Single; inline;
    function cgHeight: Single; inline;
    property carDamage[const index: integer]: Single read getcarDamage;
    function numberOfTyresOut: integer; inline;
    function pitLimiterOn: integer; inline;
    function physicsABS: Single; inline;
    function kersCharge: Single; inline;
    function kersInput: Single; inline;
    function autoShifterOn: integer; inline;
    property rideHeight[const index: integer]: Single read getrideHeight;
    function turboBoost: Single; inline;
    function ballast: Single; inline;
    function airDensity: Single; inline;
    function airTemp: Single; inline;
    function roadTemp: Single; inline;
    property localAngularVel[const index: integer]: Single
      read getlocalAngularVel;
    function finalFF: Single; inline;
    function performanceMeter: Single; inline;
    function engineBrake: integer; inline;
    function ersRecoveryLevel: integer; inline;
    function ersPowerLevel: integer; inline;
    function ersHeatCharging: integer; inline;
    function ersIsCharging: integer; inline;
    function kersCurrentKJ: Single; inline;
    function drsAvailable: integer; inline;
    function drsEnabled: integer; inline;
    property brakeTemp[const index: integer]: Single read getbrakeTemp;
    function clutch: Single; inline;
    property tyreTempI[const index: integer]: Single read gettyreTempI;
    property tyreTempM[const index: integer]: Single read gettyreTempM;
    property tyreTempO[const index: integer]: Single read gettyreTempO;
    function isAIControlled: integer; inline;
    property tyreContactPoint[const index1: integer; const index2: integer]
      : Single read gettyreContactPoint;
    property tyreContactNormal[const index1: integer; const index2: integer]
      : Single read gettyreContactNormal;
    property tyreContactHeading[const index1: integer; const index2: integer]
      : Single read gettyreContactHeading;
    function brakeBias: Single; inline;
    property localVelocity[const index: integer]: Single read getlocalVelocity;
    function P2PActivations: integer; inline;
    function P2PStatus: integer; inline;
    function currentMaxRpm: integer; inline;
    property mz[const index: integer]: Single read getmz;
    property fx[const index: integer]: Single read getfx;
    property fy[const index: integer]: Single read getfy;
    property slipRatio[const index: integer]: Single read getslipRatio;
    property slipAngle[const index: integer]: Single read getslipAngle;
    function tcinAction: integer; inline;
    function absInAction: integer; inline;
    property suspensionDamage[const index: integer]: Single
      read getsuspensionDamage;
    property tyreTemp[const index: integer]: Single read gettyreTemp;
    function waterTemp: Single; inline;
    property brakePressure[const index: integer]: Single read getbrakePressure;
    function frontBrakeCompound: integer; inline;
    function rearBrakeCompound: integer; inline;
    property padLife[const index: integer]: Single read getpadLife;
    property discLife[const index: integer]: Single read getdiscLife;
    function ignitionOn: integer; inline;
    function starterEngineOn: integer; inline;
    function isEngineRunning: integer; inline;
    function kerbVibration: Single; inline;
    function slipVibrations: Single; inline;
    function gVibrations: Single; inline;
    function absVibrations: Single; inline;

    // Shortcuts to graphics file
    function graphicsPacketId: integer; inline;
    function status: TACStatus; inline;
    function session: TACSessionType; inline;
    function currentTime: string; inline;
    function lastTime: string; inline;
    function bestTime: string; inline;
    function split: string; inline;
    function completedLaps: integer; inline;
    function position: integer; inline;
    function iCurrentTime: integer; inline;
    function iLastTime: integer;
    function iBestTime: integer; inline;
    function sessionTimeLeft: Single; inline;
    function distanceTraveled: Single; inline;
    function isInPit: integer; inline;
    function currentSectorIndex: integer; inline;
    function lastSectorTime: integer; inline;
    function numberOfLaps: integer; inline;
    function tyreCompound: string; inline;
    function replayTimeMultiplier: Single; inline;
    function normalizedCarPosition: Single; inline;
    function activeCars: integer; inline;
    property carCoordinates[const carIndex: integer; const xyz: integer]: Single
      read getCarCoordinates;
    property carID[const carIndex: integer]: integer read getCarID;
    function playerCarID: integer; inline;
    function penaltyTime: Single; inline;
    function flag: TACFlagType; inline;
    function penalty: TPenaltyShortcut; inline;
    function idealLineOn: integer; inline;
    function isInPitLane: integer; inline;
    function surfaceGrip: Single; inline;
    function mandatoryPitDone: integer; inline;
    function windSpeed: Single; inline;
    function windDirection: Single; inline;
    function isSetupMenuVisible: integer; inline;
    function mainDisplayIndex: integer; inline;
    function secondaryDisplayIndex: integer; inline;
    function TC: integer; inline;
    function TCCut: integer; inline;
    function EngineMap: integer; inline;
    function abs: integer; inline;
    function fuelXLap: integer; inline;
    function rainLights: integer; inline;
    function flashingLights: integer; inline;
    function lightsStage: integer; inline;
    function exhaustTemperature: Single; inline;
    function wiperLV: integer; inline;
    function DriverStintTotalTimeLeft: integer; inline;
    function DriverStintTimeLeft: integer; inline;
    function rainTyres: integer; inline;
    function sessionIndex: integer; inline;
    function usedFuel: Single; inline;
    function deltaLapTime: string; inline;
    function iDeltaLapTime: integer; inline;
    function estimatedLapTime: string; inline;
    function iEstimatedLapTime: integer; inline;
    function isDeltaPositive: integer; inline;
    function iSplit: integer; inline;
    function isValidLap: integer; inline;
    function fuelEstimatedLaps: Single; inline;
    function trackStatus: string; inline;
    function missingMandatoryPits: integer; inline;
    function Clock: Single; inline;
    function directionLightsLeft: integer; inline;
    function directionLightsRight: integer; inline;
    function GlobalYellow: integer; inline;
    function GlobalYellow1: integer; inline;
    function GlobalYellow2: integer; inline;
    function GlobalYellow3: integer; inline;
    function GlobalWhite: integer; inline;
    function GlobalGreen: integer; inline;
    function GlobalChequered: integer; inline;
    function GlobalRed: integer; inline;
    function mfdTyreSet: integer; inline;
    function mfdFuelToAdd: Single; inline;
    function mfdTyrePressureLF: Single; inline;
    function mfdTyrePressureRF: Single; inline;
    function mfdTyrePressureLR: Single; inline;
    function mfdTyrePressureRR: Single; inline;
    function trackGripStatus: TACTrackGripStatus; inline;
    function rainIntensity: TACRainIntensity; inline;
    function rainIntensityIn10min: TACRainIntensity; inline;
    function rainIntensityIn30min: TACRainIntensity; inline;

    // Shortcuts to static data
    function smVersion: string; inline;
    function acVersion: string; inline;
    function numberOfSessions: integer; inline;
    function numCars: integer; inline;
    function carModel: string; inline;
    function track: string; inline;
    function playerName: string; inline;
    function playerSurname: string; inline;
    function playerNick: string; inline;
    function sectorCount: integer; inline;
    function maxTorque: Single; inline;
    function maxPower: Single; inline;
    function maxRpm: integer; inline;
    function maxFuel: Single; inline;
    property suspensionMaxTravel[const index: integer]: Single
      read getSuspensionMaxTravel;
    property tyreRadius[const index: integer]: Single read getTyreRadius;
    function maxTurboBoost: Single; inline;
    function deprecated_1: Single; inline;
    function deprecated_2: Single; inline;
    function penaltiesEnabled: integer; inline;
    function aidFuelRate: Single; inline;
    function aidTireRate: Single; inline;
    function aidMechanicalDamage: Single; inline;
    function aidAllowTyreBlankets: integer; inline;
    function aidStability: Single; inline;
    function aidAutoClutch: integer; inline;
    function aidAutoBlip: integer; inline;
    function hasDRS: integer; inline;
    function hasERS: integer; inline;
    function hasKERS: integer; inline;
    function kersMaxJ: Single; inline;
    function engineBrakeSettingsCount: integer; inline;
    function ersPowerControllerCount: integer; inline;
    function trackSPlineLength: Single; inline;
    function trackConfiguration: string; inline;
    function ersMaxJ: Single; inline;
    function isTimedRace: integer; inline;
    function hasExtraLap: integer; inline;
    function carSkin: string; inline;
    function reversedGridPositions: integer; inline;
    function PitWindowStart: integer; inline;
    function PitWindowEnd: integer; inline;
    function isOnline: integer; inline;
    function dryTyresName: string; inline;
    function wetTyresName: string; inline;
  end;

  TksMemorySnapshotHelper = class helper for TksMemorySnapshot
    // check if game is up and running by comparison with "liveData" instance
    function isGameRunning(liveData: TksLiveData): boolean;
  end;

implementation

// ----------------------------------------------------------------------------
// TksLiveDataHelper
// ----------------------------------------------------------------------------

// session state

function TksLiveDataHelper.atDrivingSession: boolean;
begin
  Result := isAvailable and (GraphicsData.status <> AC_OFF);
end;

function TksLiveDataHelper.isCarAtGarage: boolean;
begin
  Result := (GraphicsData^.status = AC_LIVE) and
    (PhysicsData^.wheelPressure[0] = 0.0);
end;

function TksLiveDataHelper.isCarAtTrack: boolean;
begin
  Result := (GraphicsData^.status = AC_LIVE) and
    (PhysicsData^.wheelPressure[0] <> 0.0);
end;

// Shortcuts to GraphicsData

function TksLiveDataHelper.graphicsPacketId: integer;
begin
  Result := GraphicsData^.packetId;
end;

function TksLiveDataHelper.status: TACStatus;
begin
  Result := GraphicsData^.status;
end;

function TksLiveDataHelper.session: TACSessionType;
begin
  Result := GraphicsData^.session;
end;

function TksLiveDataHelper.currentTime: string;
begin
  Result := string(GraphicsData^.currentTime);
end;

function TksLiveDataHelper.lastTime: string;
begin
  Result := string(GraphicsData^.lastTime);
end;

function TksLiveDataHelper.bestTime: string;
begin
  Result := string(GraphicsData^.bestTime);
end;

function TksLiveDataHelper.split: string;
begin
  Result := string(GraphicsData^.split);
end;

function TksLiveDataHelper.completedLaps: integer;
begin
  Result := GraphicsData^.completedLaps;
end;

function TksLiveDataHelper.position: integer;
begin
  Result := GraphicsData^.position;
end;

function TksLiveDataHelper.iCurrentTime: integer;
begin
  Result := GraphicsData^.iCurrentTime;
end;

function TksLiveDataHelper.iLastTime: integer;
begin
  Result := GraphicsData^.iLastTime;
end;

function TksLiveDataHelper.iBestTime: integer;
begin
  Result := GraphicsData^.iBestTime;
end;

function TksLiveDataHelper.sessionTimeLeft: Single;
begin
  Result := GraphicsData^.sessionTimeLeft;
end;

function TksLiveDataHelper.distanceTraveled: Single;
begin
  Result := GraphicsData^.distanceTraveled;
end;

function TksLiveDataHelper.isInPit: integer;
begin
  Result := GraphicsData^.isInPit;
end;

function TksLiveDataHelper.currentSectorIndex: integer;
begin
  Result := GraphicsData^.currentSectorIndex;
end;

function TksLiveDataHelper.lastSectorTime: integer;
begin
  Result := GraphicsData^.lastSectorTime;
end;

function TksLiveDataHelper.numberOfLaps: integer;
begin
  Result := GraphicsData^.numberOfLaps;
end;

function TksLiveDataHelper.tyreCompound: string;
begin
  Result := string(GraphicsData^.tyreCompound);
end;

function TksLiveDataHelper.replayTimeMultiplier: Single;
begin
  Result := GraphicsData^.replayTimeMultiplier;
end;

function TksLiveDataHelper.normalizedCarPosition: Single;
begin
  Result := GraphicsData^.normalizedCarPosition;
end;

function TksLiveDataHelper.activeCars: integer;
begin
  Result := GraphicsData^.activeCars;
end;

function TksLiveDataHelper.getCarCoordinates(const carIndex: integer;
  const xyz: integer): Single;
begin
  Result := GraphicsData^.carCoordinates[carIndex, xyz];
end;

function TksLiveDataHelper.getCarID(const carIndex: integer): integer;
begin
  Result := GraphicsData^.carID[carIndex];
end;

function TksLiveDataHelper.playerCarID: integer;
begin
  Result := GraphicsData^.playerCarID;
end;

function TksLiveDataHelper.penaltyTime: Single;
begin
  Result := GraphicsData^.penaltyTime;
end;

function TksLiveDataHelper.flag: TACFlagType;
begin
  Result := GraphicsData^.flag;
end;

function TksLiveDataHelper.penalty: TPenaltyShortcut;
begin
  Result := GraphicsData^.penalty;
end;

function TksLiveDataHelper.idealLineOn: integer;
begin
  Result := GraphicsData^.idealLineOn;
end;

function TksLiveDataHelper.isInPitLane: integer;
begin
  Result := GraphicsData^.isInPitLane;
end;

function TksLiveDataHelper.surfaceGrip: Single;
begin
  Result := GraphicsData^.surfaceGrip;
end;

function TksLiveDataHelper.mandatoryPitDone: integer;
begin
  Result := GraphicsData^.mandatoryPitDone;
end;

function TksLiveDataHelper.windSpeed: Single;
begin
  Result := GraphicsData^.windSpeed;
end;

function TksLiveDataHelper.windDirection: Single;
begin
  Result := GraphicsData^.windDirection;
end;

function TksLiveDataHelper.isSetupMenuVisible: integer;
begin
  Result := GraphicsData^.isSetupMenuVisible;
end;

function TksLiveDataHelper.mainDisplayIndex: integer;
begin
  Result := GraphicsData^.mainDisplayIndex;
end;

function TksLiveDataHelper.secondaryDisplayIndex: integer;
begin
  Result := GraphicsData^.secondaryDisplayIndex;
end;

function TksLiveDataHelper.TC: integer;
begin
  Result := GraphicsData^.TC;
end;

function TksLiveDataHelper.TCCut: integer;
begin
  Result := GraphicsData^.TCCut;
end;

function TksLiveDataHelper.EngineMap: integer;
begin
  Result := GraphicsData^.EngineMap;
end;

function TksLiveDataHelper.abs: integer;
begin
  Result := GraphicsData^.abs;
end;

function TksLiveDataHelper.fuelXLap: integer;
begin
  Result := GraphicsData^.fuelXLap;
end;

function TksLiveDataHelper.rainLights: integer;
begin
  Result := GraphicsData^.rainLights;
end;

function TksLiveDataHelper.flashingLights: integer;
begin
  Result := GraphicsData^.flashingLights;
end;

function TksLiveDataHelper.lightsStage: integer;
begin
  Result := GraphicsData^.lightsStage;
end;

function TksLiveDataHelper.exhaustTemperature: Single;
begin
  Result := GraphicsData^.exhaustTemperature;
end;

function TksLiveDataHelper.wiperLV: integer;
begin
  Result := GraphicsData^.wiperLV;
end;

function TksLiveDataHelper.DriverStintTotalTimeLeft: integer;
begin
  Result := GraphicsData^.DriverStintTotalTimeLeft;
end;

function TksLiveDataHelper.DriverStintTimeLeft: integer;
begin
  Result := GraphicsData^.DriverStintTimeLeft;
end;

function TksLiveDataHelper.rainTyres: integer;
begin
  Result := GraphicsData^.rainTyres;
end;

function TksLiveDataHelper.sessionIndex: integer;
begin
  Result := GraphicsData^.sessionIndex;
end;

function TksLiveDataHelper.usedFuel: Single;
begin
  Result := GraphicsData^.usedFuel;
end;

function TksLiveDataHelper.deltaLapTime: string;
begin
  Result := string(GraphicsData^.deltaLapTime);
end;

function TksLiveDataHelper.iDeltaLapTime: integer;
begin
  Result := GraphicsData^.iDeltaLapTime;
end;

function TksLiveDataHelper.estimatedLapTime: string;
begin
  Result := string(GraphicsData^.estimatedLapTime);
end;

function TksLiveDataHelper.iEstimatedLapTime: integer;
begin
  Result := GraphicsData^.iEstimatedLapTime;
end;

function TksLiveDataHelper.isDeltaPositive: integer;
begin
  Result := GraphicsData^.isDeltaPositive;
end;

function TksLiveDataHelper.iSplit: integer;
begin
  Result := GraphicsData^.iSplit;
end;

function TksLiveDataHelper.isValidLap: integer;
begin
  Result := GraphicsData^.isValidLap;
end;

function TksLiveDataHelper.fuelEstimatedLaps: Single;
begin
  Result := GraphicsData^.fuelEstimatedLaps;
end;

function TksLiveDataHelper.trackStatus: string;
begin
  Result := string(GraphicsData^.estimatedLapTime);
end;

function TksLiveDataHelper.missingMandatoryPits: integer;
begin
  Result := GraphicsData^.missingMandatoryPits;
end;

function TksLiveDataHelper.Clock: Single;
begin
  Result := GraphicsData^.Clock;
end;

function TksLiveDataHelper.directionLightsLeft: integer;
begin
  Result := GraphicsData^.directionLightsLeft;
end;

function TksLiveDataHelper.directionLightsRight: integer;
begin
  Result := GraphicsData^.directionLightsRight;
end;

function TksLiveDataHelper.GlobalYellow: integer;
begin
  Result := GraphicsData^.GlobalYellow;
end;

function TksLiveDataHelper.GlobalYellow1: integer;
begin
  Result := GraphicsData^.GlobalYellow1;
end;

function TksLiveDataHelper.GlobalYellow2: integer;
begin
  Result := GraphicsData^.GlobalYellow2;
end;

function TksLiveDataHelper.GlobalYellow3: integer;
begin
  Result := GraphicsData^.GlobalYellow3;
end;

function TksLiveDataHelper.GlobalWhite: integer;
begin
  Result := GraphicsData^.GlobalWhite;
end;

function TksLiveDataHelper.GlobalGreen: integer;
begin
  Result := GraphicsData^.GlobalGreen;
end;

function TksLiveDataHelper.GlobalChequered: integer;
begin
  Result := GraphicsData^.GlobalChequered;
end;

function TksLiveDataHelper.GlobalRed: integer;
begin
  Result := GraphicsData^.GlobalRed;
end;

function TksLiveDataHelper.mfdTyreSet: integer;
begin
  Result := GraphicsData^.mfdTyreSet;
end;

function TksLiveDataHelper.mfdFuelToAdd: Single;
begin
  Result := GraphicsData^.mfdFuelToAdd;
end;

function TksLiveDataHelper.mfdTyrePressureLF: Single;
begin
  Result := GraphicsData^.mfdTyrePressureLF;
end;

function TksLiveDataHelper.mfdTyrePressureRF: Single;
begin
  Result := GraphicsData^.mfdTyrePressureRF;
end;

function TksLiveDataHelper.mfdTyrePressureLR: Single;
begin
  Result := GraphicsData^.mfdTyrePressureLR;
end;

function TksLiveDataHelper.mfdTyrePressureRR: Single;
begin
  Result := GraphicsData^.mfdTyrePressureRR;
end;

function TksLiveDataHelper.trackGripStatus: TACTrackGripStatus;
begin
  Result := GraphicsData^.trackGripStatus;
end;

function TksLiveDataHelper.rainIntensity: TACRainIntensity;
begin
  Result := GraphicsData^.rainIntensity;
end;

function TksLiveDataHelper.rainIntensityIn10min: TACRainIntensity;
begin
  Result := GraphicsData^.rainIntensityIn10min;
end;

function TksLiveDataHelper.rainIntensityIn30min: TACRainIntensity;
begin
  Result := GraphicsData^.rainIntensityIn30min;
end;

// Shortcuts to PhysicsData

function TksLiveDataHelper.getvelocity(const index: integer): Single;
begin
  Result := PhysicsData^.velocity[index];
end;

function TksLiveDataHelper.getaccG(const index: integer): Single;
begin
  Result := PhysicsData^.accG[index];
end;

function TksLiveDataHelper.getwheelSlip(const index: integer): Single;
begin
  Result := PhysicsData^.wheelSlip[index];
end;

function TksLiveDataHelper.getwheelLoad(const index: integer): Single;
begin
  Result := PhysicsData^.wheelLoad[index];
end;

function TksLiveDataHelper.getWheelPressure(const index: integer): Single;
begin
  Result := PhysicsData^.wheelPressure[index];
end;

function TksLiveDataHelper.getwheelAngularSpeed(const index: integer): Single;
begin
  Result := PhysicsData^.wheelAngularSpeed[index];
end;

function TksLiveDataHelper.gettyreWear(const index: integer): Single;
begin
  Result := PhysicsData^.tyreWear[index];
end;

function TksLiveDataHelper.gettyreDirtyLevel(const index: integer): Single;
begin
  Result := PhysicsData^.tyreDirtyLevel[index];
end;

function TksLiveDataHelper.gettyreCoreTemp(const index: integer): Single;
begin
  Result := PhysicsData^.tyreCoreTemp[index];
end;

function TksLiveDataHelper.getcamberRAD(const index: integer): Single;
begin
  Result := PhysicsData^.camberRAD[index];
end;

function TksLiveDataHelper.getsuspensionTravel(const index: integer): Single;
begin
  Result := PhysicsData^.suspensionTravel[index];
end;

function TksLiveDataHelper.getcarDamage(const index: integer): Single;
begin
  Result := PhysicsData^.carDamage[index];
end;

function TksLiveDataHelper.getrideHeight(const index: integer): Single;
begin
  Result := PhysicsData^.rideHeight[index];
end;

function TksLiveDataHelper.getlocalAngularVel(const index: integer): Single;
begin
  Result := PhysicsData^.localAngularVel[index];
end;

function TksLiveDataHelper.getbrakeTemp(const index: integer): Single;
begin
  Result := PhysicsData^.brakeTemp[index];
end;

function TksLiveDataHelper.gettyreTempI(const index: integer): Single;
begin
  Result := PhysicsData^.tyreTempI[index];
end;

function TksLiveDataHelper.gettyreTempM(const index: integer): Single;
begin
  Result := PhysicsData^.tyreTempM[index];
end;

function TksLiveDataHelper.gettyreTempO(const index: integer): Single;
begin
  Result := PhysicsData^.tyreTempO[index];
end;

function TksLiveDataHelper.gettyreContactPoint(const index1: integer;
  const index2: integer): Single;
begin
  Result := PhysicsData^.tyreContactPoint[index1, index2];
end;

function TksLiveDataHelper.gettyreContactNormal(const index1: integer;
  const index2: integer): Single;
begin
  Result := PhysicsData^.tyreContactNormal[index1, index2];
end;

function TksLiveDataHelper.gettyreContactHeading(const index1: integer;
  const index2: integer): Single;
begin
  Result := PhysicsData^.tyreContactHeading[index1, index2];
end;

function TksLiveDataHelper.getlocalVelocity(const index: integer): Single;
begin
  Result := PhysicsData^.localVelocity[index];
end;

function TksLiveDataHelper.getmz(const index: integer): Single;
begin
  Result := PhysicsData^.mz[index];
end;

function TksLiveDataHelper.getfx(const index: integer): Single;
begin
  Result := PhysicsData^.fx[index];
end;

function TksLiveDataHelper.getfy(const index: integer): Single;
begin
  Result := PhysicsData^.fy[index];
end;

function TksLiveDataHelper.getslipRatio(const index: integer): Single;
begin
  Result := PhysicsData^.slipRatio[index];
end;

function TksLiveDataHelper.getslipAngle(const index: integer): Single;
begin
  Result := PhysicsData^.slipAngle[index];
end;

function TksLiveDataHelper.getsuspensionDamage(const index: integer): Single;
begin
  Result := PhysicsData^.suspensionDamage[index];
end;

function TksLiveDataHelper.gettyreTemp(const index: integer): Single;
begin
  Result := PhysicsData^.tyreTemp[index];
end;

function TksLiveDataHelper.getbrakePressure(const index: integer): Single;
begin
  Result := PhysicsData^.brakePressure[index];
end;

function TksLiveDataHelper.getpadLife(const index: integer): Single;
begin
  Result := PhysicsData^.padLife[index];
end;

function TksLiveDataHelper.getdiscLife(const index: integer): Single;
begin
  Result := PhysicsData^.discLife[index];
end;

function TksLiveDataHelper.physicsPacketId: LongWord;
begin
  Result := PhysicsData^.packetId;
end;

function TksLiveDataHelper.gas: Single;
begin
  Result := PhysicsData^.gas;
end;

function TksLiveDataHelper.brake: Single;
begin
  Result := PhysicsData^.brake;
end;

function TksLiveDataHelper.fuel: Single;
begin
  Result := PhysicsData^.fuel;
end;

function TksLiveDataHelper.gear: integer;
begin
  Result := PhysicsData^.gear;
end;

function TksLiveDataHelper.rpm: integer;
begin
  Result := PhysicsData^.rpm;
end;

function TksLiveDataHelper.steerAngle: Single;
begin
  Result := PhysicsData^.steerAngle;
end;

function TksLiveDataHelper.speedKmh: Single;
begin
  Result := PhysicsData^.speedKmh;
end;

function TksLiveDataHelper.drs: Single;
begin
  Result := PhysicsData^.drs;
end;

function TksLiveDataHelper.physicsTc: Single;
begin
  Result := PhysicsData^.TC;
end;

function TksLiveDataHelper.heading: Single;
begin
  Result := PhysicsData^.heading;
end;

function TksLiveDataHelper.pitch: Single;
begin
  Result := PhysicsData^.pitch;
end;

function TksLiveDataHelper.roll: Single;
begin
  Result := PhysicsData^.roll;
end;

function TksLiveDataHelper.cgHeight: Single;
begin
  Result := PhysicsData^.cgHeight;
end;

function TksLiveDataHelper.numberOfTyresOut: integer;
begin
  Result := PhysicsData^.numberOfTyresOut;
end;

function TksLiveDataHelper.pitLimiterOn: integer;
begin
  Result := PhysicsData^.pitLimiterOn;
end;

function TksLiveDataHelper.physicsABS: Single;
begin
  Result := PhysicsData^.abs;
end;

function TksLiveDataHelper.kersCharge: Single;
begin
  Result := PhysicsData^.kersCharge;
end;

function TksLiveDataHelper.kersInput: Single;
begin
  Result := PhysicsData^.kersInput;
end;

function TksLiveDataHelper.autoShifterOn: integer;
begin
  Result := PhysicsData^.autoShifterOn;
end;

function TksLiveDataHelper.turboBoost: Single;
begin
  Result := PhysicsData^.turboBoost;
end;

function TksLiveDataHelper.ballast: Single;
begin
  Result := PhysicsData^.ballast;
end;

function TksLiveDataHelper.airDensity: Single;
begin
  Result := PhysicsData^.airDensity;
end;

function TksLiveDataHelper.airTemp: Single;
begin
  Result := PhysicsData^.airTemp;
end;

function TksLiveDataHelper.roadTemp: Single;
begin
  Result := PhysicsData^.roadTemp;
end;

function TksLiveDataHelper.finalFF: Single;
begin
  Result := PhysicsData^.finalFF;
end;

function TksLiveDataHelper.performanceMeter: Single;
begin
  Result := PhysicsData^.performanceMeter;
end;

function TksLiveDataHelper.engineBrake: integer;
begin
  Result := PhysicsData^.engineBrake;
end;

function TksLiveDataHelper.ersRecoveryLevel: integer;
begin
  Result := PhysicsData^.ersRecoveryLevel;
end;

function TksLiveDataHelper.ersPowerLevel: integer;
begin
  Result := PhysicsData^.ersPowerLevel;
end;

function TksLiveDataHelper.ersHeatCharging: integer;
begin
  Result := PhysicsData^.ersHeatCharging;
end;

function TksLiveDataHelper.ersIsCharging: integer;
begin
  Result := PhysicsData^.ersIsCharging;
end;

function TksLiveDataHelper.kersCurrentKJ: Single;
begin
  Result := PhysicsData^.kersCurrentKJ;
end;

function TksLiveDataHelper.drsAvailable: integer;
begin
  Result := PhysicsData^.drsAvailable;
end;

function TksLiveDataHelper.drsEnabled: integer;
begin
  Result := PhysicsData^.drsEnabled;
end;

function TksLiveDataHelper.clutch: Single;
begin
  Result := PhysicsData^.clutch;
end;

function TksLiveDataHelper.isAIControlled: integer;
begin
  Result := PhysicsData^.isAIControlled;
end;

function TksLiveDataHelper.brakeBias: Single;
begin
  Result := PhysicsData^.brakeBias;
end;

function TksLiveDataHelper.P2PActivations: integer;
begin
  Result := PhysicsData^.P2PActivations;
end;

function TksLiveDataHelper.P2PStatus: integer;
begin
  Result := PhysicsData^.P2PStatus;
end;

function TksLiveDataHelper.currentMaxRpm: integer;
begin
  Result := PhysicsData^.currentMaxRpm;
end;

function TksLiveDataHelper.tcinAction: integer;
begin
  Result := PhysicsData^.tcinAction;
end;

function TksLiveDataHelper.absInAction: integer;
begin
  Result := PhysicsData^.absInAction;
end;

function TksLiveDataHelper.waterTemp: Single;
begin
  Result := PhysicsData^.waterTemp;
end;

function TksLiveDataHelper.frontBrakeCompound: integer;
begin
  Result := PhysicsData^.frontBrakeCompound;
end;

function TksLiveDataHelper.rearBrakeCompound: integer;
begin
  Result := PhysicsData^.rearBrakeCompound;
end;

function TksLiveDataHelper.ignitionOn: integer;
begin
  Result := PhysicsData^.ignitionOn;
end;

function TksLiveDataHelper.starterEngineOn: integer;
begin
  Result := PhysicsData^.starterEngineOn;
end;

function TksLiveDataHelper.isEngineRunning: integer;
begin
  Result := PhysicsData^.isEngineRunning;
end;

function TksLiveDataHelper.kerbVibration: Single;
begin
  Result := PhysicsData^.kerbVibration;
end;

function TksLiveDataHelper.slipVibrations: Single;
begin
  Result := PhysicsData^.slipVibrations;
end;

function TksLiveDataHelper.gVibrations: Single;
begin
  Result := PhysicsData^.gVibrations;
end;

function TksLiveDataHelper.absVibrations: Single;
begin
  Result := PhysicsData^.absVibrations;
end;

// Shortcuts to StaticData

function TksLiveDataHelper.smVersion: string;
begin
  Result := string(StaticData^.smVersion);
end;

function TksLiveDataHelper.acVersion: string;
begin
  Result := string(StaticData^.acVersion);
end;

function TksLiveDataHelper.numberOfSessions: integer;
begin
  Result := StaticData^.numberOfSessions;
end;

function TksLiveDataHelper.numCars: integer;
begin
  Result := StaticData^.numCars;
end;

function TksLiveDataHelper.carModel: string;
begin
  Result := string(StaticData^.carModel);
end;

function TksLiveDataHelper.track: string;
begin
  Result := string(StaticData^.track);
end;

function TksLiveDataHelper.playerName: string;
begin
  Result := string(StaticData^.playerName);
end;

function TksLiveDataHelper.playerSurname: string;
begin
  Result := string(StaticData^.playerSurname);
end;

function TksLiveDataHelper.playerNick: string;
begin
  Result := string(StaticData^.playerNick);
end;

function TksLiveDataHelper.sectorCount: integer;
begin
  Result := StaticData^.sectorCount;
end;

function TksLiveDataHelper.maxTorque: Single;
begin
  Result := StaticData^.maxTorque;
end;

function TksLiveDataHelper.maxPower: Single;
begin
  Result := StaticData^.maxPower;
end;

function TksLiveDataHelper.maxRpm: integer;
begin
  Result := StaticData^.maxRpm;
end;

function TksLiveDataHelper.maxFuel: Single;
begin
  Result := StaticData^.maxFuel;
end;

function TksLiveDataHelper.getSuspensionMaxTravel(const index: integer): Single;
begin
  Result := StaticData^.suspensionMaxTravel[index];
end;

function TksLiveDataHelper.getTyreRadius(const index: integer): Single;
begin
  Result := StaticData^.tyreRadius[index];
end;

function TksLiveDataHelper.maxTurboBoost: Single;
begin
  Result := StaticData^.maxTurboBoost;
end;

function TksLiveDataHelper.deprecated_1: Single;
begin
  Result := StaticData^.deprecated_1;
end;

function TksLiveDataHelper.deprecated_2: Single;
begin
  Result := StaticData^.deprecated_2;
end;

function TksLiveDataHelper.penaltiesEnabled: integer;
begin
  Result := StaticData^.penaltiesEnabled;
end;

function TksLiveDataHelper.aidFuelRate: Single;
begin
  Result := StaticData^.aidFuelRate;
end;

function TksLiveDataHelper.aidTireRate: Single;
begin
  Result := StaticData^.aidTireRate;
end;

function TksLiveDataHelper.aidMechanicalDamage: Single;
begin
  Result := StaticData^.aidMechanicalDamage;
end;

function TksLiveDataHelper.aidAllowTyreBlankets: integer;
begin
  Result := StaticData^.aidAllowTyreBlankets;
end;

function TksLiveDataHelper.aidStability: Single;
begin
  Result := StaticData^.aidStability;
end;

function TksLiveDataHelper.aidAutoClutch: integer;
begin
  Result := StaticData^.aidAutoClutch;
end;

function TksLiveDataHelper.aidAutoBlip: integer;
begin
  Result := StaticData^.aidAutoBlip;
end;

function TksLiveDataHelper.hasDRS: integer;
begin
  Result := StaticData^.hasDRS;
end;

function TksLiveDataHelper.hasERS: integer;
begin
  Result := StaticData^.hasERS;
end;

function TksLiveDataHelper.hasKERS: integer;
begin
  Result := StaticData^.hasKERS;
end;

function TksLiveDataHelper.kersMaxJ: Single;
begin
  Result := StaticData^.kersMaxJ;
end;

function TksLiveDataHelper.engineBrakeSettingsCount: integer;
begin
  Result := StaticData^.engineBrakeSettingsCount;
end;

function TksLiveDataHelper.ersPowerControllerCount: integer;
begin
  Result := StaticData^.ersPowerControllerCount;
end;

function TksLiveDataHelper.trackSPlineLength: Single;
begin
  Result := StaticData^.trackSPlineLength;
end;

function TksLiveDataHelper.trackConfiguration: string;
begin
  Result := string(StaticData^.trackConfiguration);
end;

function TksLiveDataHelper.ersMaxJ: Single;
begin
  Result := StaticData^.ersMaxJ;
end;

function TksLiveDataHelper.isTimedRace: integer;
begin
  Result := StaticData^.isTimedRace;
end;

function TksLiveDataHelper.hasExtraLap: integer;
begin
  Result := StaticData^.hasExtraLap;
end;

function TksLiveDataHelper.carSkin: string;
begin
  Result := string(StaticData^.carSkin);
end;

function TksLiveDataHelper.reversedGridPositions: integer;
begin
  Result := StaticData^.reversedGridPositions;
end;

function TksLiveDataHelper.PitWindowStart: integer;
begin
  Result := StaticData^.PitWindowStart;
end;

function TksLiveDataHelper.PitWindowEnd: integer;
begin
  Result := StaticData^.PitWindowEnd;
end;

function TksLiveDataHelper.isOnline: integer;
begin
  Result := StaticData^.isOnline;
end;

function TksLiveDataHelper.dryTyresName: string;
begin
  Result := string(StaticData^.dryTyresName);
end;

function TksLiveDataHelper.wetTyresName: string;
begin
  Result := string(StaticData^.wetTyresName);
end;


// ----------------------------------------------------------------------------
// TksMemorySnapshotHelper
// ----------------------------------------------------------------------------

function TksMemorySnapshotHelper.isGameRunning(liveData: TksLiveData): boolean;
begin
  if isAvailable and (liveData <> nil) and (liveData.PhysicsData <> nil) then
    Result := (PhysicsData^.packetId <> liveData.PhysicsData^.packetId) or
      (elapsedTimeMs < 200)
  else
    Result := true;
end;

end.
