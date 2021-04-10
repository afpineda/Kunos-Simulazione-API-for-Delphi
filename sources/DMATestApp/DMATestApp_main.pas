unit DMATestApp_main;

{ *******************************************************

  Test application for Kunos Simulazione's shared
  memory API

  *******************************************************

  2021. Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-03-17] First implementation

  [2021-04-21] Up to date with other sources

  ******************************************************* }
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, ksSharedMem, ksSharedMem.Data;

type
  TForm_main = class(TForm)
    PC_main: TPageControl;
    Tab_Static: TTabSheet;
    Tab_Graphics: TTabSheet;
    Tab_Physics: TTabSheet;
    Panel1: TPanel;
    Btn_Update1: TButton;
    Lbl_Info: TLabel;
    Memo_static: TMemo;
    Memo_graphics: TMemo;
    Memo_physics: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Btn_Update1Click(Sender: TObject);
  private
    { Private declarations }
    ks: TksMemorySnapshot;

    procedure dumpGraphics(data: PSPageFileGraphic; dumpTo: TStrings);
    procedure dumpPhysics(data: PSPageFilePhysics; dumpTo: TStrings);
    procedure dumpStatic(data: PSPageFileStatic; dumpTo: TStrings);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

{$R *.dfm}

procedure TForm_main.Btn_Update1Click(Sender: TObject);
begin
  Memo_static.Lines.Clear;
  Memo_graphics.Lines.Clear;
  Memo_physics.Lines.Clear;
  if (ks.isAvailable) then
    try
      dumpGraphics(ks.GraphicsData, Memo_graphics.Lines);
      dumpPhysics(ks.PhysicsData, Memo_physics.Lines);
      dumpStatic(ks.StaticData, Memo_static.Lines);
      if (ks.GraphicsData.status <> AC_OFF) then
        Lbl_Info.Caption := 'Success (game running)'
      else
        Lbl_Info.Caption := 'Success (game NOT running)';
    except
      Lbl_Info.Caption := 'Read error';
    end
  else
    Lbl_Info.Caption := 'Shared memory not available';
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  Memo_static.Lines.Clear;
  Memo_graphics.Lines.Clear;
  Memo_physics.Lines.Clear;
  Lbl_Info.Caption := 'No data';
  PC_main.ActivePageIndex := 0;
  ks := TksMemorySnapshot.Create;
end;

procedure TForm_main.dumpGraphics(data: PSPageFileGraphic; dumpTo: TStrings);
begin
  dumpTo.Add('packetId : ' + data^.packetId.ToString);
  dumpTo.Add('status : ' + integer(data^.status).ToString);
  dumpTo.Add('session : ' + integer(data^.session).ToString);
  // currentTime: array [0 .. 14] of WideChar;
  // lastTime: array [0 .. 14] of WideChar;
  // bestTime: array [0 .. 14] of WideChar;
  dumpTo.Add('split :' + string(data.split));
  dumpTo.Add('completedLaps : ' + data^.completedLaps.ToString);
  dumpTo.Add('position : ' + data^.position.ToString);
  dumpTo.Add('iCurrentTime : ' + data^.iCurrentTime.ToString);
  dumpTo.Add('iLastTime : ' + data^.iLastTime.ToString);
  dumpTo.Add('iBestTime : ' + data^.iBestTime.ToString);
  dumpTo.Add('sessionTimeLeft : ' + data^.sessionTimeLeft.ToString);
  dumpTo.Add('distanceTraveled : ' + data^.distanceTraveled.ToString);
  dumpTo.Add('isInPit : ' + data^.isInPit.ToString);
  dumpTo.Add('currentSectorIndex : ' + data^.currentSectorIndex.ToString);
  dumpTo.Add('lastSectorTime : ' + data^.lastSectorTime.ToString);
  dumpTo.Add('numberOfLaps : ' + data^.numberOfLaps.ToString);
  dumpTo.Add('tyreCompound : ' + string(data^.tyreCompound));
  dumpTo.Add('replayTimeMultiplier : ' + data^.replayTimeMultiplier.ToString);
  dumpTo.Add('normalizedCarPosition : ' + data^.normalizedCarPosition.ToString);
  dumpTo.Add('activeCars : ' + data^.activeCars.ToString);
  // carCoordinates: array [0 .. 59, 0 .. 2] of Single;
  // carID: array [0 .. 59] of integer;
  dumpTo.Add('playerCarID : ' + data^.playerCarID.ToString);
  dumpTo.Add('penaltyTime : ' + data^.penaltyTime.ToString);
  dumpTo.Add('flag : ' + integer(data^.flag).ToString);
  dumpTo.Add('penalty : ' + integer(data^.penalty).ToString);
  dumpTo.Add('idealLineOn : ' + data^.idealLineOn.ToString);
  dumpTo.Add('isInPitLane : ' + data^.isInPitLane.ToString);
  dumpTo.Add('surfaceGrip : ' + data^.surfaceGrip.ToString);
  dumpTo.Add('mandatoryPitDone : ' + data^.mandatoryPitDone.ToString);
  dumpTo.Add('windSpeed : ' + data^.windSpeed.ToString);
  dumpTo.Add('windDirection : ' + data^.windDirection.ToString);
  dumpTo.Add('isSetupMenuVisible : ' + data^.isSetupMenuVisible.ToString);
  dumpTo.Add('mainDisplayIndex : ' + data^.mainDisplayIndex.ToString);
  dumpTo.Add('secondaryDisplayIndex : ' + data^.secondaryDisplayIndex.ToString);
  dumpTo.Add('tc : ' + data^.tc.ToString);
  dumpTo.Add('TCCut : ' + data^.TCCut.ToString);
  dumpTo.Add('EngineMap : ' + data^.EngineMap.ToString);
  dumpTo.Add('abs : ' + data^.abs.ToString);
  dumpTo.Add('fuelXLap : ' + data^.fuelXLap.ToString);
  dumpTo.Add('rainLights : ' + data^.rainLights.ToString);
  dumpTo.Add('flashingLights : ' + data^.flashingLights.ToString);
  dumpTo.Add('lightsStage : ' + data^.lightsStage.ToString);
  dumpTo.Add('exhaustTemperature : ' + data^.exhaustTemperature.ToString);
  dumpTo.Add('wiperLV : ' + data^.wiperLV.ToString);
  dumpTo.Add('DriverStintTotalTimeLeft : ' +
    data^.DriverStintTotalTimeLeft.ToString);
  dumpTo.Add('DriverStintTimeLeft : ' + data^.DriverStintTimeLeft.ToString);
  dumpTo.Add('rainTyres : ' + data^.rainTyres.ToString);
  dumpTo.Add('sessionIndex : ' + data^.sessionIndex.ToString);
  dumpTo.Add('usedFuel : ' + data^.usedFuel.ToString);
  // deltaLapTime: array [0 .. 14] of WideChar;
  dumpTo.Add('ideltaLapTime : ' + data^.ideltaLapTime.ToString);
  // estimatedLapTime: array [0 .. 14] of WideChar;
  dumpTo.Add('iEstimatedLapTime : ' + data^.iEstimatedLapTime.ToString);
  dumpTo.Add('isDeltaPositive : ' + data^.isDeltaPositive.ToString);
  dumpTo.Add('iSplit : ' + data^.iSplit.ToString);
  dumpTo.Add('isValidLap : ' + data^.isValidLap.ToString);
  dumpTo.Add('fuelEstimatedLaps : ' + data^.fuelEstimatedLaps.ToString);
  dumpTo.Add('trackStatus : ' + string(data^.trackStatus));
  dumpTo.Add('missingMandatoryPits : ' + data^.missingMandatoryPits.ToString);
  dumpTo.Add('Clock : ' + data^.Clock.ToString);
  dumpTo.Add('directionLightsLeft : ' + data^.directionLightsLeft.ToString);
  dumpTo.Add('directionLightsRight : ' + data^.directionLightsRight.ToString);
  dumpTo.Add('GlobalYellow : ' + data^.GlobalYellow.ToString);
  dumpTo.Add('GlobalYellow1 : ' + data^.GlobalYellow1.ToString);
  dumpTo.Add('GlobalYellow2 : ' + data^.GlobalYellow2.ToString);
  dumpTo.Add('GlobalYellow3 : ' + data^.GlobalYellow3.ToString);
  dumpTo.Add('GlobalWhite : ' + data^.GlobalWhite.ToString);
  dumpTo.Add('GlobalGreen : ' + data^.GlobalGreen.ToString);
  dumpTo.Add('GlobalChequered : ' + data^.GlobalChequered.ToString);
  dumpTo.Add('GlobalRed : ' + data^.GlobalRed.ToString);
  dumpTo.Add('mfdTyreSet : ' + data^.mfdTyreSet.ToString);
  dumpTo.Add('mfdFuelToAdd : ' + data^.mfdFuelToAdd.ToString);
  dumpTo.Add('mfdTyrePressureLF : ' + data^.mfdTyrePressureLF.ToString);
  dumpTo.Add('mfdTyrePressureRF : ' + data^.mfdTyrePressureRF.ToString);
  dumpTo.Add('mfdTyrePressureLR : ' + data^.mfdTyrePressureLR.ToString);
  dumpTo.Add('mfdTyrePressureRR : ' + data^.mfdTyrePressureRR.ToString);
  dumpTo.Add('currentTyreSet : ' + data^.currentTyreSet.ToString);
  dumpTo.Add('strategyTyreSet : ' + data^.currentTyreSet.ToString);
end;

procedure TForm_main.dumpPhysics(data: PSPageFilePhysics; dumpTo: TStrings);
begin
  dumpTo.Add('packetId : ' + data^.packetId.ToString);
  dumpTo.Add('gas : ' + data^.gas.ToString);
  dumpTo.Add('brake : ' + data^.brake.ToString);
  dumpTo.Add('fuel : ' + data^.fuel.ToString);
  dumpTo.Add('gear : ' + data^.gear.ToString);
  dumpTo.Add('rpms : ' + data^.rpm.ToString);
  dumpTo.Add('steerAngle : ' + data^.steerAngle.ToString);
  dumpTo.Add('speedKmh : ' + data^.speedKmh.ToString);
  // velocity: array [0 .. 2] of Single;
  dumpTo.Add(Format('accG : %1.3f; %1.3f', [data^.accG[0], data^.accG[1]]));
  dumpTo.Add(Format('wheelSlip : %2.2f ; %2.2f ; %2.2f ; %2.2f', [data^.wheelSlip[0],
    data^.wheelSlip[1], data^.wheelSlip[2], data^.wheelSlip[3]]));
  dumpTo.Add(Format('wheelLoad : %2.2f ; %2.2f ; %2.2f ; %2.2f', [data^.wheelLoad[0],
    data^.wheelLoad[1], data^.wheelLoad[2], data^.wheelLoad[3]]));
  dumpTo.Add('wheelPressure : ' + data^.wheelPressure[0].ToString + ' ; ' +
    data^.wheelPressure[1].ToString + ' ; ' + data^.wheelPressure[2].ToString +
    ' ; ' + data^.wheelPressure[3].ToString);
  dumpTo.Add('wheelAngularSpeed : ' + data^.wheelAngularSpeed[0].ToString +
    ' ; ' + data^.wheelAngularSpeed[1].ToString + ' ; ' +
    data^.wheelAngularSpeed[2].ToString + ' ; ' + data^.wheelAngularSpeed[3]
    .ToString);
  dumpTo.Add('tyreWear : ' + data^.tyreWear[0].ToString + ' ; ' + data^.tyreWear
    [1].ToString + ' ; ' + data^.tyreWear[2].ToString + ' ; ' + data^.tyreWear
    [3].ToString);
  dumpTo.Add('tyreDirtyLevel : ' + data^.tyreDirtyLevel[0].ToString + ' ; ' +
    data^.tyreDirtyLevel[1].ToString + ' ; ' + data^.tyreDirtyLevel[2].ToString
    + ' ; ' + data^.tyreDirtyLevel[3].ToString);
  dumpTo.Add('tyreCoreTemp : ' + data^.tyreCoreTemp[0].ToString + ' ; ' +
    data^.tyreCoreTemp[1].ToString + ' ; ' + data^.tyreCoreTemp[2].ToString +
    ' ; ' + data^.tyreCoreTemp[3].ToString);
  // camberRAD: array [0 .. 3] of Single;
  // suspensionTravel: array [0 .. 3] of Single;
  dumpTo.Add('drs : ' + data^.drs.ToString);
  dumpTo.Add('tc : ' + data^.tc.ToString);
  dumpTo.Add('heading : ' + data^.heading.ToString);
  dumpTo.Add('pitch : ' + data^.pitch.ToString);
  dumpTo.Add('roll : ' + data^.roll.ToString);
  dumpTo.Add('cgHeight : ' + data^.cgHeight.ToString);
  // carDamage: array [0 .. 4] of Single;
  dumpTo.Add('numberOfTyresOut : ' + data^.numberOfTyresOut.ToString);
  dumpTo.Add('pitLimiterOn : ' + data^.pitLimiterOn.ToString);
  dumpTo.Add('abs : ' + data^.abs.ToString);
  dumpTo.Add('kersCharge : ' + data^.kersCharge.ToString);
  dumpTo.Add('kersInput : ' + data^.kersInput.ToString);
  dumpTo.Add('autoShifterOn : ' + data^.autoShifterOn.ToString);
  // rideHeight: array [0 .. 1] of Single;
  dumpTo.Add('turboBoost : ' + data^.turboBoost.ToString);
  dumpTo.Add('ballast : ' + data^.ballast.ToString);
  dumpTo.Add('airDensity : ' + data^.airDensity.ToString);
  dumpTo.Add('airTemp : ' + data^.airTemp.ToString);
  dumpTo.Add('roadTemp : ' + data^.roadTemp.ToString);
  // localAngularVel: array [0 .. 2] of Single;
  dumpTo.Add('finalFF : ' + data^.finalFF.ToString);
  dumpTo.Add('performanceMeter : ' + data^.performanceMeter.ToString);
  dumpTo.Add('engineBrake : ' + data^.engineBrake.ToString);
  dumpTo.Add('ersRecoveryLevel : ' + data^.ersRecoveryLevel.ToString);
  dumpTo.Add('ersPowerLevel : ' + data^.ersPowerLevel.ToString);
  dumpTo.Add('ersHeatCharging : ' + data^.ersHeatCharging.ToString);
  dumpTo.Add('ersIsCharging : ' + data^.ersIsCharging.ToString);
  dumpTo.Add('kersCurrentKJ : ' + data^.kersCurrentKJ.ToString);
  dumpTo.Add('drsAvailable : ' + data^.drsAvailable.ToString);
  dumpTo.Add('drsEnabled : ' + data^.drsEnabled.ToString);
  // brakeTemp: array [0 .. 3] of Single;
  dumpTo.Add('clutch : ' + data^.clutch.ToString);
  // tyreTempI: array [0 .. 3] of Single;
  // tyreTempM: array [0 .. 3] of Single;
  // tyreTempO: array [0 .. 3] of Single;
  dumpTo.Add('isAIControlled : ' + data^.isAIControlled.ToString);
  // tyreContactPoint: array [0 .. 3, 0 .. 2] of Single;
  // tyreContactNormal: array [0 .. 3, 0 .. 2] of Single;
  // tyreContactHeading: array [0 .. 3, 0 .. 2] of Single;
  dumpTo.Add('brakeBias : ' + data^.brakeBias.ToString);
  // localVelocity: array [0 .. 2] of Single;
  dumpTo.Add('P2PActivations : ' + data^.P2PActivations.ToString);
  dumpTo.Add('P2PStatus : ' + data^.P2PStatus.ToString);
  dumpTo.Add('currentMaxRpm : ' + data^.currentMaxRpm.ToString);
  // mz: array [0 .. 3] of Single;
  // fx: array [0 .. 3] of Single;
  // fy: array [0 .. 3] of Single;
  // slipRatio: array [0 .. 3] of Single;
  // slipAngle: array [0 .. 3] of Single;
  dumpTo.Add('tcinAction : ' + data^.tcinAction.ToString);
  dumpTo.Add('absInAction : ' + data^.absInAction.ToString);
  // suspensionDamage: array [0 .. 3] of Single;
  dumpTo.Add(Format('tyreTemp : %2.2f ; %2.2f ; %2.2f ; %2.2f', [data^.tyreTemp[0],
    data^.tyreTemp[1], data^.tyreTemp[2], data^.tyreTemp[3]]));
  dumpTo.Add('waterTemp : ' + data^.waterTemp.ToString);
  // brakePressure: array [0 .. 3] of Single;
  dumpTo.Add('frontBrakeCompound : ' + data^.frontBrakeCompound.ToString);
  dumpTo.Add('rearBrakeCompound : ' + data^.rearBrakeCompound.ToString);
  // padLife: array [0 .. 3] of Single;
  // discLife: array [0 .. 3] of Single;
  dumpTo.Add('ignitionOn : ' + data^.ignitionOn.ToString);
  dumpTo.Add('starterEngineOn : ' + data^.starterEngineOn.ToString);
  dumpTo.Add('isEngineRunning : ' + data^.isEngineRunning.ToString);
  dumpTo.Add('kerbVibration : ' + data^.kerbVibration.ToString);
  dumpTo.Add('slipVibrations : ' + data^.slipVibrations.ToString);
  dumpTo.Add('gVibrations : ' + data^.gVibrations.ToString);
  dumpTo.Add('absVibrations : ' + data^.absVibrations.ToString);
end;

procedure TForm_main.dumpStatic(data: PSPageFileStatic; dumpTo: TStrings);
begin
  dumpTo.Add('smVersion : ' + string(data^.smVersion));
  dumpTo.Add('acVersion : ' + string(data^.acVersion));
  dumpTo.Add('numberOfSessions : ' + data^.numberOfSessions.ToString);
  dumpTo.Add('numCars : ' + data^.numCars.ToString);
  dumpTo.Add('carModel : ' + string(data^.carModel));
  dumpTo.Add('track : ' + string(data^.track));
  dumpTo.Add('playerName : ' + string(data^.playerName));
  dumpTo.Add('playerSurname : ' + string(data^.playerSurname));
  dumpTo.Add('playerNick : ' + string(data^.playerNick));
  dumpTo.Add('sectorCount : ' + data^.sectorCount.ToString);
  dumpTo.Add('maxTorque : ' + data^.maxTorque.ToString);
  dumpTo.Add('maxPower : ' + data^.maxPower.ToString);
  dumpTo.Add('maxRpm : ' + data^.maxRpm.ToString);
  dumpTo.Add('maxFuel : ' + data^.maxFuel.ToString);
  // suspensionMaxTravel: array [0 .. 4] of Single;
  // tyreRadius: array [0 .. 4] of Single;
  dumpTo.Add('maxTurboBoost : ' + data^.maxTurboBoost.ToString);
  // deprecated_1: Single;
  // deprecated_2: Single;
  dumpTo.Add('penaltiesEnabled : ' + data^.penaltiesEnabled.ToString);
  dumpTo.Add('aidFuelRate : ' + data^.aidFuelRate.ToString);
  dumpTo.Add('aidTireRate : ' + data^.aidTireRate.ToString);
  dumpTo.Add('aidMechanicalDamage : ' + data^.aidMechanicalDamage.ToString);
  dumpTo.Add('aidAllowTyreBlankets : ' + data^.aidAllowTyreBlankets.ToString);
  dumpTo.Add('aidStability : ' + data^.aidStability.ToString);
  dumpTo.Add('aidAutoClutch : ' + data^.aidAutoClutch.ToString);
  dumpTo.Add('aidAutoBlip : ' + data^.aidAutoBlip.ToString);
  // hasDRS: DWORD;
  // hasERS: DWORD;
  // hasKERS: DWORD;
  // kersMaxJ: Single;
  // engineBrakeSettingsCount: DWORD;
  // ersPowerControllerCount: DWORD;
  // trackSPlineLength: Single;
  // trackConfiguration: array [0 .. 33] of WideChar;
  // ersMaxJ: Single;
  // isTimedRace: DWORD;
  // hasExtraLap: DWORD;
  // carSkin: array [0 .. 33] of WideChar;
  // reversedGridPositions: DWORD;
  dumpTo.Add('PitWindowStart : ' + data^.PitWindowStart.ToString);
  dumpTo.Add('PitWindowEnd : ' + data^.PitWindowEnd.ToString);
  dumpTo.Add('isOnline : ' + data^.isOnline.ToString);
  dumpTo.Add('dryTyresName : ' + string(data^.dryTyresName));
  dumpTo.Add('wetTyresName : ' + string(data^.wetTyresName));
end;

end.
