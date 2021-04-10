unit ksBroadcasting.Leaderboard;
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

  [2021-04-07] First implementation

  ******************************************************* }

interface

uses
  System.Generics.Collections,
  ksBroadcasting.Data;

type
  TksLeaderboard = class
  public type
    TCarDistances = TArray<TArray<integer>>;
    TField = (driverFirstName, driverLastName, driverShortName,
      driverDisplayName, teamName, raceNumber, bestTime, lastTime, currentTime,
      officialPos, trackPos, carLocation, laps, deltaToPersonalBestLap,
      gapToNextCar, gapToBestLap);
  private
    FEntryList: array of TksCarInfo;
    FCarDataList: array of TksCarData;
    FSessionData: TksSessionData;
    function GetCount: integer;
    function GetCarInfo(index: integer): TksCarInfo;
    function GetCarData(index: integer): TksCarData;
    function GetSessionTime: string;
  public
    constructor Create(entryList: TksEntryList; carDataList: TksCarDataList;
      const sessionData: TksSessionData);
    destructor Destroy; override;
    function GetCarDistances(trackMeters: integer): TCarDistances;
    function CarToCarDistance(car1index, car2index: integer;
      trackMeters: integer): integer;
    function IndexOf(const carNumber: integer): integer;
    function GetField(index: integer; field: TField): string;
    function GetRaceMeters(index: integer; trackMeters: integer): Int64;
    property carData[index: integer]: TksCarData read GetCarData;
    property carInfo[index: integer]: TksCarInfo read GetCarInfo;
    property Count: integer read GetCount;
    property sessionData: TksSessionData read FSessionData;
    property sessionTimeAsString: string read GetSessionTime;
  end;

function LapTimeMsToStr(const timeMS: integer): string;
function SessionTimeMsToStr(const timeMS: Single): string;
function DeltaTimeMsToStr(const time1MS, time2MS: integer): string;
function DistanceBetweenCars(car1Pos, car2Pos, trackMeters: integer): integer;

implementation

uses
  System.Generics.Defaults,
  StrUtils,
  SysUtils;

// ----------------------------------------------------------------------------
// Exported functions
// ----------------------------------------------------------------------------

function LapTimeMsToStr(const timeMS: integer): string;
var
  millis, seconds, minutes: integer;
begin
  if (timeMS < 0) or (timeMS >= 3600000) then
    Result := '--:--.--'
  else
  begin
    millis := timeMS mod 1000;
    seconds := (timeMS div 1000) mod 60;
    minutes := (timeMS div 60000) mod 60;
    Result := Format('%.2d:%.2d.%.3d', [minutes, seconds, millis]);
  end;
end;

function SessionTimeMsToStr(const timeMS: Single): string;
var
  timeMsInt64, seconds, minutes, hours: Int64;
begin
  timeMsInt64 := Trunc(timeMS);
  if (timeMsInt64 < 0) or (timeMsInt64 >= HIGH(integer)) then
    Result := '--:--.--'
  else
  begin
    seconds := (timeMsInt64 div 1000) mod 60;
    minutes := (timeMsInt64 div 60000) mod 60;
    hours := (timeMsInt64 div 3600000) mod 60;
    Result := Format('%.2d:%.2d:%.2d', [hours, minutes, seconds]);
  end;
end;

function DeltaTimeMsToStr(const time1MS, time2MS: integer): string;
var
  delta: integer;
  millis, seconds, minutes: integer;
  sign: string;
begin
  if (time1MS < 0) or (time1MS >= 3600000) or (time2MS < 0) or
    (time2MS >= 3600000) then
    Result := '-.--'
  else
  begin
    if (time1MS >= time2MS) then
    begin
      delta := time1MS - time2MS;
      sign := '+'
    end
    else
    begin
      delta := time2MS - time1MS;
      sign := '-'
    end;
    millis := delta mod 1000;
    seconds := (delta div 1000) mod 60;
    minutes := (delta div 60000) mod 60;

    if (delta < 60000) then
      Result := Format('%s%2d.%.3d', [sign, seconds, millis])
    else
      Result := Format('%s%.2d:%.2d', [sign, minutes, seconds])
  end;
end;

// ----------------------------------------------------------------------------
// TksLeaderboard
// ----------------------------------------------------------------------------

constructor TksLeaderboard.Create(entryList: TksEntryList;
  carDataList: TksCarDataList; const sessionData: TksSessionData);
var
  carDataPair: TPair<integer, TksCarData>;
  carInfo: TksCarInfo;
  i: integer;
begin
  FSessionData := sessionData;
  SetLength(FEntryList, carDataList.Count);
  SetLength(FCarDataList, carDataList.Count);
  for carDataPair in carDataList do
    if (carDataPair.Value.Position > 0) and
      (carDataPair.Value.Position <= carDataList.Count) then
    begin
      i := carDataPair.Value.Position - 1;
      FCarDataList[i] := carDataPair.Value;
      carInfo := entryList.Items[carDataPair.Value.carIndex];
      // if (carInfo<>nil) then
      FEntryList[i] := TksCarInfo.Create(carInfo);
      // else
      // // should not happen
      // FEntryList[i] := TksCarInfo.Create;
    end;
end;

destructor TksLeaderboard.Destroy;
var
  i: integer;
begin
  for i := Low(FEntryList) to High(FEntryList) do
    FEntryList[i].Free;
end;

function TksLeaderboard.GetField(index: integer; field: TField): string;
var
  carInfo: TksCarInfo;
  driverIndex: integer;
  driverInfo: TKsDriverInfo;
begin
  driverIndex := FCarDataList[index].driverIndex;
  carInfo := FEntryList[index];
  driverInfo := carInfo.Drivers.Items[driverIndex];
  case field of
    driverFirstName:
      Result := driverInfo.FirstName;
    driverLastName:
      Result := driverInfo.LastName;
    driverShortName:
      Result := driverInfo.ShortName;
    driverDisplayName:
      Result := driverInfo.DisplayName;
    teamName:
      if (Length(carInfo.teamName) = 0) then
        Result := driverInfo.DisplayName
      else
        Result := carInfo.teamName;
    raceNumber:
      Result := Format('%.3d', [carInfo.raceNumber]);
    bestTime:
      Result := LapTimeMsToStr(FCarDataList[index].BestSessionLap.LaptimeMS);
    lastTime:
      Result := LapTimeMsToStr(FCarDataList[index].LastLap.LaptimeMS);
    currentTime:
      Result := LapTimeMsToStr(FCarDataList[index].CurrentLap.LaptimeMS);
    officialPos:
      Result := Format('%.2d', [FCarDataList[index].Position]);
    trackPos:
      Result := Format('%.2d', [FCarDataList[index].TrackPosition]);
    TField.carLocation:
      case FCarDataList[index].carLocation of
        TksCarLocationEnum.NONE:
          Result := '?';
        TksCarLocationEnum.Track:
          Result := ' ';
        TksCarLocationEnum.Pitlane:
          Result := 'P'
      else
        Result := 'p';
      end;
    laps:
      Result := Format('%3d', [FCarDataList[index].laps]);
    deltaToPersonalBestLap:
      Result := DeltaTimeMsToStr(FCarDataList[index].delta, 0);
    gapToNextCar:
      if (FSessionData.SessionType <> TksRaceSessionType.Race) then
        Result := DeltaTimeMsToStr(FCarDataList[index].BestSessionLap.LaptimeMS,
          FSessionData.BestSessionLap.LaptimeMS)
      else if (index < Length(FCarDataList) - 1) then
      begin
        if (FCarDataList[index].laps > FCarDataList[index + 1].laps) then
          Result := Format('+%d L',
            [FCarDataList[index].laps - FCarDataList[index + 1].laps])
        else if (FCarDataList[index].laps < FCarDataList[index + 1].laps) then
          Result := Format('-%d L',
            [FCarDataList[index + 1].laps - FCarDataList[index].laps])
        else
          Result := DeltaTimeMsToStr(FCarDataList[index].CurrentLap.LaptimeMS,
            FCarDataList[index + 1].CurrentLap.LaptimeMS);
      end
      else
        Result := '';
    gapToBestLap:
      Result := DeltaTimeMsToStr(FCarDataList[index].BestSessionLap.LaptimeMS,
        FSessionData.BestSessionLap.LaptimeMS);
  else
    Result := '???';
  end;
end;

function TksLeaderboard.GetCount: integer;
begin
  Result := Length(FCarDataList);
end;

function TksLeaderboard.GetCarInfo(index: integer): TksCarInfo;
begin
  Result := FEntryList[index];
end;

function TksLeaderboard.GetCarData(index: integer): TksCarData;
begin
  Result := FCarDataList[index];
end;

function TksLeaderboard.GetSessionTime: string;
begin
  Result := SessionTimeMsToStr(FSessionData.SessionTime);
end;

function TksLeaderboard.GetRaceMeters(index: integer;
  trackMeters: integer): Int64;
begin
  Result := (FCarDataList[index].laps * trackMeters) +
    Trunc(FCarDataList[index].SplinePosition * trackMeters);
end;

function TksLeaderboard.IndexOf(const carNumber: integer): integer;
var
  i: integer;
begin
  Result := -1;
  i := 0;
  while (Result < 0) and (i < Length(FCarDataList)) do
    if (FEntryList[i].raceNumber = carNumber) then
      Result := i
    else
      inc(i);
end;

function TksLeaderboard.CarToCarDistance(car1index, car2index: integer;
  trackMeters: integer): integer;
var
  pos1, pos2: Int64;
begin
  pos1 := Trunc(FCarDataList[car1index].SplinePosition * trackMeters);
  pos2 := Trunc(FCarDataList[car2index].SplinePosition * trackMeters);
  Result := DistanceBetweenCars(pos1, pos2, trackMeters);
end;

function DistanceBetweenCars(car1Pos, car2Pos, trackMeters: integer): integer;
var
  aux: Int64;
  d1, d2: integer;
begin
  if (car1Pos < car2Pos) then
  begin
    aux := car2Pos;
    car2Pos := car1Pos;
    car1Pos := aux;
  end;
  d1 := car1Pos - car2Pos;
  d2 := (trackMeters - car1Pos) + car2Pos;
  if (d1 < d2) then
    Result := d1
  else
    Result := d2;
end;

function TksLeaderboard.GetCarDistances(trackMeters: integer): TCarDistances;
var
  idx1, idx2: integer;
begin
  SetLength(Result, Length(FCarDataList));
  for idx1 := Low(Result) to High(Result) do
  begin
    SetLength(Result[idx1], Length(FCarDataList));
    for idx2 := Low(Result[idx1]) to idx1 - 1 do
      Result[idx1][idx2] := Result[idx2][idx1];
    Result[idx1][idx1] := 0;
    for idx2 := idx1 + 1 to High(Result[idx1]) do
      Result[idx1][idx2] := CarToCarDistance(idx1, idx2, trackMeters);
  end;
end;

end.
