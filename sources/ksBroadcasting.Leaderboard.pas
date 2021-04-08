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
    TField = (driverFirstName, driverLastName, driverShortName,
      driverDisplayName, teamName, raceNumber, bestTime, lastTime, currentTime,
      officialPos, trackPos, carLocation, laps);
    TSortCriteria = (bestLap, leadership);
  private
    FEntryList: TksEntryList;
    FCarDataList: TksCarDataList;
    FSortIndex: TList<integer>;
    procedure CreateSortIndex(sort: TSortCriteria);
    function GetCount: integer;
    function GetDriverDisplayName(const driver: TKsDriverInfo): string; inline;
    function GetCarModel(index: integer): BYTE;
    function GetCarLocation(index: integer): TksCarLocationEnum;
  public
    constructor Create(sort: TSortCriteria; entryList: TksEntryList;
      carDataList: TksCarDataList);
    destructor Destroy; override;
    function GetField(index: integer; field: TField): string;
    function GetFieldAsInteger(index: integer; field: TField): integer;
    property CarModel[index: integer]: BYTE read GetCarModel;
    property carLocation[index: integer]: TksCarLocationEnum
      read GetCarLocation;
    property Count: integer read GetCount;
    // procedure Update(const entryList: TksEntryList); overload;
    // procedure Update(const carData: TksCarData); overload;
  end;

function LapTimeMsToStr(const timeMS: integer): string; overload;
function SessionTimeMsToStr(const timeMS: single): string; overload;

implementation

uses
  System.Generics.Defaults,
  StrUtils,
  SysUtils;

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

function SessionTimeMsToStr(const timeMS: single): string;
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

constructor TksLeaderboard.Create(sort: TSortCriteria; entryList: TksEntryList;
  carDataList: TksCarDataList);
begin
  FEntryList := entryList;
  FCarDataList := carDataList;
  FSortIndex := TList<integer>.Create;
  CreateSortIndex(sort);
end;

destructor TksLeaderboard.Destroy;
begin
  FEntryList.Free;
  FCarDataList.Free;
  FSortIndex.Free;
end;

procedure TksLeaderboard.CreateSortIndex(sort: TSortCriteria);
var
  // carData: TksCarData;
  carInfo: TksCarInfo;
  val1, val2: Int64;
  i, j, aux: integer;
begin
  FSortIndex.Clear;
  for carInfo in FEntryList.Values do
    FSortIndex.Add(carInfo.carIndex);

  for i := 0 to FSortIndex.Count - 1 do
    for j := 0 to FSortIndex.Count - 1 do
    begin
      val1 := FCarDataList.Items[FSortIndex[i]].Position;
      val2 := FCarDataList.Items[FSortIndex[j]].Position;
      if (val1 < val2) then
      begin
        aux := FSortIndex[i];
        FSortIndex[i] := FSortIndex[j];
        FSortIndex[j] := aux;
      end;
    end;

  // NOTE: BUGGED !!!
  //
  // FSortIndex.sort(TComparer<integer>.Construct(
  // function(const a, b: integer): integer
  // var
  // aux: Int64;
  // begin
  // case sort of
  // leadership:
  // aux := FCarDataList.Items[b].RacePositionValue - FCarDataList.Items[a]
  // .RacePositionValue;
  // else
  // aux := integer(FCarDataList.Items[b].QualifyPositionValue -
  // FCarDataList.Items[a].QualifyPositionValue);
  // end;
  // if (aux >= 0) then
  // Result := 1
  // else
  // Result := -1;
  // end));
end;

function TksLeaderboard.GetField(index: integer; field: TField): string;
var
  carId: integer;
  carInfo: TksCarInfo;
  driverIndex: integer;
  driverInfo: TKsDriverInfo;
begin
  carId := FSortIndex.Items[index];
  driverIndex := FCarDataList.Items[carId].driverIndex;
  carInfo := FEntryList.Items[carId];
  driverInfo := carInfo.Drivers.Items[driverIndex];
  case field of
    driverFirstName:
      Result := driverInfo.FirstName;
    driverLastName:
      Result := driverInfo.LastName;
    driverShortName:
      Result := driverInfo.ShortName;
    driverDisplayName:
      Result := GetDriverDisplayName(driverInfo);
    teamName:
      if (Length(carInfo.teamName) = 0) then
        Result := GetDriverDisplayName(driverInfo)
      else
        Result := carInfo.teamName;
    raceNumber:
      Result := Format('%.3d', [carInfo.raceNumber]);
    bestTime:
      Result := LapTimeMsToStr(FCarDataList.Items[carId]
        .BestSessionLap.LaptimeMS);
    lastTime:
      Result := LapTimeMsToStr(FCarDataList.Items[carId].LastLap.LaptimeMS);
    currentTime:
      Result := LapTimeMsToStr(FCarDataList.Items[carId].CurrentLap.LaptimeMS);
    officialPos:
      Result := Format('%.2d', [FCarDataList.Items[carId].Position]);
    trackPos:
      Result := Format('%.2d', [FCarDataList.Items[carId].TrackPosition]);
    TField.carLocation:
      case FCarDataList.Items[carId].carLocation of
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
      Result := Format('%3d', [FCarDataList.Items[carId].laps]);
  else
    Result := '???';
  end;
end;

function TksLeaderboard.GetFieldAsInteger(index: integer;
  field: TField): integer;
var
  carId: integer;
  carInfo: TksCarInfo;
  // driverIndex: integer;
  // driverInfo: TKsDriverInfo;
begin
  carId := FSortIndex.Items[index];
  carInfo := FEntryList.Items[carId];
  case field of
    raceNumber:
      Result := carInfo.raceNumber;
    bestTime:
      Result := FCarDataList.Items[carId].BestSessionLap.LaptimeMS;
    lastTime:
      Result := FCarDataList.Items[carId].LastLap.LaptimeMS;
    currentTime:
      Result := FCarDataList.Items[carId].CurrentLap.LaptimeMS;
    officialPos:
      Result := FCarDataList.Items[carId].Position;
    trackPos:
      Result := FCarDataList.Items[carId].TrackPosition;
    TField.carLocation:
      Result := integer(FCarDataList.Items[carId].carLocation);
  else
    Result := 0;
  end;
end;

function TksLeaderboard.GetDriverDisplayName(const driver
  : TKsDriverInfo): string;
begin
  Result := LeftStr(driver.FirstName, 1) + '. ' + driver.LastName
end;

function TksLeaderboard.GetCount: integer;
begin
  Result := FSortIndex.Count;
end;

function TksLeaderboard.GetCarModel(index: integer): BYTE;
var
  carId: integer;
begin
  carId := FSortIndex.Items[index];
  Result := FEntryList.Items[carId].CarModelType;
end;

function TksLeaderboard.GetCarLocation(index: integer): TksCarLocationEnum;
var
  carId: integer;
begin
  carId := FSortIndex.Items[index];
  Result := FCarDataList.Items[carId].carLocation;
end;

end.
