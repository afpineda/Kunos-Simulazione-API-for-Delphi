unit ksBroadcasting.Data;

interface

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

  [2021-03-28] First implementation

  [2021-03-30] Fixed bug on TksEntryList.Sort

  ******************************************************* }

uses
  System.Types,
  System.Generics.Collections,
  System.classes;

{ SUMMARY:

  Data structures needed to handle KS' broadcasting protocol

  NOTES:
  - CarIndex is not an index: do not assume sequential numbering, nor
  zero-based
}

{$SCOPEDENUMS ON}
{$Z4}

type
{$Z1}
  TksDriverCategory = (Platinum = 3, Gold = 2, Silver = 1, Bronze = 0,
    Error = 255);

  TksLapType = (Error = 0, Outlap = 1, Regular = 2, Inlap = 3);

  TksCarLocationEnum = (NONE = 0, Track = 1, Pitlane = 2, PitEntry = 3,
    PitExit = 4);

  TksSessionPhase = (NONE = 0, Starting = 1, PreFormation = 2, FormationLap = 3,
    PreSession = 4, Session = 5, SessionOver = 6, PostSession = 7,
    ResultUI = 8);

  TksRaceSessionType = (Practice = 0, Qualifying = 4, Superpole = 9, Race = 10,
    Hotlap = 11, Hotstint = 12, HotlapSuperpole = 13, Replay = 14);

  TksBroadcastingEventType = (NONE = 0, GreenFlag = 1, SessionOver = 2,
    PenaltyCommMsg = 3, Accident = 4, LapCompleted = 5, BestSessionLap = 6,
    BestPersonalLap = 7);
{$Z2}
  TksNationalityEnum = (Any = 0, Italy = 1, Germany = 2, France = 3, Spain = 4,
    GreatBritain = 5, Hungary = 6, Belgium = 7, Switzerland = 8, Austria = 9,
    Russia = 10, Thailand = 11, Netherlands = 12, Poland = 13, Argentina = 14,
    Monaco = 15, Ireland = 16, Brazil = 17, SouthAfrica = 18, PuertoRico = 19,
    Slovakia = 20, Oman = 21, Greece = 22, SaudiArabia = 23, Norway = 24,
    Turkey = 25, SouthKorea = 26, Lebanon = 27, Armenia = 28, Mexico = 29,
    Sweden = 30, Finland = 31, Denmark = 32, Croatia = 33, Canada = 34,
    China = 35, Portugal = 36, Singapore = 37, Indonesia = 38, USA = 39,
    NewZealand = 40, Australia = 41, SanMarino = 42, UAE = 43, Luxembourg = 44,
    Kuwait = 45, HongKong = 46, Colombia = 47, Japan = 48, Andorra = 49,
    Azerbaijan = 50, Bulgaria = 51, Cuba = 52, CzechRepublic = 53, Estonia = 54,
    Georgia = 55, India = 56, Israel = 57, Jamaica = 58, Latvia = 59,
    Lithuania = 60, Macau = 61, Malaysia = 62, Nepal = 63, NewCaledonia = 64,
    Nigeria = 65, NorthernIreland = 66, PapuaNewGuinea = 67, Philippines = 68,
    Qatar = 69, Romania = 70, Scotland = 71, Serbia = 72, Slovenia = 73,
    Taiwan = 74, Ukraine = 75, Venezuela = 76, Wales = 77, Iran = 78,
    Bahrain = 79, Zimbabwe = 80, ChineseTaipei = 81, Chile = 82, Uruguay = 83,
    Madagascar = 84);

type
  TKsRegistrationResult = record
    ConnectionID: integer;
    Success: boolean;
    ReadOnly: boolean;
    ErrorMessage: string;
    procedure readFromStream(const strm: TStream);
  end;

type
  TKsDriverInfo = class
  private
    FFirstName: string;
    FLastName: string;
    FShortName: string;
    FCategory: TksDriverCategory;
    FNationality: TksNationalityEnum;
    function GetDisplayName: string;
  public
    property DisplayName: string read GetDisplayName;
    property FirstName: string read FFirstName;
    property LastName: string read FLastName;
    property ShortName: string read FShortName;
    property Category: TksDriverCategory read FCategory;
    property Nationality: TksNationalityEnum read FNationality;
    constructor Create(source: TKsDriverInfo = nil);
    procedure readFromStream(const strm: TStream);
  end;

type
  TksCarInfo = class
  private
    FCarIndex: UInt16;
    FCarModelType: BYTE;
    FTeamName: string;
    FRaceNumber: integer;
    FCupCategory: BYTE;
    FCurrentDriverIndex: BYTE;
    FDrivers: TObjectList<TKsDriverInfo>;
    FNationality: TksNationalityEnum;
  public
    constructor Create; overload;
    constructor Create(source: TksCarInfo); overload;
    destructor Destroy; override;
    procedure readFromStream(const strm: TStream);
    property Drivers: TObjectList<TKsDriverInfo> read FDrivers;
    property carIndex: UInt16 read FCarIndex;
    property CarModelType: BYTE read FCarModelType;
    property TeamName: string read FTeamName;
    property raceNumber: integer read FRaceNumber;
    property CupCategory: BYTE read FCupCategory;
    property CurrentDriverIndex: BYTE read FCurrentDriverIndex;
    property Nationality: TksNationalityEnum read FNationality;
  end;

type
  TksEntryList = class(TDictionary<integer, TksCarInfo>)
  public
    constructor Create; overload;
    constructor Create(source: TksEntryList); overload;
    destructor Destroy; override;
    function findIndex(const carIndex: UInt16): TksCarInfo;
    function findRaceNumber(const raceNumber: integer): TksCarInfo;
    procedure Clear;
  end;

type
  TksLapInfo = record
    LaptimeMS: Int32;
    SplitsMS: array of Int32;
    carIndex: UInt16;
    DriverIndex: UInt16;
    IsInvalid: boolean;
    IsValidForBest: boolean;
    LapType: TksLapType;
    procedure readFromStream(const strm: TStream);
  end;

type
  TksCarData = record
    { NOTES from observation:
      - TrackPosition is allways zero
      - WorldPosX,Y does not look like coordinates
      - Yaw is useless
      - Position: strict sequential numbering, 1-based.
    }
    carIndex: UInt16;
    DriverIndex: UInt16;
    Gear: BYTE;
    WorldPosX: Single;
    WorldPosY: Single;
    Yaw: Single;
    CarLocation: TksCarLocationEnum;
    Kmh: UInt16;
    Position: UInt16;
    TrackPosition: UInt16;
    SplinePosition: Single;
    Delta: Int32;
    BestSessionLap: TksLapInfo;
    LastLap: TksLapInfo;
    CurrentLap: TksLapInfo;
    Laps: UInt16;
    CupPosition: UInt16;
    DriverCount: BYTE;
    procedure readFromStream(const strm: TStream);
  end;

type
  TksCarDataList = TDictionary<integer, TksCarData>;

type
  TksSessionData = record
    { NOTES from observation:
      - SessionTime: Elapsed session time counting from race/session start
      - RemainingTime:  allways zero, no meaning ?
      - SessionEndTime: Remaining time to checkered flag (countdown) or
      end of session
      - SessionRemainingTime: no meaning ?
      - EventIndex: allways zero ?
      - SessionIndex: first session is zero, then increases on each new session.
    }
    EventIndex: UInt16;
    SessionIndex: UInt16;
    Phase: TksSessionPhase;
    SessionTime: Single;
    RemainingTime: Single;
    TimeOfDay: Single;
    RainLevel: Single;
    Clouds: Single;
    Wetness: Single;
    BestSessionLap: TksLapInfo;
    // NOTE: Included in kunos api, but not used anywhere
    // BestLapCarIndex: SmallInt;
    // BestLapDriverIndex: SmallInt;
    FocusedCarIndex: Int32;
    ActiveCameraSet: string;
    ActiveCamera: string;
    IsReplayPlaying: boolean;
    ReplaySessionTime: Single;
    ReplayRemainingTime: Single;
    SessionRemainingTime: Single;
    SessionEndTime: Single;
    SessionType: TksRaceSessionType;
    AmbientTemp: BYTE;
    TrackTemp: BYTE;
    CurrentHudPage: string;
    procedure readFromStream(const strm: TStream);
    procedure Reset;
  end;

type
  TksTrackData = class
  private
    FName: string;
    FID: integer;
    FMeters: integer;
    FCameraSets: TDictionary<string, TStringList>;
    FHUDPages: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure readFromStream(const strm: TStream);
    property Name: string read FName;
    property ID: integer read FID;
    property Meters: integer read FMeters;
    property HUDPages: TStringList read FHUDPages;
    property CameraSets: TDictionary<string, TStringList> read FCameraSets;
  end;

type
  TksBroadcastingEvent = record
    eventType: TksBroadcastingEventType;
    messageText: string;
    TimeMS: integer;
    carIndex: integer;
    procedure readFromStream(const strm: TStream);
  end;

function ReadString(const strm: TStream): string;
procedure WriteString(const strm: TStream; const str: string);

implementation

uses
  StrUtils,
  System.Generics.Defaults,
  System.SysUtils;

// ----------------------------------------------------------------------------
// Auxiliary
// ----------------------------------------------------------------------------

function ReadString(const strm: TStream): string;
var
  strLen: UInt16;
  bytes: TBytes;
begin
  strm.ReadBuffer(strLen, sizeof(strLen));
  SetLength(bytes, strLen);
  strm.ReadBuffer(bytes, 0, strLen);
  Result := TEncoding.UTF8.GetString(bytes);
end;

procedure WriteString(const strm: TStream; const str: string);
var
  strLen: UInt16;
  bytes: TBytes;
begin
  bytes := TEncoding.UTF8.GetBytes(str);
  strLen := Length(bytes);
  strm.WriteBuffer(strLen, sizeof(strLen));
  strm.WriteBuffer(bytes, strLen);
end;

// ----------------------------------------------------------------------------
// TKsRegistrationResult
// ----------------------------------------------------------------------------

procedure TKsRegistrationResult.readFromStream(const strm: TStream);
var
  aux: BYTE;
begin
  strm.ReadBuffer(ConnectionID, sizeof(ConnectionID));
  strm.ReadBuffer(aux, sizeof(aux));
  Success := (aux <> 0);
  strm.ReadBuffer(aux, sizeof(aux));
  ReadOnly := (aux = 0);
  ErrorMessage := ReadString(strm);
end;

// ----------------------------------------------------------------------------
// TksCarData
// ----------------------------------------------------------------------------

procedure TksCarData.readFromStream(const strm: TStream);
begin
  strm.ReadBuffer(carIndex, sizeof(carIndex));
  strm.ReadBuffer(DriverIndex, sizeof(DriverIndex));
  strm.ReadBuffer(DriverCount, sizeof(DriverCount));
  strm.ReadBuffer(Gear, sizeof(Gear));
  Gear := Gear - 2;
  strm.ReadBuffer(WorldPosX, sizeof(WorldPosX));
  strm.ReadBuffer(WorldPosY, sizeof(WorldPosY));
  strm.ReadBuffer(Yaw, sizeof(Yaw));
  strm.ReadBuffer(CarLocation, sizeof(CarLocation));
  strm.ReadBuffer(Kmh, sizeof(Kmh));
  strm.ReadBuffer(Position, sizeof(Position));
  strm.ReadBuffer(CupPosition, sizeof(CupPosition));
  strm.ReadBuffer(TrackPosition, sizeof(TrackPosition));
  strm.ReadBuffer(SplinePosition, sizeof(SplinePosition));
  strm.ReadBuffer(Laps, sizeof(Laps));
  strm.ReadBuffer(Delta, sizeof(Delta));
  BestSessionLap.readFromStream(strm);
  LastLap.readFromStream(strm);
  CurrentLap.readFromStream(strm);
end;

// ----------------------------------------------------------------------------
// TksLapInfo
// ----------------------------------------------------------------------------

procedure TksLapInfo.readFromStream(const strm: TStream);
var
  aux, splitCount: BYTE;
  i: integer;
begin
  strm.ReadBuffer(LaptimeMS, sizeof(LaptimeMS));
  strm.ReadBuffer(carIndex, sizeof(carIndex));
  strm.ReadBuffer(DriverIndex, sizeof(DriverIndex));
  strm.ReadBuffer(splitCount, sizeof(splitCount));
  if (splitCount < 3) then
    SetLength(SplitsMS, 3)
  else
    SetLength(SplitsMS, splitCount);
  for i := 0 to splitCount - 1 do
    strm.ReadBuffer(SplitsMS[i], sizeof(SplitsMS[i]));
  for i := splitCount to 2 do
    SplitsMS[i] := High(SplitsMS[i]);
  strm.ReadBuffer(aux, sizeof(aux));
  IsInvalid := aux <> 0;
  strm.ReadBuffer(aux, sizeof(aux));
  IsValidForBest := aux <> 0;
  strm.ReadBuffer(aux, sizeof(aux));
  strm.ReadBuffer(splitCount, sizeof(splitCount));
  if (aux <> 0) then
    LapType := TksLapType.Outlap
  else if (splitCount <> 0) then
    LapType := TksLapType.Inlap
  else
    LapType := TksLapType.Regular;
end;

// ----------------------------------------------------------------------------
// TksCarInfo
// ----------------------------------------------------------------------------

constructor TKsDriverInfo.Create(source: TKsDriverInfo);
begin
  if (source <> nil) then
  begin
    FFirstName := source.FFirstName;
    FLastName := source.FLastName;
    FShortName := source.FShortName;
    FCategory := source.FCategory;
    FNationality := source.FNationality;
  end;
end;

procedure TKsDriverInfo.readFromStream(const strm: TStream);
begin
  FFirstName := ReadString(strm);
  FLastName := ReadString(strm);
  FShortName := ReadString(strm);
  strm.ReadBuffer(FCategory, sizeof(FCategory));
  strm.ReadBuffer(FNationality, sizeof(FNationality));
end;

function TKsDriverInfo.GetDisplayName: string;
var
  lfn, lln: integer;
begin
  lfn := Length(FFirstName);
  lln := Length(FLastName);
  if (lfn > 0) and (lln > 0) then
    Result := LeftStr(FFirstName, 1) + '. ' + FLastName
  else if (lln > 0) then
    Result := FLastName
  else if (lfn > 0) then
    Result := FFirstName
  else
    Result := FShortName;
end;

// ----------------------------------------------------------------------------
// TksCarInfo
// ----------------------------------------------------------------------------

constructor TksCarInfo.Create;
begin
  FCarIndex := HIGH(FCarIndex);
  FDrivers := TObjectList<TKsDriverInfo>.Create;
  FDrivers.OwnsObjects := true;
  FCarModelType := 0;
  FTeamName := '';
  FRaceNumber := 0;
  FCupCategory := 0;
  FCurrentDriverIndex := HIGH(FCurrentDriverIndex);
  FNationality := TksNationalityEnum.Any;
end;

constructor TksCarInfo.Create(source: TksCarInfo);
var
  i: integer;
begin
  FCarIndex := source.FCarIndex;
  FDrivers := TObjectList<TKsDriverInfo>.Create;
  FDrivers.OwnsObjects := true;
  FCarModelType := source.FCarModelType;
  FTeamName := source.FTeamName;
  FRaceNumber := source.FRaceNumber;
  FCupCategory := source.FCupCategory;
  FCurrentDriverIndex := source.FCurrentDriverIndex;
  FNationality := source.FNationality;
  for i := 0 to source.FDrivers.Count - 1 do
    FDrivers.Add(TKsDriverInfo.Create(source.FDrivers[i]));
end;

destructor TksCarInfo.Destroy;
begin
  FDrivers.Free;
  inherited Destroy;
end;

procedure TksCarInfo.readFromStream(const strm: TStream);
var
  DriverCount: BYTE;
  driverInfo: TKsDriverInfo;
  i: BYTE;
begin
  strm.ReadBuffer(FCarIndex, sizeof(FCarIndex));
  strm.ReadBuffer(FCarModelType, sizeof(FCarModelType));
  FTeamName := ReadString(strm);
  strm.ReadBuffer(FRaceNumber, sizeof(FRaceNumber));
  strm.ReadBuffer(FCupCategory, sizeof(FCupCategory));
  strm.ReadBuffer(FCurrentDriverIndex, sizeof(FCurrentDriverIndex));
  strm.ReadBuffer(FNationality, sizeof(FNationality));
  strm.ReadBuffer(DriverCount, sizeof(DriverCount));
  if (DriverCount > 0) then
    for i := 0 to DriverCount - 1 do
    begin
      driverInfo := TKsDriverInfo.Create;
      driverInfo.readFromStream(strm);
      FDrivers.Add(driverInfo);
    end;
end;

// ----------------------------------------------------------------------------
// TksEntryList
// ----------------------------------------------------------------------------

constructor TksEntryList.Create;
begin
  inherited Create;
end;

constructor TksEntryList.Create(source: TksEntryList);
var
  item: TPair<integer, TksCarInfo>;
begin
  inherited Create;
  for item in source do
    Add(item.key, TksCarInfo.Create(item.value));
end;

destructor TksEntryList.Destroy;
begin
  Clear;
  inherited;
end;

// function TksEntryList.Duplicate;
// var
// item: TksCarInfo;
// begin
// Result := TksEntryList.Create;
// for item in Values do
// Result.Add(item.carIndex, TksCarInfo.Create(item));
// end;

// procedure TksEntryList.Sort;
// var
// Comparison: TComparison<TksCarInfo>;
// begin
// Comparison := function(const Left, Right: TksCarInfo): integer
// begin
// Result := Left.FCarIndex - Right.FCarIndex;
// end;
// inherited Sort(TComparer<TksCarInfo>.Construct(Comparison));
// end;

function TksEntryList.findIndex(const carIndex: UInt16): TksCarInfo;
begin
  // Result := nil;
  // i := 0;
  // while (Result = nil) and (i < Count) and (Items[i].carIndex <= carIndex) do
  // begin
  // if (Items[i].carIndex = carIndex) then
  // Result := Items[i];
  // inc(i);
  // end;
  if self.ContainsKey(carIndex) then
    Result := self.Items[carIndex]
  else
    Result := nil;
end;

function TksEntryList.findRaceNumber(const raceNumber: integer): TksCarInfo;
// var
// i: integer;
// begin
// Result := nil;
// i := 0;
// while (Result = nil) and (i < Count) do
// begin
// if (Items[i].raceNumber = raceNumber) then
// Result := Items[i];
// inc(i);
// end;
var
  item: TksCarInfo;
begin
  for item in Values do
    if (item.raceNumber = raceNumber) then
    begin
      Result := item;
      Exit;
    end;
  Result := nil;
end;

procedure TksEntryList.Clear;
var
  item: TksCarInfo;
begin
  for item in Values do
    item.Free;
  inherited Clear;
end;

// ----------------------------------------------------------------------------
// TksSessionData
// ----------------------------------------------------------------------------

procedure TksSessionData.readFromStream(const strm: TStream);
var
  aux: BYTE;
begin
  strm.ReadBuffer(EventIndex, sizeof(EventIndex));
  strm.ReadBuffer(SessionIndex, sizeof(SessionIndex));
  strm.ReadBuffer(SessionType, sizeof(SessionType));
  strm.ReadBuffer(Phase, sizeof(Phase));
  strm.ReadBuffer(SessionTime, sizeof(SessionTime));
  strm.ReadBuffer(SessionEndTime, sizeof(SessionEndTime));
  strm.ReadBuffer(FocusedCarIndex, sizeof(FocusedCarIndex));
  ActiveCameraSet := ReadString(strm);
  ActiveCamera := ReadString(strm);
  CurrentHudPage := ReadString(strm);
  strm.ReadBuffer(aux, sizeof(aux));
  IsReplayPlaying := aux <> 0;
  if (IsReplayPlaying) then
  begin
    strm.ReadBuffer(ReplaySessionTime, sizeof(ReplaySessionTime));
    strm.ReadBuffer(ReplayRemainingTime, sizeof(ReplayRemainingTime));
  end;
  strm.ReadBuffer(TimeOfDay, sizeof(TimeOfDay));
  strm.ReadBuffer(AmbientTemp, sizeof(AmbientTemp));
  strm.ReadBuffer(TrackTemp, sizeof(TrackTemp));
  strm.ReadBuffer(aux, sizeof(aux));
  Clouds := aux / 10.0;
  strm.ReadBuffer(aux, sizeof(aux));
  RainLevel := aux / 10.0;
  strm.ReadBuffer(aux, sizeof(aux));
  Wetness := aux / 10.0;
  BestSessionLap.readFromStream(strm);
end;

procedure TksSessionData.Reset;
begin
  SessionIndex := HIGH(SessionIndex);
  Phase := TksSessionPhase.NONE;
  SessionType := TksRaceSessionType.Practice;
end;

// ----------------------------------------------------------------------------
// TksTrackData
// ----------------------------------------------------------------------------

constructor TksTrackData.Create;
begin
  FCameraSets := TDictionary<string, TStringList>.Create;
  FHUDPages := TStringList.Create;
  FMeters := -1;
  FID := -1;
  FName := '';
end;

destructor TksTrackData.Destroy;
var
  camSet: string;
begin
  for camSet in CameraSets.Keys do
    CameraSets.Items[camSet].Free;
  FCameraSets.Free;
  FHUDPages.Free;
  inherited;
end;

procedure TksTrackData.readFromStream(const strm: TStream);
var
  i, j, camSetCount, camCount, hudCount: BYTE;
  camSetName: string;
  cams: TStringList;
begin
  FName := ReadString(strm);
  strm.ReadBuffer(FID, sizeof(FID));
  strm.ReadBuffer(FMeters, sizeof(FMeters));
  strm.ReadBuffer(camSetCount, sizeof(camSetCount));
  // Cameras
  FCameraSets.Clear;
  if (camSetCount > 0) then
    for i := 0 to camSetCount - 1 do
    begin
      camSetName := ReadString(strm);
      strm.ReadBuffer(camCount, sizeof(camCount));
      cams := TStringList.Create;
      if (camCount > 0) then
        for j := 0 to camCount - 1 do
          cams.Add(ReadString(strm));
      FCameraSets.Add(camSetName, cams);
    end;
  // Hud pages
  strm.ReadBuffer(hudCount, sizeof(hudCount));
  FHUDPages.Clear;
  if (hudCount > 0) then
    for i := 0 to hudCount - 1 do
      FHUDPages.Add(ReadString(strm));
end;

// ----------------------------------------------------------------------------
// TksBroadcastingEvent
// ----------------------------------------------------------------------------

procedure TksBroadcastingEvent.readFromStream(const strm: TStream);
begin
  strm.ReadBuffer(eventType, sizeof(eventType));
  messageText := ReadString(strm);
  strm.ReadBuffer(TimeMS, sizeof(TimeMS));
  strm.ReadBuffer(carIndex, sizeof(carIndex));
end;

end.
