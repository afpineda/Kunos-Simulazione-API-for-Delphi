unit ksBroadcasting.Data;

interface

uses
  System.Types,
  System.Generics.Collections,
  System.classes;

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
  TKsDriverInfo = class
  private
    FFirstName: string;
    FLastName: string;
    FShortName: string;
    FCategory: TksDriverCategory;
    FNationality: TksNationalityEnum;
  public
    property FirstName: string read FFirstName;
    property LastName: string read FLastName;
    property ShortName: string read FShortName;
    property Category: TksDriverCategory read FCategory;
    property Nationality: TksNationalityEnum read FNationality;
  end;

type
  TksCarInfo = class
  private
    FCarIndex: SmallInt;
    FCarModelType: BYTE;
    FTeamName: string;
    FRaceNumber: integer;
    FCupCategory: BYTE;
    FCurrentDriverIndex: BYTE;
    FDrivers: TObjectList<TKsDriverInfo>;
    FNationality: TksNationalityEnum;
  public
    class function findCarInfo(entryList: TObjectList<TksCarInfo>;
      const carIndex: SmallInt): TksCarInfo;
    class function findRaceNumber(entryList: TObjectList<TksCarInfo>;
      const raceNumber: integer): TksCarInfo;
    class procedure Copy(fromList, toList: TObjectList<TksCarInfo>);
    constructor Create(AcarIndex: SmallInt); overload;
    constructor Create(source: TksCarInfo); overload;
    destructor Destroy; override;
    procedure readFromStream(const strm: TStream);
    property Drivers: TObjectList<TKsDriverInfo> read FDrivers;
    property carIndex: SmallInt read FCarIndex;
    property CarModelType: BYTE read FCarModelType;
    property TeamName: string read FTeamName;
    property raceNumber: integer read FRaceNumber;
    property CupCategory: BYTE read FCupCategory;
    property CurrentDriverIndex: BYTE read FCurrentDriverIndex;
    property Nationality: TksNationalityEnum read FNationality;
  end;

type
  TksLapInfo = record
    laptimeMS: Int32;
    splitsMS: array of Int32;
    carIndex: UInt16;
    driverIndex: UInt16;
    isInvalid: boolean;
    isValidForBest: boolean;
    lapType: TksLapType;
    procedure readFromStream(const strm: TStream);
  end;

type
  TksCarData = record
    carIndex: UInt16;
    driverIndex: UInt16;
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
  TksSessionData = record
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
    BestLapCarIndex: SmallInt;
    BestLapDriverIndex: SmallInt;
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
    CarID: integer;
    procedure readFromStream(const strm: TStream);
  end;

function ReadString(const strm: TStream): string;
procedure WriteString(const strm: TStream; const str: string);

implementation

uses
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
// TksCarData
// ----------------------------------------------------------------------------

procedure TksCarData.readFromStream(const strm: TStream);
begin
  strm.ReadBuffer(carIndex, sizeof(carIndex));
  strm.ReadBuffer(driverIndex, sizeof(driverIndex));
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
  strm.ReadBuffer(laptimeMS, sizeof(laptimeMS));
  strm.ReadBuffer(carIndex, sizeof(carIndex));
  strm.ReadBuffer(driverIndex, sizeof(driverIndex));
  strm.ReadBuffer(splitCount, sizeof(splitCount));
  if (splitCount < 3) then
    SetLength(splitsMS, 3)
  else
    SetLength(splitsMS, splitCount);
  for i := 0 to splitCount - 1 do
    strm.ReadBuffer(splitsMS[i], sizeof(splitsMS[i]));
  for i := splitCount to 2 do
    splitsMS[i] := High(splitsMS[i]);
  strm.ReadBuffer(aux, sizeof(aux));
  isInvalid := aux <> 0;
  strm.ReadBuffer(aux, sizeof(aux));
  isValidForBest := aux <> 0;
  strm.ReadBuffer(aux, sizeof(aux));
  strm.ReadBuffer(splitCount, sizeof(splitCount));
  if (aux <> 0) then
    lapType := TksLapType.Outlap
  else if (splitCount <> 0) then
    lapType := TksLapType.Inlap
  else
    lapType := TksLapType.Regular;
end;

// ----------------------------------------------------------------------------
// TksCarInfo
// ----------------------------------------------------------------------------

class function TksCarInfo.findCarInfo(entryList: TObjectList<TksCarInfo>;
  const carIndex: SmallInt): TksCarInfo;
var
  i: integer;
begin
  Result := nil;
  i := 0;
  while (Result = nil) and (i < entryList.Count) do
    if (entryList.Items[i].carIndex = carIndex) then
      Result := entryList.Items[i]
    else
      inc(i);
end;

class function TksCarInfo.findRaceNumber(entryList: TObjectList<TksCarInfo>;
  const raceNumber: integer): TksCarInfo;
var
  i: integer;
begin
  Result := nil;
  i := 0;
  while (Result = nil) and (i < entryList.Count) do
    if (entryList.Items[i].raceNumber = raceNumber) then
      Result := entryList.Items[i]
    else
      inc(i);
end;

class procedure TksCarInfo.Copy(fromList, toList: TObjectList<TksCarInfo>);
var
  i: integer;
begin
  toList.Clear;
  if (fromList <> nil) then
  begin
    for i := 0 to fromList.Count - 1 do
      toList.Add(fromList.Items[i]);
  end;
end;

constructor TksCarInfo.Create(AcarIndex: SmallInt);
begin
  FCarIndex := AcarIndex;
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
    FDrivers.Add(source.FDrivers[i]);
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
  strm.ReadBuffer(FCarModelType, sizeof(FCarModelType));
  FTeamName := ReadString(strm);
  strm.ReadBuffer(FRaceNumber, sizeof(FRaceNumber));
  strm.ReadBuffer(FCupCategory, sizeof(FCupCategory));
  strm.ReadBuffer(FCurrentDriverIndex, sizeof(FCurrentDriverIndex));
  strm.ReadBuffer(FNationality, sizeof(FNationality));
  strm.ReadBuffer(DriverCount, sizeof(DriverCount));
  for i := 0 to DriverCount - 1 do
  begin
    driverInfo := TKsDriverInfo.Create;
    driverInfo.FFirstName := ReadString(strm);
    driverInfo.FLastName := ReadString(strm);
    driverInfo.FShortName := ReadString(strm);
    strm.ReadBuffer(driverInfo.FCategory, sizeof(driverInfo.FCategory));
    strm.ReadBuffer(driverInfo.FNationality, sizeof(driverInfo.FNationality));
    FDrivers.Add(driverInfo);
  end;
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
  strm.ReadBuffer(eventType,sizeof(EventType));
  messageText := ReadString(strm);
  strm.ReadBuffer(TimeMS,sizeof(TimeMS));
  strm.ReadBuffer(CarID,sizeof(CarID));
end;

end.