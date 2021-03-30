unit ksSharedMem;

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

{
  SUMMARY:

  - TksLiveData: read live AC/ACC shared memory data
  - TksMemorySnapshot: store a snapshot of AC/ACC shared memory
}

uses
  ksSharedMem.Data;

type
  TksLiveData = class
    {
      PURPOUSE:
      To access shared memory data from Assetto Corsa or Asseto
      Corsa Competizione, as far as one of them is running on
      the same computer.

      GENERAL USAGE:
      - Create a single instance
      - Check "isAvailable" (as many times as you wish: idempotent operation).
      - read data using "*Data" properties
      - Destroy instance

      EXAMPLE:
      ks := TksLiveData.Create
      while (not ks.isAvailable) do
      sleep(1000);
      try
      WriteLn(string(ks.getStaticData^.smVersion));
      finally
      ks.Free;
      end;

      NOTES ON "*Data" properties:
      - Nil if the corresponding shared memory file is not available.
      Only a call to "isAvailable" may change this result.

      OTHER NOTES:
      - Shared memory files will be kept open even if the game is not running
      anymore. No memory access exceptions will be raisen and you will read the
      same data again and again. This is Windows' behavior and can not be changed.
      - You may detect such a situation when "packetId" remains the same in
      a certain time lapse.
      - In ACC, all fields of physics data are set to zero when "status" is
      different to AC_LIVE. Also if the game is not running anymore.
    }
  protected
    hPhysicsMap: THandle;
    hGraphicsMap: THandle;
    hStaticMap: THandle;
    mappedPhysicsData: PSPageFilePhysics;
    mappedGraphicsData: PSPageFileGraphic;
    mappedStaticData: PSPageFileStatic;

  public
    constructor Create;
    destructor Destroy; override;
    function isAvailable: boolean; virtual;

    property PhysicsData: PSPageFilePhysics read mappedPhysicsData;
    property GraphicsData: PSPageFileGraphic read mappedGraphicsData;
    property StaticData: PSPageFileStatic read mappedStaticData;
  end;

type
  TksMemorySnapshot = class(TksLiveData)
    {
      PURPOUSE:
      To hold a snapshot of shared memory data from Assetto Corsa or
      Asseto Corsa Competizione.

      NOTES:
      - properties will return garbage if "isAvailable" is false
      - "isAvailable" will return false until first call to "CopyFrom".
      - properties will return the same data until "CopyFrom" is called again.
    }

  private
    FPhysicsData: SPageFilePhysics;
    FGraphicsData: SPageFileGraphic;
    FStaticData: SPageFileStatic;
    FtimeStamp: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CopyFrom(source: TksLiveData); overload;
    function isAvailable: boolean; override;
    function elapsedTimeMs: int64;

    property timeStamp: TDateTime read FtimeStamp;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  WinApi.Windows;

// ----------------------------------------------------------------------------
// Auxiliary function
// ----------------------------------------------------------------------------

function OpenAux(const filename: string; const filesize: DWORD;
  out mapHandle: THandle; out dataPointer: Pointer): boolean;
begin
  Result := false;
  dataPointer := nil;
  mapHandle := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, PWideChar(filename));
  if (mapHandle <> 0) then
  begin
    // dataPointer := MapViewOfFile(mapHandle, FILE_MAP_READ, 0, 0, filesize);
    dataPointer := MapViewOfFile(mapHandle, FILE_MAP_WRITE, 0, 0, filesize);
    if (dataPointer = nil) then
    begin
      CloseHandle(mapHandle);
      mapHandle := 0;
    end
    else
      Result := true;
  end;
end;

// ----------------------------------------------------------------------------
// TksLiveData
// ----------------------------------------------------------------------------

constructor TksLiveData.Create;
begin
  mappedPhysicsData := nil;
  mappedGraphicsData := nil;
  mappedStaticData := nil;
  hPhysicsMap := 0;
  hGraphicsMap := 0;
  hStaticMap := 0;
end;

destructor TksLiveData.Destroy;
begin
  if (mappedPhysicsData <> nil) then
    UnmapViewOfFile(mappedPhysicsData);
  if (hPhysicsMap <> 0) then
    CloseHandle(hPhysicsMap);
  if (mappedGraphicsData <> nil) then
    UnmapViewOfFile(mappedGraphicsData);
  if (hGraphicsMap <> 0) then
    CloseHandle(hGraphicsMap);
  if (mappedStaticData <> nil) then
    UnmapViewOfFile(mappedStaticData);
  if (hStaticMap <> 0) then
    CloseHandle(hStaticMap);
  inherited Destroy;
end;

function TksLiveData.isAvailable: boolean;
begin
  if (mappedPhysicsData = nil) then
    OpenAux(ksPhysicsMemFile, sizeof(SPageFilePhysics), hPhysicsMap,
      Pointer(mappedPhysicsData));
  if (mappedGraphicsData = nil) then
    OpenAux(ksGraphicsMemFile, sizeof(SPageFileGraphic), hGraphicsMap,
      Pointer(mappedGraphicsData));
  if (mappedStaticData = nil) then
    OpenAux(ksStaticMemFile, sizeof(SPageFileStatic), hStaticMap,
      Pointer(mappedStaticData));
  Result := (mappedPhysicsData <> nil) and (mappedGraphicsData <> nil) and
    (mappedStaticData <> nil);
end;

// ----------------------------------------------------------------------------
// TksMemorySnapshot
// ----------------------------------------------------------------------------

constructor TksMemorySnapshot.Create;
begin
  mappedPhysicsData := @FPhysicsData;
  mappedGraphicsData := @FGraphicsData;
  mappedStaticData := @FStaticData;
  FtimeStamp := Now();
  FGraphicsData.status := TACStatus.AC_OFF;
  hStaticMap := THandle(1); // Used as an initialization flag
  hPhysicsMap := 0;
  hGraphicsMap := 0;
  // Do not call inherited constructor
end;

destructor TksMemorySnapshot.Destroy;
begin
  // Do nothing. This is OK. Do not call inherited destructor.
end;

function TksMemorySnapshot.isAvailable: boolean;
begin
  Result := (hStaticMap = 0);
end;

procedure TksMemorySnapshot.CopyFrom(source: TksLiveData);
begin
  if (source <> nil) and (source.mappedStaticData <> nil) then
  begin
    hStaticMap := 0;
    CopyMemory(mappedPhysicsData, source.mappedPhysicsData,
      sizeof(SPageFilePhysics));
    CopyMemory(mappedGraphicsData, source.mappedGraphicsData,
      sizeof(SPageFileGraphic));
    CopyMemory(mappedStaticData, source.mappedStaticData,
      sizeof(SPageFileStatic));
    FtimeStamp := Now();
  end;
end;

function TksMemorySnapshot.elapsedTimeMs: int64;
begin
  Result := MilliSecondsBetween(Now, FtimeStamp);
end;

// ----------------------------------------------------------------------------

end.
