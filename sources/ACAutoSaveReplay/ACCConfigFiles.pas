unit ACCConfigFiles;

{ *******************************************************

  Auto save replay for AC/ACC

  Sends the "save replay" key to ACC at regular intervals

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

  [2021-04-04] First implementation

  ******************************************************* }

interface

type
  TBroadcastingJson = record
    port: integer;
    connectionPassword: string;
  end;

  TReplayJson = record
    maxTimeReplaySeconds: integer;
    autoSaveEnabled: boolean;
  end;

function GetBroadcastingCfg: TBroadcastingJson;
function GetReplayCfg: TReplayJson;

implementation

uses
  ACAutosave_strings,
  WinFolders,
  System.IOUtils,
  System.SysUtils,
  System.JSON;

const
  BROADCASTING_PATH = 'Assetto Corsa Competizione\Config\broadcasting.json';
  REPLAY_PATH = 'Assetto Corsa Competizione\Config\replay.json';

function GetJSON(const filename: string): TJSONValue;
var
  data: TBytes;
  text: string;
  l: integer;
  encoding: TEncoding;
begin
  data := TFile.ReadAllBytes(filename);
  l := TEncoding.GetBufferEncoding(data, encoding);
  if (l = 0) and (char(data[1]) = #0) then
    // UTF-16 encoding without BOM
    text := TEncoding.Unicode.GetString(data)
  else
    // Use BOM
    text := encoding.GetString(data, l, Length(data) - l);
  Result := TJsonObject.ParseJSONValue(text, false, true);
end;

function GetBroadcastingCfg: TBroadcastingJson;
var
  root, item: TJSONValue;
  filename: string;
begin
  filename := IncludeTrailingPathDelimiter(TWindowsFolder.myDocuments) +
    BROADCASTING_PATH;
  root := GetJSON(filename);
  item := root.FindValue('updListenerPort');
  if (item <> nil) and (item is TJSONNumber) then
    Result.port := (item as TJSONNumber).AsInt
  else
    raise Exception.Create(str_jsonFieldNotFound + ': updListenerPort');
  item := root.FindValue('connectionPassword');
  if (item <> nil) and (item is TJSONString) then
    Result.connectionPassword := (item as TJSONString).Value
  else
    raise Exception.Create(str_jsonFieldNotFound + ': connectionPassword');
end;

function GetReplayCfg: TReplayJson;
var
  root, item: TJSONValue;
  filename: string;
begin
  filename := IncludeTrailingPathDelimiter(TWindowsFolder.myDocuments) +
    REPLAY_PATH;
  root := GetJSON(filename);
  item := root.FindValue('maxTimeReplaySeconds');
  if (item <> nil) and (item is TJSONNumber) then
    Result.maxTimeReplaySeconds := (item as TJSONNumber).AsInt
  else
    raise Exception.Create(str_jsonFieldNotFound + ': maxTimeReplaySeconds');
  item := root.FindValue('autoSaveEnabled');
  if (item <> nil) and (item is TJSONNumber) then
    Result.autoSaveEnabled := ((item as TJSONNumber).AsInt > 0)
  else
    Result.autoSaveEnabled := false;
end;

end.
