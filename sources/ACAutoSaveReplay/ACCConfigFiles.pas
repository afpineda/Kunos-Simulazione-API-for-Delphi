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

  [2021-04-06] Added support for any configured
  key to save replay

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

  TSaveReplayControlCfg = record
    available: boolean;
    key: string;
    bShift: boolean;
    bCtrl: boolean;
    bAlt: boolean;
  end;

function GetBroadcastingCfg: TBroadcastingJson;
function GetReplayCfg: TReplayJson;
function GetSaveReplayControlCfg: TSaveReplayControlCfg;

implementation

uses
  ACAutosave_strings,
  WinFolders,
  System.IOUtils,
  System.SysUtils,
  System.Generics.Collections,
  System.JSON;

const
  BROADCASTING_PATH = 'Assetto Corsa Competizione\Config\broadcasting.json';
  REPLAY_PATH = 'Assetto Corsa Competizione\Config\replay.json';
  CONTROLS_PATH = 'Assetto Corsa Competizione\Config\controls.json';

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

function GetSaveReplayControlCfg: TSaveReplayControlCfg;
var
  root, item: TJSONValue;
  jsonarray: TJSONArray;
  jsonobj: TJsonObject;
  jsonstr: TJSONString;
  filename: string;
  i: integer;
begin
  Result.available := false;
  filename := IncludeTrailingPathDelimiter(TWindowsFolder.myDocuments) +
    CONTROLS_PATH;
  root := GetJSON(filename);
  item := root.FindValue('keyboardSettings.raceCommandButtonList');
  if (item <> nil) and (item is TJSONArray) then
  begin
    jsonarray := (item as TJSONArray);
    for i := 0 to jsonarray.Count - 1 do
      if (jsonarray.Items[i] is TJsonObject) then
      begin
        jsonobj := (jsonarray.Items[i] as TJsonObject);
        item := jsonobj.GetValue('actionType');
        if (item = nil) then
          continue;
        jsonstr := (item as TJSONString);
        if (jsonstr.Value = 'SaveReplay') then
        begin
          item := jsonobj.GetValue('key');
          if (item <> nil) and (item is TJsonObject) then
          begin
            jsonobj := item as TJsonObject;
            item := jsonobj.FindValue('key');
            if item <> nil then
              Result.key := item.AsType<TJSONString>.Value
            else
              raise Exception.Create(str_jsonFieldNotFound + ': key');
            item := jsonobj.FindValue('bShift');
            if item <> nil then
              Result.bShift := item.AsType<TJSONBool>.AsBoolean
            else
              raise Exception.Create(str_jsonFieldNotFound + ': bShift');
            item := jsonobj.FindValue('bCtrl');
            if item <> nil then
              Result.bCtrl := item.AsType<TJSONBool>.AsBoolean
            else
              raise Exception.Create(str_jsonFieldNotFound + ': bCtrl');
            item := jsonobj.FindValue('bAlt');
            if item <> nil then
              Result.bAlt := item.AsType<TJSONBool>.AsBoolean
            else
              raise Exception.Create(str_jsonFieldNotFound + ': bAlt');
            Result.available := true;
            Exit;
            item := jsonobj.FindValue('bCmd');
            if (item <> nil) and item.AsType<TJSONBool>.AsBoolean then
              raise Exception.CreateFmt(str_unsupportedKey_error,
                ['bCmd modifier']);
            Result.available := true;
            Exit;
          end;
        end;
      end;
  end;
end;

end.
