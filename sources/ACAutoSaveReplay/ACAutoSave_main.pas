unit ACAutoSave_main;

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

uses
  ACAutoSave_protocol,
  ACCConfigFiles,
  ACCkeyboard,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Menus;

type
  TForm_main = class(TForm)
    Memo_log: TMemo;
    SB_main: TStatusBar;
    Menu_main: TMainMenu;
    Menu_Action: TMenuItem;
    Menu_About: TMenuItem;
    Menu_ClearLog: TMenuItem;
    Menu_disable: TMenuItem;
    N1: TMenuItem;
    Menu_saveNow: TMenuItem;
    Menu_Checks: TMenuItem;
    Menu_ShowState: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Menu_AboutClick(Sender: TObject);
    procedure Menu_ClearLogClick(Sender: TObject);
    procedure Menu_disableClick(Sender: TObject);
    procedure Menu_saveNowClick(Sender: TObject);
    procedure Menu_ShowStateClick(Sender: TObject);
  private
    Protocol: TAutosaveReplayProtocol;
    brdcastCfg: TBroadcastingJson;
    replayCfg: TReplayJson;
    saveReplayKey: TSaveReplayControlCfg;
    saveKeyInput: TInputArray;
    procedure LaunchProtocol;
    procedure LoadGameConfig;
    procedure OnLaunchFailure;
    procedure Log(const txt: string); inline;
    procedure SetStatusBar(const txt: string); inline;
    procedure OnSaveReplay(Sender: TObject);
    procedure OnStateChange(Sender: TObject);
    procedure OnRejected(Sender: TObject; const msg: string);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  ksBroadcasting.Data,
  ACAutoSave_processes,
  ACAutoSave_strings, I18NUtils;

{$R *.dfm}
// ---- AUXILIARY

procedure TForm_main.Log(const txt: string);
begin
  Memo_log.Lines.Add(txt);
  SendMessage(Memo_log.Handle, EM_LINESCROLL, 0, Memo_log.Lines.Count);
end;

procedure TForm_main.SetStatusBar(const txt: string);
begin
  SB_main.Panels[0].Text := txt;
end;

function FormatSessionTime(time: single): string;
var
  t, hh, mm, ss: int64;
begin
  t := Trunc(time / 1000);
  ss := t mod 60;
  mm := (t div 60) mod 60;
  hh := (t div 3600) mod 60;
  Result := Format('%.2d:%.2d:%.2d', [hh, mm, ss]);
end;

function FormatSessionType(st: TksRaceSessionType): string;
begin
  case st of
    TksRaceSessionType.Qualifying:
      Result := str_session_qualy;
    TksRaceSessionType.Race:
      Result := str_session_race;
  else
    Result := str_session_other;
  end;
end;

function FormatSessionPhase(sp: TksSessionPhase): string;
begin
  case sp of
    TksSessionPhase.NONE, TksSessionPhase.Starting,
      TksSessionPhase.PreFormation, TksSessionPhase.FormationLap,
      TksSessionPhase.PreSession:
      Result := str_phase_pre;
    TksSessionPhase.Session:
      Result := str_phase_race;
  else
    Result := str_phase_post;
  end;
end;

procedure TForm_main.LoadGameConfig;
var
  straux: string;
begin
  Log(str_line);
  Log(str_readingGameCfg);

  Log(str_line);
  Log('broadcasting.json');
  brdcastCfg := GetBroadcastingCfg;
  Log(str_ok);
  Log(Format(str_broadcast_cfg_values, [brdcastCfg.Port]));

  Log(str_line);
  Log('replay.json');
  replayCfg := GetReplayCfg;
  Log(str_ok);
  Log(Format(str_replay_cfg_values, [replayCfg.maxTimeReplaySeconds,
    integer(replayCfg.autoSaveEnabled)]));

  Log(str_line);
  Log('controls.json');
  saveReplayKey := GetSaveReplayControlCfg;
  saveKeyInput := GetInputArray(saveReplayKey);
  Log(str_ok);
  if saveReplayKey.available then
  begin
    straux := saveReplayKey.key;
    if saveReplayKey.bCtrl then
      straux := 'Ctrl+' + straux;
    if saveReplayKey.bAlt then
      straux := 'Alt+' + straux;
    if saveReplayKey.bShift then
      straux := 'Shift+' + straux;
    // NOTE: Current implementation does not work when sending key modifiers
    // Sent input is correct, however ACC does not seem to accept it
{$IFNDEF DEBUG}
    if saveReplayKey.bCtrl or saveReplayKey.bAlt or saveReplayKey.bShift then
      raise Exception.CreateFmt(str_unsupportedKey_error, [straux])
    else
{$ENDIF}
      Log(str_key_literal + straux);
  end
  else
    Log(str_defaultKey);
end;

procedure TForm_main.OnLaunchFailure;
begin
  SetStatusBar(str_error);
  Menu_Action.Enabled := false;
end;

procedure TForm_main.LaunchProtocol;
begin
  try
    LoadGameConfig;
    Log(str_line);
    Log(str_start_protocol);
    Protocol.SaveIntervalSeconds := replayCfg.maxTimeReplaySeconds -
      ((Protocol.UpdateIntervalMs div 1000) * 4);
    if (Protocol.SaveIntervalSeconds < 10) then
      Protocol.SaveIntervalSeconds := 10;
    Protocol.SaveOnEndOfSession := not replayCfg.autoSaveEnabled;
    Protocol.Start(brdcastCfg.Port, brdcastCfg.connectionPassword);
    if (Protocol.SaveIntervalSeconds < 600) then
    begin
      Log(str_smallAutoReplay);
      Menu_disable.Click;
    end
    else
      Log(str_ok);
    OnStateChange(Protocol);
  except
    on E: Exception do
    begin
      Log(str_error + ': ' + E.Message);
      OnLaunchFailure;
    end;
  end
end;

const
{$IFDEF DEBUG}
  VER_STR = 'Debug Version %d.%d.%d';
{$ELSE}
  VER_STR = 'Version %d.%d.%d';
{$ENDIF}

function GetVersion(const FileName: string): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PWideChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PWideChar(FileName), 0, VerInfoSize, PVerInfo) and
      VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
      with PVerValue^ do
        Result := Format(VER_STR, [HiWord(dwFileVersionMS),
          LoWord(dwFileVersionMS), HiWord(dwFileVersionLS)]); // Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

// ---- FORM CREATE/DESTROY

procedure TForm_main.FormCreate(Sender: TObject);
begin
  I18NUtils.Translate(self);
  Protocol := TAutosaveReplayProtocol.Create;
  Protocol.OnSaveReplay := OnSaveReplay;
  Protocol.OnStateChangeEvent := OnStateChange;
  Protocol.OnRegistrationRejected := OnRejected;
  Memo_log.Lines.Clear;
  SB_main.Panels[0].Text := str_error;
  Menu_disable.Checked := false;
  SetLength(saveKeyInput, 0);
{$IFDEF DEBUG}
  Log('***** DEBUG VERSION *****');
{$ENDIF}
  LaunchProtocol;
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  Protocol.Free;
end;

// ---- PROTOCOL EVENTS

procedure TForm_main.OnSaveReplay(Sender: TObject);
begin
  Log(str_line);
  Log(str_autosaving);
  if Protocol.State = InProgress then
    Log(FormatSessionTime(Protocol.LiveSessionTime));
  if (Menu_disable.Checked) then
  begin
    Log(str_disable_warning);
  end
  else
  begin
    if SendInputToACC(saveKeyInput) then
      Log(str_ok)
    else
    begin
      Log(str_error);
      Beep;
    end;
  end;
end;

procedure TForm_main.OnStateChange(Sender: TObject);
var
  status: string;
begin
  if Menu_disable.Checked then
    status := str_disable_warning
  else
    case Protocol.State of
      Inactive:
        status := str_state_notRegistered;
      Waiting:
        status := str_state_waiting;
      InProgress:
        begin
          status := str_state_inProgress;
        end;
    end;
  SetStatusBar(status);
end;

procedure TForm_main.OnRejected(Sender: TObject; const msg: string);
begin
  // NOTE: should not happen
  Protocol.Stop;
  Log(str_line);
  Log(str_error);
  Log(str_rejected);
  Log(msg);
  SetStatusBar(str_error);
end;

// ---- Menu

procedure TForm_main.Menu_AboutClick(Sender: TObject);
begin
  Log(str_line);
  Log(str_copyright_notice);
  Log(str_copyright_notice1);
  Log(str_copyright_notice2);
  Log(GetVersion(Application.ExeName));
end;

procedure TForm_main.Menu_ClearLogClick(Sender: TObject);
begin
  Memo_log.Lines.Clear;
end;

procedure TForm_main.Menu_disableClick(Sender: TObject);
begin
  Menu_disable.Checked := not Menu_disable.Checked;
  Log(str_line);
  if (Menu_disable.Checked) then
  begin
    Log(str_disable_warning);
  end
  else
  begin
    Log(str_enable_warning);
  end;
  OnStateChange(Protocol);
end;

procedure TForm_main.Menu_saveNowClick(Sender: TObject);
begin
  OnSaveReplay(Protocol);
end;

procedure TForm_main.Menu_ShowStateClick(Sender: TObject);
begin
  Log(str_line);
  case Protocol.State of
    Inactive:
      begin
        Log(str_state_notRegistered);
      end;
  else
    begin
      Log(FormatSessionTime(Protocol.LiveSessionTime));
      Log(FormatSessionType(Protocol.LiveSessionType));
      Log(FormatSessionPhase(Protocol.LiveSessionPhase));
    end;
  end;
end;

end.
