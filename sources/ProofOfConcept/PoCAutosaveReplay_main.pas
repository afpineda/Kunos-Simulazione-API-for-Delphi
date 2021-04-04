unit PoCAutosaveReplay_main;


// SENDINPUT:
// https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-sendinput

// SetForegroundWindow
// https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-setforegroundwindow

// Findwindow
// https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-findwindowa

// EnumWindows
// https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-enumwindows
// https://stackoverflow.com/questions/1888863/how-to-get-main-window-handle-from-process-id

// GetWindowThreadProcessId
// https://docs.microsoft.com/en-us/windows/win32/api/winuser/nf-winuser-getwindowthreadprocessid

interface

uses
  PocAutosaveReplay_protocol,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Grids, Vcl.ValEdit;

const
  ACC_EXE = 'AC2-Win64-Shipping.exe';
  ACC_CFG_PATH = 'Assetto Corsa Competizione\Config';

type
  TForm_main = class(TForm)
    PC_main: TPageControl;
    Page_log: TTabSheet;
    Memo_log: TMemo;
    Timer_main: TTimer;
    Btn_Reg: TButton;
    VE_main: TValueListEditor;
    Page_sendinput: TTabSheet;
    Btn_SendToNotepad: TButton;
    Btn_SendToAcc: TButton;
    Page_ACCconfig: TTabSheet;
    Btn_ReadBrdCfg: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Btn_RegClick(Sender: TObject);
    procedure Timer_mainTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Btn_SendToNotepadClick(Sender: TObject);
    procedure Btn_SendToAccClick(Sender: TObject);
    procedure Btn_ReadBrdCfgClick(Sender: TObject);
  private
    { Private declarations }
    Protocol: TAutosaveReplayProtocol;
    procedure OnSaveReplay(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  System.UITypes,
  WinFolders,
  System.JSON,
  System.JSON.Readers,
  System.IOUtils,
  StrUtils,
  PoCAutosaveReplay_proc;

{$R *.dfm}

procedure TForm_main.OnSaveReplay(Sender: TObject);
begin
  Memo_log.Lines.Add('Autosave');
end;

procedure TForm_main.Timer_mainTimer(Sender: TObject);
begin
  VE_main.Strings.Clear;
  case Protocol.State of
    NotRegistered:
      VE_main.Strings.Add('Sate=Not registered');
  else
    begin
      VE_main.Strings.Add('State=Registered');
      VE_main.Strings.Add('SessionTime=' + Protocol.LiveSessionTime.ToString);
      VE_main.Strings.Add('SessionType=' + integer(Protocol.LiveSessionType)
        .ToString);
      VE_main.Strings.Add('Phase=' + integer(Protocol.LiveSessionPhase)
        .ToString);
    end;

  end;

end;

procedure TForm_main.Btn_ReadBrdCfgClick(Sender: TObject);
var
  filename: string;
  JSON, a: TJsonValue;
  // data: TStringList;
  text: string;
  reader: TJsonTextReader;
  strreader: TStringReader;
  b: TBytes;
  l: integer;
  enc: TEncoding;
begin
  filename := IncludeTrailingPathDelimiter(TWindowsFolder.myDocuments) +
    IncludeTrailingPathDelimiter(ACC_CFG_PATH) + 'broadcasting.json';
  Memo_log.Lines.Add('CFG:');
  Memo_log.Lines.Add(filename);

  b := TFile.ReadAllBytes(filename);
  enc := nil;
  l := TEncoding.GetBufferEncoding(b, enc);
  text := enc.GetString(b, l, Length(b) - l);
  if (l = 0) and (char(b[1]) = #0) and (char(b[3]) = #0) then
    text := TEncoding.Unicode.GetString(b);
  // text := TEncoding.Unicode.GetString(b);
  JSON := TJsonObject.ParseJSONValue(text, false, true);
  a := JSON.FindValue('updListenerPort');
  if (a <> nil) then
    Memo_log.Lines.Add(a.ToString);
end;

procedure TForm_main.Btn_RegClick(Sender: TObject);
begin
  if Protocol.Registered then
    Protocol.Stop
  else
  begin
    Protocol.SaveOnEndOfSession := true;
    Protocol.Start(9001, 'kagarro');
  end;
end;

procedure TForm_main.Btn_SendToAccClick(Sender: TObject);
begin
  SendInputToExe(ACC_EXE, vkEscape);
end;

procedure TForm_main.Btn_SendToNotepadClick(Sender: TObject);
begin
  SendInputToExe('notepad.exe', vkM);
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  Protocol := TAutosaveReplayProtocol.Create;
  Protocol.OnSaveReplay := OnSaveReplay;
  Memo_log.Lines.Clear;
{$IFDEF DEBUG}
  Memo_log.Lines.Add('DEBUG: no actual saving');
{$ENDIF}
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  Protocol.Free;
end;

end.
