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

type
  TForm_main = class(TForm)
    PC_main: TPageControl;
    Page_log: TTabSheet;
    Memo_log: TMemo;
    Timer_main: TTimer;
    Btn_Reg: TButton;
    VE_main: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure Btn_RegClick(Sender: TObject);
    procedure Timer_mainTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
      VE_main.Strings.Add('Phase=' + integer(Protocol.LiveSessionPhase).ToString);
    end;

  end;

end;

procedure TForm_main.Btn_RegClick(Sender: TObject);
begin
  if Protocol.Registered then
    Protocol.Unregister
  else
  begin
    Protocol.SaveOnEndOfSession := true;
    Protocol.Register(9001, 'kagarro', 1);
  end;
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
