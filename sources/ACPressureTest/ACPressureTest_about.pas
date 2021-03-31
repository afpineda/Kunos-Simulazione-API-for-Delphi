unit ACPressureTest_about;

{ *******************************************************

  Automated pressure test for AC/ACC

  Computes correct cold pressures to achieve target
  hot pressures

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

  [2021-03-17] First implementation

  [2021-03-30] Added "JJ Endurance" Logo and link

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls;

type
  TForm_About = class(TForm)
    Lbl_build: TLabel;
    Panel_text: TPanel;
    Lbl_C: TLabel;
    Lbl_notForSale: TLabel;
    Panel_logo: TPanel;
    Img_logo: TImage;
    Link_JJ: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Link_JJClick(Sender: TObject);
    procedure Link_JJMouseEnter(Sender: TObject);
    procedure Link_JJMouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_About: TForm_About;

implementation

uses
  ShellAPI,
  ACPressureTest_strings,
  I18NUtils;

{$R *.dfm}

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
{$IFDEF DEBUG}
        Result := Format('Debug Version %d.%d.%d', [HiWord(dwFileVersionMS),
          LoWord(dwFileVersionMS), HiWord(dwFileVersionLS)]); // Build
{$ELSE}
        Result := Format('Version %d.%d.%d', [HiWord(dwFileVersionMS),
          LoWord(dwFileVersionMS), HiWord(dwFileVersionLS)]); // Build
{$ENDIF}
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

procedure TForm_About.FormCreate(Sender: TObject);
begin
  Translate(self);
  try
    Lbl_build.Caption := GetVersion(Application.ExeName);
  except
    Lbl_build.Caption := '';
  end;
end;

procedure TForm_About.Link_JJClick(Sender: TObject);
begin
  ShellAPI.ShellExecute(0, 'Open', PChar(str_sponsor_link), PChar(''), nil,
    SW_SHOWNORMAL);
end;

procedure TForm_About.Link_JJMouseEnter(Sender: TObject);
begin
  Link_JJ.Font.Style := [fsUnderline];
end;

procedure TForm_About.Link_JJMouseLeave(Sender: TObject);
begin
  Link_JJ.Font.Style := [];
end;

end.
