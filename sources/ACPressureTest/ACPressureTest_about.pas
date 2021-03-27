unit ACPressureTest_about;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm_About = class(TForm)
    Lbl_C: TLabel;
    Lbl_build: TLabel;
    Lbl_notForSale: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form_About: TForm_About;

implementation
  uses
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

end.
