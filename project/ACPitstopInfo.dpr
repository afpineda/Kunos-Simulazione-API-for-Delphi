program ACPitstopInfo;

uses
  Vcl.Forms,
  ACPitstopInfo_main in '..\sources\ACPitstopInfo\ACPitstopInfo_main.pas' {Form_main};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
