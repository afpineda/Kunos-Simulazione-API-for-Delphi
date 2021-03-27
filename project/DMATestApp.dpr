program DMATestApp;

uses
  Vcl.Forms,
  DMATestApp_main in '..\sources\DMATestApp\DMATestApp_main.pas' {Form_main},
  ksSharedMem.Data in '..\sources\ksSharedMem.Data.pas',
  ksSharedMem in '..\sources\ksSharedMem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
