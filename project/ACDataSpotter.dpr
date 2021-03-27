program ACDataSpotter;

uses
  Vcl.Forms,
  ACDataSpotter_main in '..\sources\ACDAtaSpotter\ACDataSpotter_main.pas' {Form_main},
  ksSharedMem in '..\sources\ksSharedMem.pas',
  ksSharedMem.Utils in '..\sources\ksSharedMem.Utils.pas',
  ksSharedMem.Data in '..\sources\ksSharedMem.Data.pas',
  ksSharedMem.Events in '..\sources\ksSharedMem.Events.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
