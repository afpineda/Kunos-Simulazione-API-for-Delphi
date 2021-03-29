program PoCUDPImpl1;

uses
  Vcl.Forms,
  PoCUDPImpl1_main in '..\..\sources\ProofOfConcept\PoCUDPImpl1_main.pas' {Form_main},
  ksBroadcasting.Data in '..\..\sources\ksBroadcasting.Data.pas',
  ksBroadcasting in '..\..\sources\ksBroadcasting.pas',
  ksBroadcasting.UDP in '..\..\sources\ksBroadcasting.UDP.pas',
  ksBroadcasting.Protocol in '..\..\sources\ksBroadcasting.Protocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
