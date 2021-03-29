program BroadcastTestApp;

uses
  Vcl.Forms,
  BroadcastTestApp_main in '..\sources\BroadcastTestApp\BroadcastTestApp_main.pas' {Form_main},
  ksBroadcasting in '..\sources\ksBroadcasting.pas',
  ksBroadcasting.Data in '..\sources\ksBroadcasting.Data.pas',
  ksBroadcasting.UDP in '..\sources\ksBroadcasting.UDP.pas',
  ksBroadcasting.Protocol in '..\sources\ksBroadcasting.Protocol.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
