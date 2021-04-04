program PoCAutosaveReplay;

uses
  Vcl.Forms,
  PoCAutosaveReplay_main in '..\..\sources\ProofOfConcept\PoCAutosaveReplay_main.pas' {Form_main},
  PoCAutosaveReplay_proc in '..\..\sources\ProofOfConcept\PoCAutosaveReplay_proc.pas',
  ksBroadcasting.Data in '..\..\sources\ksBroadcasting.Data.pas',
  ksBroadcasting in '..\..\sources\ksBroadcasting.pas',
  ksBroadcasting.UDP in '..\..\sources\ksBroadcasting.UDP.pas',
  PoCAutosaveReplay_protocol in '..\..\sources\ProofOfConcept\PoCAutosaveReplay_protocol.pas',
  ksBroadcasting.Protocol in '..\..\sources\ksBroadcasting.Protocol.pas',
  WinFolders in '..\..\sources\Lib\WinFolders.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
