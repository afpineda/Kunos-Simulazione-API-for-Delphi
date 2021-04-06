program ACAutoSaveReplay;

uses
  Vcl.Forms,
  ACAutoSave_main in '..\sources\ACAutoSaveReplay\ACAutoSave_main.pas' {Form_main},
  ksBroadcasting.Data in '..\sources\ksBroadcasting.Data.pas',
  ksBroadcasting in '..\sources\ksBroadcasting.pas',
  ksBroadcasting.UDP in '..\sources\ksBroadcasting.UDP.pas',
  ACAutoSave_protocol in '..\sources\ACAutoSaveReplay\ACAutoSave_protocol.pas',
  ksBroadcasting.Protocol in '..\sources\ksBroadcasting.Protocol.pas',
  WinFolders in '..\sources\Lib\WinFolders.pas',
  ACCConfigFiles in '..\sources\ACAutoSaveReplay\ACCConfigFiles.pas',
  ACAutoSave_strings in '..\sources\ACAutoSaveReplay\ACAutoSave_strings.pas',
  ACAutoSave_processes in '..\sources\ACAutoSaveReplay\ACAutoSave_processes.pas',
  I18NUtils in '..\sources\Lib\I18NUtils.pas',
  ACAutoSave_spanish in '..\sources\ACAutoSaveReplay\ACAutoSave_spanish.pas',
  ACCkeyboard in '..\sources\ACAutoSaveReplay\ACCkeyboard.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Auto save replay';
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
