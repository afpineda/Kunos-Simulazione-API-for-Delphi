unit ACCConfigWatcher;
{ *******************************************************

  Auto save replay for AC/ACC

  Watch for changes at certain ACC's config files

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

  [2021-04-08] First implementation

  ******************************************************* }

interface

uses
  System.Classes;

type
  TACCConfigWatcher = class(TThread)
  private
    FOnChangeEvent: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    procedure Kill;
    property OnChange: TNotifyEvent read FOnChangeEvent write FOnChangeEvent;
  end;

implementation

uses
  // SysUtils,
  ACCConfigFiles,
  WinApi.Windows;

type
  TFILE_NOTIFY_INFORMATION = packed record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: WideChar;
  end;

  PFILE_NOTIFY_INFORMATION = ^TFILE_NOTIFY_INFORMATION;

function TestACCConfigChange: boolean;
const
  BufSize = 32 * 1024;
var
  notifyInfo: PFILE_NOTIFY_INFORMATION;
  FolderHandle: THandle;
  folderName, FileName: string;
  bytesCount: DWORD;
begin
  Result := false;
  folderName := GetACCConfigPath;
  FolderHandle := CreateFile(PChar(folderName), FILE_LIST_DIRECTORY or
    GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil,
    OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  if (FolderHandle = INVALID_HANDLE_VALUE) then
    Exit;
  GetMem(notifyInfo, BufSize);
  try
    if ReadDirectoryChanges(FolderHandle, notifyInfo, BufSize, false,
      FILE_NOTIFY_CHANGE_LAST_WRITE, @bytesCount, nil, nil) and (bytesCount > 0)
    then
    begin
      CloseHandle(FolderHandle);
      repeat
        FileName := WideCharLenToString((@notifyInfo^.FileName),
          notifyInfo^.FileNameLength div sizeof(WIDECHAR));
        notifyInfo := PFILE_NOTIFY_INFORMATION(PByte(notifyInfo) +
          notifyInfo^.NextEntryOffset);
        Result := IsACCConfigFile(FileName);
      until (notifyInfo^.NextEntryOffset = 0) or Result;
    end;
  finally
    FreeMem(notifyInfo);
  end;
end;

procedure TACCConfigWatcher.Kill;
begin
  TerminateThread(self.Handle, 0);
end;

procedure TACCConfigWatcher.Execute;
begin
  while (true) do
    if TestACCConfigChange then
      if (Assigned(FOnChangeEvent)) then
        TThread.Synchronize(nil,
          procedure
          begin
            FOnChangeEvent(self);
          end);
end;

end.
