unit ACAutoSave_processes;

{ *******************************************************

  Auto save replay for AC/ACC

  Sends the "save replay" key to ACC

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

  [2021-04-04] First implementation

  [2021-04-06] Added support for any configured
  key to save replay

  ******************************************************* }

interface

uses
  ACCKeyboard,
  ACCConfigFiles,
  system.SysUtils,
  WinApi.Windows;

function SendInputToACC(const input: TInputArray): boolean;
function IsACCRunning: boolean;

implementation

uses
  WinApi.psapi,
  system.Classes,
  system.IOUtils;

const
  ACC_EXE = 'AC2-Win64-Shipping.exe';

function GetExeName(Handle: THandle): string;
var
  size: DWORD;
begin
  SetLength(Result, MAX_PATH);
  size := GetModuleFileNameExW(Handle, 0, PChar(Result), MAX_PATH - 1);
  if (size > 0) then
  begin
    SetLength(Result, size);
  end
  else
    Result := '';
end;

type
  TInternalEnumerator = record
    PID: DWORD;
    Result: HWND;
  end;

  PInternalEnumerator = ^TInternalEnumerator;

function enumerator(wHandle: HWND; res: PInternalEnumerator): BOOL; stdcall;
var
  winProcID: DWORD;
begin
  GetWindowThreadProcessID(wHandle, winProcID);
  Result := (winProcID <> res^.PID);
  if (not Result) then
    res^.Result := wHandle;
end;

function GetWindowHandleOfPID(const PID: DWORD): HWND;
var
  enumdata: TInternalEnumerator;
begin
  enumdata.Result := 0;
  enumdata.PID := PID;
  EnumWindows(@enumerator, LPARAM(@enumdata));
  Result := enumdata.Result;
end;

function FindWindowHandle(const findExeName: string): HWND;
var
  parray: array of DWORD;
  count: cardinal;
  hProc: THandle;
  name: string;
begin
  // determine size of process' id array
  SetLength(parray, 256);
  count := 0;
  while (Length(parray) < 16383) and
    (not WinApi.psapi.EnumProcesses(@parray[0], Length(parray) * sizeof(DWORD),
    count)) do
    SetLength(parray, Length(parray) * 2);

  // enumerate
  count := count div sizeof(DWORD);
  while (count > 0) do
  begin
    hProc := OpenProcess(PROCESS_QUERY_INFORMATION, true, parray[count]);
    if (hProc <> 0) then
    begin
      name := GetExeName(hProc);
      name := TPath.GetFileName(name);
      if (CompareText(name, findExeName) = 0) then
      begin
        Result := GetWindowHandleOfPID(parray[count]);
        CloseHandle(hProc);
        Exit;
      end;
      CloseHandle(hProc);
    end;
    dec(count);
  end; // while
  Result := 0;
end;

function SendInputToExe(const ExeName: string;
  const input: TInputArray): boolean;
var
  Handle: HWND;
begin
  Result := false;
  Handle := FindWindowHandle(TPath.GetFileName(ExeName));
  if (Handle <> 0) and (Length(input) > 0) then
  begin
    SetForegroundWindow(Handle);
    Result := SendInput(Length(input), input[0], sizeof(TInput))
      = Length(input);
  end;
end;

function IsACCRunning: boolean;
begin
  Result := FindWindowHandle(ACC_EXE) <> 0;
end;

function SendInputToACC(const input: TInputArray): boolean;
begin
{$IFDEF DEBUG}
  Result := SendInputToExe('notepad.exe', input);
{$ELSE}
  Result := SendInputToExe(ACC_EXE, input);
{$ENDIF}
end;



end.
