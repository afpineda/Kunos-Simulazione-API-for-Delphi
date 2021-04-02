unit PoCAutosaveReplay_proc;

interface

uses
  // system.Generics.Collections,
  system.SysUtils,
  // WinApi.tlhelp32,
  WinApi.Windows;

function FindWindowHandle(const findExeName: string): HWND;
function SendInputToExe(const ExeName: string; vkey: DWORD): boolean;

implementation

uses
  WinApi.psapi,
  system.Classes,
  system.IOUtils;

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

function SendInputToExe(const ExeName: string; vkey: DWORD): boolean;
var
  Handle: HWND;
  inp: array [0 .. 1] of TInput;
begin
  Result := false;
  Handle := FindWindowHandle(TPath.GetFileName(ExeName));
  if (Handle <> 0) then
  begin
    inp[0].Itype := 1;
    inp[0].ki.wVk := vkey;
    inp[0].ki.wScan := 0;
    inp[0].ki.dwFlags := 0;
    inp[1] := inp[0];
    inp[1].ki.dwFlags := KEYEVENTF_KEYUP;

    SetForegroundWindow(Handle);
    Result := SendInput(Length(inp), inp[0], Length(inp) * sizeof(TInput))
      = Length(inp);
  end;
end;

end.
