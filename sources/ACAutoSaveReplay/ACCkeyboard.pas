unit ACCkeyboard;
{ *******************************************************

  Auto save replay for AC/ACC

  Sends the "save replay" key to ACC at regular intervals

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

  [2021-04-06] First implementation

  ******************************************************* }

interface

uses
  ACCConfigFiles,
  Windows;

type
  TInputArray = array of TInput;

function GetInputArray(const keycfg: TSaveReplayControlCfg): TInputArray;

implementation

uses
  ACAutoSave_strings,
  System.SysUtils;

function MapKunosKeyToVirtualKey(const keyTag: string): SmallInt;
begin
  if Length(keyTag) = 1 then
    Result := LOWORD(VkKeyScan(PCHAR(keyTag)[0]))
  else if CompareText(keyTag, 'Hyphen') = 0 then
    Result := LOWORD(VkKeyScan('-'))
  else if CompareText(keyTag, 'SpaceBar') = 0 then
    Result := LOWORD(VkKeyScan(' '))
  else if CompareText(keyTag, 'Comma') = 0 then
    Result := LOWORD(VkKeyScan(','))
  else if CompareText(keyTag, 'Period') = 0 then
    Result := LOWORD(VkKeyScan('.'))
  else if CompareText(keyTag, 'LeftAlt') = 0 then
    Result := VK_LMENU
  else if CompareText(keyTag, 'RightAlt') = 0 then
    Result := VK_RMENU
  else if CompareText(keyTag, 'Tab') = 0 then
    Result := VK_TAB
  else if CompareText(keyTag, 'CapsLock') = 0 then
    Result := VK_CAPITAL
  else if CompareText(keyTag, 'Down') = 0 then
    Result := VK_DOWN
  else if CompareText(keyTag, 'Up') = 0 then
    Result := VK_UP
  else if CompareText(keyTag, 'Left') = 0 then
    Result := VK_LEFT
  else if CompareText(keyTag, 'Right') = 0 then
    Result := VK_RIGHT
  else if CompareText(keyTag, 'Delete') = 0 then
    Result := VK_DELETE
  else if CompareText(keyTag, 'Insert') = 0 then
    Result := VK_INSERT
  else if CompareText(keyTag, 'Home') = 0 then
    Result := VK_HOME
  else if CompareText(keyTag, 'End') = 0 then
    Result := VK_END
  else if CompareText(keyTag, 'PageUp') = 0 then
    Result := VK_PRIOR
  else if CompareText(keyTag, 'PageDown') = 0 then
    Result := VK_NEXT
  else if CompareText(keyTag, 'NumLock') = 0 then
    Result := VK_NUMLOCK
  else if CompareText(keyTag, 'ScrollLock') = 0 then
    Result := VK_SCROLL
  else if CompareText(keyTag, 'NumPadZero') = 0 then
    Result := VK_NUMPAD0
  else if CompareText(keyTag, 'NumPadOne') = 0 then
    Result := VK_NUMPAD1
  else if CompareText(keyTag, 'NumPadTwo') = 0 then
    Result := VK_NUMPAD2
  else if CompareText(keyTag, 'NumPadThree') = 0 then
    Result := VK_NUMPAD3
  else if CompareText(keyTag, 'NumPadFour') = 0 then
    Result := VK_NUMPAD4
  else if CompareText(keyTag, 'NumPadFive') = 0 then
    Result := VK_NUMPAD5
  else if CompareText(keyTag, 'NumPadSix') = 0 then
    Result := VK_NUMPAD6
  else if CompareText(keyTag, 'NumPadSeven') = 0 then
    Result := VK_NUMPAD7
  else if CompareText(keyTag, 'NumPadEight') = 0 then
    Result := VK_NUMPAD8
  else if CompareText(keyTag, 'NumPadNine') = 0 then
    Result := VK_NUMPAD9
  else if CompareText(keyTag, 'Add') = 0 then
    Result := VK_ADD
  else if CompareText(keyTag, 'Subtract') = 0 then
    Result := VK_SUBTRACT
  else if CompareText(keyTag, 'Divide') = 0 then
    Result := VK_DIVIDE
  else if CompareText(keyTag, 'Multiply') = 0 then
    Result := VK_MULTIPLY
  else if CompareText(keyTag, 'Decimal') = 0 then
    Result := VK_DECIMAL
  else if CompareText(keyTag, 'Pause') = 0 then
    Result := VK_PAUSE
  else if CompareText(keyTag, 'F1') = 0 then
    Result := VK_F1
  else if CompareText(keyTag, 'F2') = 0 then
    Result := VK_F2
  else if CompareText(keyTag, 'F3') = 0 then
    Result := VK_F3
  else if CompareText(keyTag, 'F4') = 0 then
    Result := VK_F4
  else if CompareText(keyTag, 'F5') = 0 then
    Result := VK_F5
  else if CompareText(keyTag, 'F6') = 0 then
    Result := VK_F6
  else if CompareText(keyTag, 'F7') = 0 then
    Result := VK_F7
  else if CompareText(keyTag, 'F8') = 0 then
    Result := VK_F8
  else if CompareText(keyTag, 'F9') = 0 then
    Result := VK_F9
  else if CompareText(keyTag, 'F10') = 0 then
    Result := VK_F10
  else if CompareText(keyTag, 'F11') = 0 then
    Result := VK_F11
  else if CompareText(keyTag, 'F12') = 0 then
    Result := VK_F12
  else
    raise Exception.CreateFmt(str_unsupportedKey_error, [keyTag]);
end;

 function GetInputArray(const keycfg: TSaveReplayControlCfg): TInputArray;
 var
   len, i: integer;
 begin
   if (keycfg.available) then
   begin
     len := 1;
     if keycfg.bShift then
       inc(len);
     if keycfg.bCtrl then
       inc(len);
     if keycfg.bAlt then
       inc(len);
     SetLength(Result, len * 2);
     i := 0;
     if keycfg.bShift then
     begin
       Result[i].Itype := INPUT_KEYBOARD;
       Result[i].ki.wVk := VK_RSHIFT;
       Result[i].ki.wScan := 0;
       Result[i].ki.dwFlags := 0;
       inc(i);
     end;
     if keycfg.bCtrl then
     begin
       Result[i].Itype := INPUT_KEYBOARD;
       Result[i].ki.wVk := VK_LCONTROL;
       Result[i].ki.wScan := 0;
       Result[i].ki.dwFlags := 0;
       inc(i);
     end;
     if keycfg.bAlt then
     begin
       Result[i].Itype := INPUT_KEYBOARD;
       Result[i].ki.wVk := VK_LMENU;
       Result[i].ki.wScan := 0;
       Result[i].ki.dwFlags := 0;
       inc(i);
     end;
     Result[i].Itype := INPUT_KEYBOARD;
     Result[i].ki.wVk := MapKunosKeyToVirtualKey(keycfg.key);
     Result[i].ki.wScan := 0;
     Result[i].ki.dwFlags := 0;
     for i := 0 to len - 1 do
     begin
       Result[len + i] := Result[len - (i + 1)];
       Result[len + i].ki.dwFlags := KEYEVENTF_KEYUP;
     end;
   end
   else
   begin
     SetLength(Result, 2);
     Result[0].Itype := INPUT_KEYBOARD;
     Result[0].ki.wVk := LOWORD(VkKeyScan('m'));
     Result[0].ki.wScan := 0;
     Result[0].ki.dwFlags := 0;
     Result[1] := Result[0];
     Result[1].ki.dwFlags := KEYEVENTF_KEYUP;
   end;
 end;

//function GetInputArray(const keycfg: TSaveReplayControlCfg): TInputArray;
//begin
//  if (keycfg.available) then
//  begin
//    SetLength(Result, 2);
//    Result[0].Itype := 1;
//    Result[0].ki.wVk := MapKunosKeyToVirtualKey(keycfg.key);
//    if (keycfg.bShift) then
//      Result[0].ki.wVk := Result[0].ki.wVk or $0100;
//    if (keycfg.bCtrl) then
//      Result[0].ki.wVk := Result[0].ki.wVk or $0200;
//    if (keycfg.bAlt) then
//      Result[0].ki.wVk := Result[0].ki.wVk or $0400;
//    Result[0].ki.wScan := 0;
//    Result[0].ki.dwFlags := 0;
//    Result[1] := Result[0];
//    Result[1].ki.dwFlags := KEYEVENTF_KEYUP
//  end
//  else
//  begin
//    SetLength(Result, 2);
//    Result[0].Itype := 1;
//    Result[0].ki.wVk := LOWORD(VkKeyScan('m'));
//    Result[0].ki.wScan := 0;
//    Result[0].ki.dwFlags := 0;
//    Result[1] := Result[0];
//    Result[1].ki.dwFlags := KEYEVENTF_KEYUP;
//  end;
//end;

end.
