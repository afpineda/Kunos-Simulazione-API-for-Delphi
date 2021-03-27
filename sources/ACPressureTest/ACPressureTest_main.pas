unit ACPressureTest_main;

{ *******************************************************

  Automated pressure test for AC/ACC

  Computes correct cold pressures to achieve target
  hot pressures

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

  [2021-03-17] First implementation

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,
  Vcl.ComCtrls, Vcl.Grids, Vcl.ValEdit, ksSharedMem, ksSharedMem.Data,
  ACPressureTest_CopyPasteOptions,
  ACPressureTest_state;

type
  TForm_main = class(TForm)
    Menu_main: TMainMenu;
    Menu_globalOptions: TMenuItem;
    Menu_stayOnTop: TMenuItem;
    N1: TMenuItem;
    Menu_CopyPasteOptions: TMenuItem;
    Status_main: TStatusBar;
    Menu_action: TMenuItem;
    Menu_copy: TMenuItem;
    Timer_main: TTimer;
    N2: TMenuItem;
    Menu_Restart: TMenuItem;
    Menu_TestOptions: TMenuItem;
    Menu_NormalizeAtFinishLine: TMenuItem;
    Menu_NormalizeAtCarPos: TMenuItem;
    N3: TMenuItem;
    Menu_CompleteAtTeleport: TMenuItem;
    Menu_Min2Laps: TMenuItem;
    Menu_Min5Laps: TMenuItem;
    Menu_Min7Laps: TMenuItem;
    PC_main: TPageControl;
    Page_Test: TTabSheet;
    Page_Results: TTabSheet;
    VLE_test: TValueListEditor;
    Panel_TargetPSI: TPanel;
    Lbl_target: TLabel;
    Edit_Target_FL: TLabeledEdit;
    Edit_Target_FR: TLabeledEdit;
    Edit_Target_RL: TLabeledEdit;
    Edit_Target_RR: TLabeledEdit;
    Menu_DefaultTargetPSI: TMenuItem;
    VLE_Results: TValueListEditor;
    Lbl_NormalizedPressures: TLabel;
    Menu_AppOptions: TMenuItem;
    Menu_cleanReg: TMenuItem;
    Chk_Link: TCheckBox;
    N4: TMenuItem;
    Menu_About: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Menu_stayOnTopClick(Sender: TObject);
    procedure Timer_mainTimer(Sender: TObject);
    procedure Menu_actionClick(Sender: TObject);
    procedure Menu_RestartClick(Sender: TObject);
    procedure Menu_NormalizeAtFinishLineClick(Sender: TObject);
    procedure Menu_NormalizeAtCarPosClick(Sender: TObject);
    procedure Menu_CompleteAtTeleportClick(Sender: TObject);
    procedure Menu_Min2LapsClick(Sender: TObject);
    procedure Menu_Min5LapsClick(Sender: TObject);
    procedure Menu_Min7LapsClick(Sender: TObject);
    procedure Edit_Target_FLExit(Sender: TObject);
    procedure Edit_Target_FLEnter(Sender: TObject);
    procedure Menu_DefaultTargetPSIClick(Sender: TObject);
    procedure Page_ResultsShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Menu_cleanRegClick(Sender: TObject);
    procedure Menu_copyClick(Sender: TObject);
    procedure Menu_CopyPasteOptionsClick(Sender: TObject);
    procedure Edit_Target_FLKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure Menu_AboutClick(Sender: TObject);
  private
    { Private declarations }
    live: TksLiveData;
    test: TPressureTest;
    savedEditText: TCaption;
    customTargetPressures: boolean;
    procedure OnStateChange(Sender: TObject);
    procedure Recalculate;
    procedure UseDefaultPressures;
    procedure LoadAppState;
    procedure SaveAppState;
    procedure ClearAppState;
    procedure getTargetPSI(out target: array of Single);

  public
    { Public declarations }
    savedCopyPasteFields: string;
  end;

var
  Form_main: TForm_main;

implementation

uses
  Vcl.Clipbrd,
  System.UITypes,
  System.Win.Registry,
  ksSharedMem.Utils,
  I18NUtils,
  ACPressureTest_About,
  ACPressureTest_strings;

{$R *.dfm}

function SF2_1(const number: Single): string; inline;
begin
  Result := Format('%2.1f', [number]);
end;

procedure TForm_main.Edit_Target_FLEnter(Sender: TObject);
begin
  savedEditText := (Sender as TLabeledEdit).Text;
end;

procedure TForm_main.Edit_Target_FLExit(Sender: TObject);
var
  value: Single;
  ok: boolean;
begin
  with (Sender as TLabeledEdit) do
  begin
    try
      value := StrToFloat(Text);
      ok := (value >= 20.0) and (value <= 35.0);
      customTargetPressures := (Text <> savedEditText);
    except
      ok := false;
    end;
    if (not ok) then
    begin
      Application.MessageBox(PWideCHAR(str_invalidValue), '',
        MB_OK or MB_ICONERROR);
      Text := savedEditText;
    end
    else
    begin
      if (Chk_Link.Checked) then
      begin
        Edit_Target_FL.Text := (Sender as TLabeledEdit).Text;
        Edit_Target_FR.Text := (Sender as TLabeledEdit).Text;
        Edit_Target_RL.Text := (Sender as TLabeledEdit).Text;
        Edit_Target_RR.Text := (Sender as TLabeledEdit).Text;
      end;
      Recalculate;

    end;
  end;
end;

procedure TForm_main.Edit_Target_FLKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vkReturn) then
    Chk_Link.SetFocus;
end;

procedure TForm_main.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (Menu_cleanReg.Checked) then
    ClearAppState
  else
    SaveAppState;
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  I18NUtils.Translate(self);
  savedCopyPasteFields := '';
  live := TksLiveData.Create;
  test := TPressureTest.Create;
  test.OnStateChange := OnStateChange;
  Menu_CompleteAtTeleport.Checked := true;
  Timer_main.Enabled := true;
  Status_main.Panels[0].Text := '';
  Status_main.Hint := '';
  VLE_test.Strings.Clear;
  VLE_Results.Strings.Clear;
{$IFNDEF DEBUG}
  PC_main.Pages[0].TabVisible := false;
  PC_main.Pages[1].TabVisible := false;
{$ENDIF}
  PC_main.ActivePageIndex := 0;
  Edit_Target_FL.EditLabel.Caption := str_FL;
  Edit_Target_FR.EditLabel.Caption := str_FR;
  Edit_Target_RL.EditLabel.Caption := str_RL;
  Edit_Target_RR.EditLabel.Caption := str_RR;
  Edit_Target_FL.Text := '';
  Edit_Target_FR.Text := '';
  Edit_Target_RL.Text := '';
  Edit_Target_RR.Text := '';
  customTargetPressures := false;
  LoadAppState;
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  test.Free;
  live.Free;
end;

procedure TForm_main.Menu_AboutClick(Sender: TObject);
var
  fs: TFormStyle;
begin
  fs := self.FormStyle;
  self.FormStyle := fsNormal;
  Form_about.ParentWindow := self.Handle;
  Form_About.ShowModal;
  self.FormStyle := fs;
end;

procedure TForm_main.Menu_actionClick(Sender: TObject);
begin
{$IFNDEF DEBUG}
  Menu_copy.Enabled := (test.State = stFinished);
{$ENDIF}
end;

procedure TForm_main.Menu_cleanRegClick(Sender: TObject);
begin
  Menu_cleanReg.Checked := not Menu_cleanReg.Checked;
end;

procedure TForm_main.Menu_CompleteAtTeleportClick(Sender: TObject);
begin
  test.MinLapCount := 0;
  (Sender as TMenuItem).Checked := true;
end;

procedure TForm_main.Menu_copyClick(Sender: TObject);
var
  i: integer;
  field: string;
  target: array [0 .. 3] of Single;
begin
{$IFNDEF DEBUG}
  if (test.State = stFinished) then
{$ENDIF}
  begin
    clipboard.Open;
    clipboard.Clear;
    i := 0;
    While (Form_CopyPasteOptions.NextField(i, field)) do
      try
        getTargetPSI(target);
        if (field = str_carModel) then
          clipboard.AsText := clipboard.AsText + test.CarModel + #9
        else if (field = str_trackName) then
          clipboard.AsText := clipboard.AsText + test.TrackName + #9
        else if (field = str_weather_options) and (test.isRainTyre) then
          clipboard.AsText := clipboard.AsText + str_wet + #9
        else if (field = str_weather_options) then
          clipboard.AsText := clipboard.AsText + str_dry + #9
        else if (field = str_compound) then
          clipboard.AsText := clipboard.AsText + test.tyreCompound + #9
        else if (field = str_airTemp) then
          clipboard.AsText := clipboard.AsText + SF2_1(test.airTemp) + #9
        else if (field = str_roadTemp) then
          clipboard.AsText := clipboard.AsText + SF2_1(test.roadTemp) + #9
        else if (field = str_FL) then
          clipboard.AsText := clipboard.AsText +
            SF2_1(test.NormalizedPressure[FL, target[FL]]) + #9
        else if (field = str_FR) then
          clipboard.AsText := clipboard.AsText +
            SF2_1(test.NormalizedPressure[FR, target[FR]]) + #9
        else if (field = str_RL) then
          clipboard.AsText := clipboard.AsText +
            SF2_1(test.NormalizedPressure[RL, target[RL]]) + #9
        else if (field = str_RR) then
          clipboard.AsText := clipboard.AsText +
            SF2_1(test.NormalizedPressure[RR, target[RR]]) + #9;
      except
        clipboard.Close;
        Application.MessageBox(PWideCHAR(str_invalidValue),
          PWideCHAR(str_error), MB_OK or MB_ICONERROR);
        Exit;
      end;
    clipboard.Close;
  end;
end;

procedure TForm_main.Menu_CopyPasteOptionsClick(Sender: TObject);
begin
  Form_main.FormStyle := fsNormal;
  Form_CopyPasteOptions.ShowModal;
  if (Menu_stayOnTop.Checked) then
    Form_main.FormStyle := fsStayOnTop;
end;

procedure TForm_main.Menu_DefaultTargetPSIClick(Sender: TObject);
begin
  UseDefaultPressures;
  Recalculate;
end;

procedure TForm_main.Menu_Min7LapsClick(Sender: TObject);
begin
  test.MinLapCount := 7;
  (Sender as TMenuItem).Checked := true;
end;

procedure TForm_main.Menu_Min2LapsClick(Sender: TObject);
begin
  test.MinLapCount := 2;
  (Sender as TMenuItem).Checked := true;
end;

procedure TForm_main.Menu_Min5LapsClick(Sender: TObject);
begin
  test.MinLapCount := 5;
  (Sender as TMenuItem).Checked := true;
end;

procedure TForm_main.Menu_NormalizeAtCarPosClick(Sender: TObject);
begin
  test.NormalizeAtTrackPos := live.normalizedCarPosition;
end;

procedure TForm_main.Menu_NormalizeAtFinishLineClick(Sender: TObject);
begin
  test.NormalizeAtTrackPos := 0.0;
end;

procedure TForm_main.Menu_RestartClick(Sender: TObject);
begin
  PC_main.ActivePageIndex := 0;
  test.Reset;
end;

procedure TForm_main.Menu_stayOnTopClick(Sender: TObject);
begin
  Menu_stayOnTop.Checked := (not Menu_stayOnTop.Checked);
  if (Menu_stayOnTop.Checked) then
    Form_main.FormStyle := fsStayOnTop
  else
    Form_main.FormStyle := fsNormal;
end;

procedure TForm_main.Timer_mainTimer(Sender: TObject);
begin
  test.Step(live);
  if (test.State = stOngoing) then
    VLE_test.Values[str_carPos] :=
      Format('%3.1f%%', [live.normalizedCarPosition * 100]);

end;

procedure TForm_main.OnStateChange(Sender: TObject);
begin
  case test.State of
    stUnavailable:
      begin
        Status_main.Panels[0].Text := str_state_unavailable;
        Status_main.Hint := str_hint_unavailable;
        VLE_test.Strings.Clear;
      end;
    stOngoing:
      begin
        Status_main.Panels[0].Text := str_state_Ongoing;
        Status_main.Hint := str_hint_Ongoing;
        if (test.lapCount = 0) then
        begin
          // VLE_test.Values[str_compound] := test.tyreCompound;
          // VLE_test.Values[str_airTemp] := SF2_1(test.airTemp);
          // VLE_test.Values[str_roadTemp] := SF2_1(test.roadTemp);
          if (live.session = TACSessionType.AC_HOTLAP) then
            VLE_test.Values[str_error] := str_hotlapNotSuitable
          else
          begin
            VLE_test.Values[str_FL + str_cold] := SF2_1(test.FL_cold) + str_PSI;
            VLE_test.Values[str_FR + str_cold] := SF2_1(test.FR_cold) + str_PSI;
            VLE_test.Values[str_RL + str_cold] := SF2_1(test.RL_cold) + str_PSI;
            VLE_test.Values[str_RR + str_cold] := SF2_1(test.RR_cold) + str_PSI;
          end;
        end
        else
        begin
          VLE_test.Values[str_laps] := test.lapCount.ToString;
          VLE_test.Values[str_FL + str_hot] := SF2_1(test.FL_hot) + str_PSI;
          VLE_test.Values[str_FR + str_hot] := SF2_1(test.FR_hot) + str_PSI;
          VLE_test.Values[str_RL + str_hot] := SF2_1(test.RL_hot) + str_PSI;
          VLE_test.Values[str_RR + str_hot] := SF2_1(test.RR_hot) + str_PSI;
        end;

      end;
    stWaitingColdPressures:
      begin
        Status_main.Panels[0].Text := str_state_waitingColdPressures;
        Status_main.Hint := str_hint_waitingColdPressures;
        VLE_test.Strings.Clear;
      end;
    stWaitingForGarage:
      begin
        Status_main.Panels[0].Text := str_state_waitingForGarage;
        Status_main.Hint := str_hint_waitingForGarage;
        VLE_test.Strings.Clear;
      end;
    stFinished:
      begin
        if (not customTargetPressures) then
          UseDefaultPressures;
        Status_main.Panels[0].Text := str_state_finished;
        Status_main.Hint := str_hint_finished;
        PC_main.ActivePageIndex := 1;
      end;
  end;
  Timer_main.Enabled := (test.State <> stFinished);
end;

procedure TForm_main.Page_ResultsShow(Sender: TObject);
begin
  Beep;
  Recalculate;
end;

procedure TForm_main.getTargetPSI(out target: array of Single);
begin
  target[FL] := StrToFloat(Edit_Target_FL.Text);
  target[FR] := StrToFloat(Edit_Target_FR.Text);
  target[RL] := StrToFloat(Edit_Target_RL.Text);
  target[RR] := StrToFloat(Edit_Target_RR.Text);
end;

procedure TForm_main.Recalculate;
var
  target_psi: array [0 .. 3] of Single;
begin
  VLE_Results.Strings.Clear;
  try
    getTargetPSI(target_psi);
  except
    VLE_Results.Values[str_error] := str_invalidValue;
    Exit;
  end;

  VLE_Results.Values[str_compound] := test.tyreCompound;
  VLE_Results.Values[str_airTemp] := SF2_1(test.airTemp);
  VLE_Results.Values[str_roadTemp] := SF2_1(test.roadTemp);
  VLE_Results.Values[str_FL] :=
    SF2_1(test.NormalizedPressure[FL, target_psi[FL]]) + str_PSI;
  VLE_Results.Values[str_FR] :=
    SF2_1(test.NormalizedPressure[FR, target_psi[FR]]) + str_PSI;
  VLE_Results.Values[str_RL] :=
    SF2_1(test.NormalizedPressure[RL, target_psi[RL]]) + str_PSI;
  VLE_Results.Values[str_RR] :=
    SF2_1(test.NormalizedPressure[RR, target_psi[RR]]) + str_PSI;
end;

const
  APP_REG_KEY = '\SOFTWARE\ACPressureTest';
  KEY_stayOnTop = 'StayOnTop';
  KEY_X = 'X';
  KEY_Y = 'Y';
  KEY_WIDTH = 'Width';
  KEY_HEIGHT = 'Height';
  KEY_MIN_LAPS = 'MinLaps';
  KEY_COPYPASTE_FIELDS = 'CopyPasteFields';
  KEY_LINKEDITBOXES = 'LinkEditBoxes';
  // KEY_TARGET_PSI_PREFIX = 'PSI_';

procedure TForm_main.UseDefaultPressures;
var
  defPSI: Single;
  aux: string;
begin
  defPSI := DefaultTyrePressure(test.tyreCompound);
  // str(defPSI: 2: 1, aux);
  aux := SF2_1(defPSI);
  Edit_Target_FL.Text := aux;
  Edit_Target_FR.Text := aux;
  Edit_Target_RL.Text := aux;
  Edit_Target_RR.Text := aux;
  customTargetPressures := false;
end;

procedure TForm_main.LoadAppState;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    if (reg.OpenKey(APP_REG_KEY, false)) then
      try
        Menu_stayOnTop.Checked := reg.ReadBool(KEY_stayOnTop);
        if (Menu_stayOnTop.Checked) then
          self.FormStyle := fsStayOnTop
        else
          self.FormStyle := fsNormal;

        self.Left := reg.ReadInteger(KEY_X);
        self.Top := reg.ReadInteger(KEY_Y);
        self.Width := reg.ReadInteger(KEY_WIDTH);
        self.Height := reg.ReadInteger(KEY_HEIGHT);
        test.MinLapCount := reg.ReadInteger(KEY_MIN_LAPS);
        case test.MinLapCount of
          3:
            Menu_Min2Laps.Checked := true;
          5:
            Menu_Min5Laps.Checked := true;
          10:
            Menu_Min7Laps.Checked := true;
        else
          Menu_CompleteAtTeleport.Checked := true;
        end;
        savedCopyPasteFields := reg.ReadString(KEY_COPYPASTE_FIELDS);
        Chk_Link.Checked := reg.ReadBool(KEY_LINKEDITBOXES);
        reg.CloseKey;
      except
      end;
  finally
    reg.Free;
  end;
end;

procedure TForm_main.SaveAppState;
var
  reg: TRegistry;
  aux, field: string;
  idx: integer;
begin
  reg := TRegistry.Create;
  try
    if (reg.OpenKey(APP_REG_KEY, true)) then
      try
        reg.WriteBool(KEY_stayOnTop, Menu_stayOnTop.Checked);
        reg.WriteInteger(KEY_X, self.Left);
        reg.WriteInteger(KEY_Y, self.Top);
        reg.WriteInteger(KEY_WIDTH, self.Width);
        reg.WriteInteger(KEY_HEIGHT, self.Height);
        reg.WriteInteger(KEY_MIN_LAPS, test.MinLapCount);
        aux := '';
        idx := 0;
        while Form_CopyPasteOptions.NextField(idx, field) do
          aux := aux + field + ';';
        reg.WriteString(KEY_COPYPASTE_FIELDS, aux);
        reg.WriteBool(KEY_LINKEDITBOXES, Chk_Link.Checked);
        reg.CloseKey;
      except
      end;
  finally
    reg.Free;
  end;
end;

procedure TForm_main.ClearAppState;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.DeleteKey(APP_REG_KEY);
  except
  end;
  reg.Free;
end;

end.
