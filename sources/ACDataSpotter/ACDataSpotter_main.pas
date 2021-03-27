unit ACDataSpotter_main;
{ *******************************************************

  Proof of concept for KS' live data spotter

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

  [2021-03-25] First implementation

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ksSharedMem.Events, ksSharedMem;

type
  TForm_main = class(TForm)
    Memo_main: TMemo;
    Timer_main: TTimer;
    Chk_stayOnTop: TCheckBox;
    Btn_clear: TButton;
    procedure Timer_mainTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Chk_stayOnTopClick(Sender: TObject);
    procedure Btn_clearClick(Sender: TObject);
  private
    { Private declarations }
    live: TksLiveData;
    spotter: TksDataSpotter;
    procedure OnKsDatachange(Sender: TksDataSpotter;
      const change: TksDataSpotter.TChanges);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

{$R *.dfm}

procedure TForm_main.Btn_clearClick(Sender: TObject);
begin
  Memo_main.Clear;
end;

procedure TForm_main.Chk_stayOnTopClick(Sender: TObject);
begin
  if (Chk_stayOnTop.Checked) then
    self.FormStyle := fsStayOntop
  else
    self.FormStyle := fsNormal;
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  live := TksLiveData.Create;
  spotter := TksDataSpotter.Create;
  spotter.OnChange := OnKsDatachange;
  Memo_main.Clear;
  Timer_main.Enabled := true;
end;

procedure TForm_main.Timer_mainTimer(Sender: TObject);
begin
  spotter.Step(live);
end;

procedure TForm_main.OnKsDatachange(Sender: TksDataSpotter;
  const change: TksDataSpotter.TChanges);
var
  msg: string;
begin
  msg := '';
  case change of
    chgFirstRun:
      msg := 'Initialization';
    chgPitLimiter:
      msg := 'Pit limiter on/off';
    chgAIControlledCar:
      msg := 'AI control on/off';
    chgWaterTemp:
      msg := 'Warning: high water temp';
    chgPadLife:
      msg := 'Warning: pad life is over';
    chgDiscLife:
      msg := 'Warning: disc life is over';
    chgIgnition:
      msg := 'Ignition on/off';
    chgStarterEngine:
      msg := 'Starter on/off';
    chgEngineRunning:
      msg := 'Engine on/off';
    chgStatus:
      msg := 'Status: ' + integer(live.GraphicsData^.status).ToString;
    chgSession:
      msg := 'Session: ' + integer(live.GraphicsData^.session).ToString;
    chgCompletedLaps:
      msg := 'New lap';
    chgPosition:
      msg := 'New position';
    chgBestTime:
      msg := 'New best lap';
    chgInPit:
      msg := 'car in/out of pit';
    chgActiveCars:
      msg := 'new car at session';
    chgFlag:
      msg := 'Flag';
    chgPenalty:
      msg := 'Penalty';
    chgInPitLane:
      msg := 'car in/out of pit lane';
    chgMandatoryPitDone:
      msg := 'Mandatory pit stop done';
    chgSetupMenuVisible:
      msg := 'Working/not working on car setup';
    chgEngineMap:
      msg := 'Engine map changed';
    chgRainTyres:
      msg := 'Rain tyres in/out';
    chgFuelEstimatedLaps:
      msg := 'Warning: low fuel';
    chgMissingMandatoryPits:
      msg := 'Missing mandatory pit stop';
    chgGlobalChequered:
      msg := 'End of race/new race';
    chgNotRunning:
      msg := 'Not in a driving session';
    chgAtGarage:
      msg := 'Car in/out of garage';
    chgCarDamage:
      msg := 'Car damaged';
    chgSuspensionDamage:
      msg := 'Suspension damaged';
    chgGripStatus:
      msg := 'Grip status changed';
    chgRainIntensity:
      msg := 'more/less rain intensity';
  end;
  if (msg <> '') then
    Memo_main.Lines.Add(msg);
end;

end.
