unit PoCACPressureTest_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ksSharedMem, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm_main = class(TForm)
    Memo_log: TMemo;
    Timer_main: TTimer;
    Panel1: TPanel;
    Lbl_status: TLabel;
    Chk_stayOnTop: TCheckBox;
    Btn_ClearLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Timer_mainTimer(Sender: TObject);
    procedure Chk_stayOnTopClick(Sender: TObject);
    procedure Btn_ClearLogClick(Sender: TObject);
  private
    { Private declarations }
    sharedMem: TKsLiveData; // TksMemorySnapshot;
    state: integer;
    initialPSI: array [0 .. 3] of Single;
    currentPSI: array [0 .. 3] of Single;
    initialRoadTemp, initialAirTemp: Single;
    initialLap, completedLaps: integer;
    procedure Log(const msg: string);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  ksSharedData,
  ksSharedMem.Utils;

const
  ST_start = 0;
  ST_waiting_cold_pressures = 1;
  ST_ongoing_test = 2;

var
  psi_target: array [0 .. 5] of Single;
  psi_target_names: array [0 .. 5] of string;

{$R *.dfm}

procedure TForm_main.Log(const msg: string);
begin
  Memo_log.Lines.Add(msg);
end;

procedure TForm_main.Btn_ClearLogClick(Sender: TObject);
begin
  Memo_log.Lines.Clear;
end;

procedure TForm_main.Chk_stayOnTopClick(Sender: TObject);
begin
  if Chk_stayOnTop.Checked then
    Form_main.FormStyle := fsStayOnTop
  else
    Form_main.FormStyle := fsNormal;
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  sharedMem := TKsLiveData.Create;
  state := ST_start;
  Memo_log.Lines.Clear;
  Memo_log.Lines.Add('Mera prueba de concepto');
  Lbl_status.Caption := 'Espera...';
end;

procedure TForm_main.Timer_mainTimer(Sender: TObject);
var
  psiRatio: array [0 .. 3] of Single;
  i: integer;
begin
  if (sharedMem.atDrivingSession) then
    try
      Lbl_status.Caption := Format('Sesión activa (%1.5f)',
        [sharedMem.normalizedCarPosition]);

      if (state <> ST_waiting_cold_pressures) and (sharedMem.status = AC_LIVE)
        and (sharedMem.ignitionOn = 0) then
      begin
        // Car is at garage
        if (state = ST_ongoing_test) then
        begin
          // End of test
          if (completedLaps > 0) and (initialPSI[0] <> 0.0) and
            (initialPSI[1] <> 0.0) and (initialPSI[2] <> 0.0) and
            (initialPSI[3] <> 0.0) then
          begin
            for i := 0 to 3 do
              psiRatio[i] := currentPSI[i] / initialPSI[i];

            Log(Format('Ratio: %2.4f  %2.4f  %2.4f  %2.4f',
              [psiRatio[0], psiRatio[1], psiRatio[2], psiRatio[3]]));
            Log(Format('- Presiones correctas tras %d vueltas -',
              [completedLaps]));
            Log('Objetivo (PSI) | FL | FR | RL | RR ');
            for i := 0 to 5 do
            begin
              Log(Format('%s (%2.1f) | %2.1f | %2.1f | %2.1f | %2.1f',
                [psi_target_names[i], psi_target[i],
                (psi_target[i] / psiRatio[0]), (psi_target[i] / psiRatio[1]),
                (psi_target[i] / psiRatio[2]), (psi_target[i] / psiRatio[3])]));
            end;
          end;
        end;

        state := ST_waiting_cold_pressures;
        Log('Reiniciando test');
      end;

      if (state = ST_waiting_cold_pressures) and
        (sharedMem.PhysicsData^.ignitionOn <> 0) and
        (sharedMem.GraphicsData^.status = AC_LIVE) then
      begin
        // Car returned from garage
        initialPSI[0] := sharedMem.wheelPressure[0];
        initialPSI[1] := sharedMem.wheelPressure[1];
        initialPSI[2] := sharedMem.wheelPressure[2];
        initialPSI[3] := sharedMem.wheelPressure[3];
        initialRoadTemp := sharedMem.roadTemp;
        initialAirTemp := sharedMem.airTemp;
        initialLap := sharedMem.completedLaps;
        completedLaps := 0;
        Log(Format
          ('Presiones iniciales: %2.1f  %2.1f  %2.1f  %2.1f | %2.0fºC %2.0fºC | Lap %d',
          [initialPSI[0], initialPSI[1], initialPSI[2], initialPSI[3],
          initialAirTemp, initialRoadTemp, initialLap]));
        state := ST_ongoing_test;
      end;
      if (state = ST_ongoing_test) and (sharedMem.status = AC_LIVE) and
        ((sharedMem.completedLaps - initialLap) <> completedLaps) and
        (sharedMem.normalizedCarPosition >= 0.0) then
      begin
        completedLaps := sharedMem.completedLaps - initialLap;
        currentPSI[0] := sharedMem.wheelPressure[0];
        currentPSI[1] := sharedMem.wheelPressure[1];
        currentPSI[2] := sharedMem.wheelPressure[2];
        currentPSI[3] := sharedMem.wheelPressure[3];
        Log(Format('Completadas %d vueltas de test', [completedLaps]));
        Log(Format('Lectura: %2.1f  %2.1f  %2.1f  %2.1f',
          [currentPSI[0], currentPSI[1], currentPSI[2], currentPSI[3]]));
      end;

    except
      on e: Exception do
      begin
        Timer_main.Enabled := false;
        Log('Error de programa: se ha detenido la actividad');
        Log(e.Message);
      end;
    end
  else
    Lbl_status.Caption := 'Sesión INACTIVA';

end;

initialization

psi_target_names[0] := 'GT3 2019 seco';
psi_target_names[1] := 'GT3 2019 lluvia';
psi_target_names[2] := 'GT3 2020 seco';
psi_target_names[3] := 'GT3 2020 lluvia';
psi_target_names[4] := 'GT4 seco';
psi_target_names[5] := 'GT4 lluvia';
psi_target[0] := 27.5;
psi_target[1] := 30.0;
psi_target[2] := 27.3;
psi_target[3] := 30.0;
psi_target[4] := 26.8;
psi_target[5] := 30.0;

end.
