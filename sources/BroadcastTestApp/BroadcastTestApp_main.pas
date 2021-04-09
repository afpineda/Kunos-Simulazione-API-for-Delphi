unit BroadcastTestApp_main;

{ *******************************************************

  Kunos Simulazione's broadcasting
  Delphi demo

  *******************************************************

  This Delphi/pascal translation:
  (C) 2021. Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-03-10] First implementation

  [2021-03-30] Minor improvements.
  No broadcasting activity detection.

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.ValEdit,
  ksBroadcasting.Leaderboard,
  ksBroadcasting.Protocol,
  ksBroadcasting.UDP,
  ksBroadcasting.Data, Vcl.WinXCtrls;

type
  TForm_main = class(TForm)
    PC_main: TPageControl;
    Page_Connect: TTabSheet;
    Panel_connection: TPanel;
    Edt_CommandPwd: TLabeledEdit;
    Edt_ConnPwd: TLabeledEdit;
    Edt_Port: TLabeledEdit;
    Edt_Host: TLabeledEdit;
    Memo_Log: TMemo;
    Panel_ConnectButtons: TPanel;
    Btn_connect: TButton;
    Btn_Disconnect: TButton;
    Btn_ClearLog: TButton;
    Btn_DefConnField: TButton;
    Page_carEntries: TTabSheet;
    Grid_carEntries: TStringGrid;
    Grid_drivers: TStringGrid;
    Lbl_drivers: TLabel;
    Lbl_cars: TLabel;
    Splitter1: TSplitter;
    Page_CarData: TTabSheet;
    List_CarData: TListView;
    Page_SessionData: TTabSheet;
    VE_Session: TValueListEditor;
    Page_TrackData: TTabSheet;
    VE_Track: TValueListEditor;
    LV_HUD: TListView;
    Lbl_Hud: TLabel;
    Lbl_Cams: TLabel;
    LV_Cam: TListView;
    Splitter2: TSplitter;
    Btn_ForceTrackData: TButton;
    AI_Receiving: TActivityIndicator;
    Page_Plotter: TTabSheet;
    PB_Plotter: TPaintBox;
    Btn_ClearPlotter: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Btn_DefConnFieldClick(Sender: TObject);
    procedure Btn_ClearLogClick(Sender: TObject);
    procedure Btn_connectClick(Sender: TObject);
    procedure Btn_DisconnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LV_HUDDblClick(Sender: TObject);
    procedure LV_CamDblClick(Sender: TObject);
    procedure Btn_ForceTrackDataClick(Sender: TObject);
    procedure Grid_carEntriesDblClick(Sender: TObject);
    procedure Btn_ClearPlotterClick(Sender: TObject);
    procedure PB_PlotterPaint(Sender: TObject);
  private
    { Private declarations }
    PlotBmp: TBitmap;

    Protocol: TksUDPv4BroadcastingProtocol;
    procedure OnConnection(Sender: TksBroadcastingProtocol;
      const result: TKsRegistrationResult);
    procedure OnEntryList(Sender: TksBroadcastingProtocol;
      NewList: TksEntryList);
    procedure OnCarDataUpdate(Sender: TksBroadcastingProtocol;
      const carData: TKsCarData);
    procedure OnSessionDataUpdate(Sender: TksBroadcastingProtocol;
      const sessionData,prev: TKsSessionData);
    procedure OnTrackData(Sender: TksBroadcastingProtocol;
      trackData: TksTrackData);
    procedure OnBroadcastingEvent(Sender: TksBroadcastingProtocol;
      const event: TksBroadcastingEvent);
    procedure OnServerInactivity(Sender: TObject);
    procedure OnLeaderboard(Sender: TksBroadcastingProtocol;
      Leaderboard: TksLeaderboard);

    procedure PlotCar(const carData: TKsCarData);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  Winapi.Winsock2,
  System.DateUtils,
  System.Net.Socket;

{$R *.dfm}
// ----------------------------------------------------------------------------
// TksUDPProtocol events
// ----------------------------------------------------------------------------

procedure TForm_main.OnConnection(Sender: TksBroadcastingProtocol;
  const result: TKsRegistrationResult);
begin
  Memo_Log.Lines.Add('Registration result: ' + result.ErrorMessage);
  List_CarData.Items.Clear;
  with result do
    if (success and ReadOnly) then
      Memo_Log.Lines.Add('Connected as read-only')
    else if (success) then
      Memo_Log.Lines.Add('Connected')
    else
    begin
      Memo_Log.Lines.Add('Rejected');
    end;
  AI_Receiving.Animate := result.success;
end;

procedure TForm_main.OnServerInactivity(Sender: TObject);
begin
  Memo_Log.Lines.Add('Server is inactive');
end;

procedure TForm_main.OnEntryList(Sender: TksBroadcastingProtocol;
  NewList: TksEntryList);
var
  i, j, rc: Integer;
  carInfo: TKsCarInfo;
begin
  Grid_carEntries.RowCount := 1;
  Grid_carEntries.ColCount := 3;
  Grid_carEntries.Cells[0, 0] := 'Car#';
  Grid_carEntries.Cells[1, 0] := 'Team';
  Grid_carEntries.Cells[2, 0] := 'Driver';
  Grid_drivers.RowCount := 1;
  Grid_drivers.ColCount := 4;
  Grid_drivers.Cells[0, 0] := 'Car#';
  Grid_drivers.Cells[1, 0] := 'Team';
  Grid_drivers.Cells[2, 0] := 'Name';
  Grid_drivers.Cells[3, 0] := 'Alias';

  if (NewList <> nil) and (NewList.Count > 0) then
  begin
    Grid_carEntries.RowCount := NewList.Count + 1;
    Grid_carEntries.FixedRows := 1;
    Memo_Log.Lines.Add('Entry list received');
    i := 0;
    for carInfo in NewList.Values do
    begin
      Grid_carEntries.Cells[0, i + 1] := carInfo.RaceNumber.ToString;
      Grid_carEntries.Cells[1, i + 1] := carInfo.TeamName;
      Grid_carEntries.Cells[2, i + 1] := carInfo.Drivers
        [carInfo.CurrentDriverIndex].ShortName;
      for j := 0 to carInfo.Drivers.Count - 1 do
      begin
        Grid_drivers.RowCount := Grid_drivers.RowCount + 1;
        rc := Grid_drivers.RowCount - 1;
        Grid_drivers.Cells[0, rc] := carInfo.RaceNumber.ToString;
        Grid_drivers.Cells[1, rc] := carInfo.TeamName;
        Grid_drivers.Cells[2, rc] := carInfo.Drivers[j].FirstName + ' ' +
          carInfo.Drivers[j].LastName;
        Grid_drivers.Cells[3, rc] := carInfo.Drivers[j].ShortName;
      end;
      Grid_drivers.FixedRows := 1;
      inc(i);
    end;
  end
  else
  begin
    Memo_Log.Lines.Add('Entry list cleared');
    List_CarData.Items.Clear;
  end;
  NewList.Free;
end;

procedure TForm_main.OnCarDataUpdate(Sender: TksBroadcastingProtocol;
  const carData: TKsCarData);
var
  item: TListItem;
  i: Integer;
  carIndexStr: string;
begin
  // Memo_Log.Lines.Add('Car data updated');
  item := nil;
  i := 0;
  carIndexStr := carData.carIndex.ToString;
  while (item = nil) and (i < List_CarData.Items.Count) do
  begin
    if (List_CarData.Items[i].Caption = carIndexStr) then
      item := List_CarData.Items[i];
    inc(i);
  end;
  if (item = nil) then
    item := List_CarData.Items.Add
  else
    item.SubItems.Clear;
  with carData do
  begin
    item.Caption := carData.carIndex.ToString;
    item.SubItems.Add(driverIndex.ToString);
    item.SubItems.Add(Format('%.4f', [WorldPosX]));
    item.SubItems.Add(Format('%.4f', [WorldPosY]));
    item.SubItems.Add(Position.ToString);
    item.SubItems.Add(TrackPosition.ToString);
    item.SubItems.Add(Format('%2.1f%%', [SplinePosition * 100]));
    item.SubItems.Add(Laps.ToString);
    item.SubItems.Add(LastLap.laptimeMS.ToString);
  end;
  PlotCar(carData);
end;

procedure TForm_main.OnSessionDataUpdate(Sender: TksBroadcastingProtocol;
  const sessionData,prev: TKsSessionData);
begin
  // Memo_Log.Lines.Add('Session data updated');
  VE_Session.Strings.Clear;
  with VE_Session.Strings do
  begin
    Add('Session Time=' + sessionData.SessionTime.ToString);
    Add('Session Remaining Time=' + sessionData.SessionRemainingTime.ToString);
    Add('Session End Time=' + sessionData.SessionEndTime.ToString);
    Add('Remaining time=' + sessionData.RemainingTime.ToString);
    Add('Rain Level=' + sessionData.RainLevel.ToString);
    Add('Clouds=' + sessionData.Clouds.ToString);
    Add('Wetness=' + sessionData.Wetness.ToString);
    Add('Best Lap=' + sessionData.BestSessionLap.laptimeMS.ToString);
    Add('Best car index=' + sessionData.BestSessionLap.carIndex.ToString);
    Add('Focused car index=' + sessionData.FocusedCarIndex.ToString);
    Add('Camera set=' + sessionData.ActiveCameraSet);
    Add('Camera =' + sessionData.ActiveCamera);
    Add('Air temp=' + sessionData.AmbientTemp.ToString);
    Add('Track temp=' + sessionData.TrackTemp.ToString);
    Add('Event Index=' + sessionData.EventIndex.ToString);
    Add('Session Index=' + sessionData.SessionIndex.ToString);
    Add('Type=' + Byte(sessionData.SessionType).ToString);
    Add('Phase=' + Byte(sessionData.Phase).ToString);
  end;
  if (sessionData.SessionIndex<>prev.SessionIndex) then
    Memo_Log.Lines.Add('NOTE: New session');
  if (sessionData.Phase<>prev.Phase) then
    Memo_Log.Lines.Add('NOTE: Next phase or session reset');
end;

procedure TForm_main.OnTrackData(Sender: TksBroadcastingProtocol;
  trackData: TksTrackData);
var
  camSet: string;
  j: Integer;
  item: TListItem;
begin
  Memo_Log.Lines.Add('Track data received');
  VE_Track.Strings.Clear;
  VE_Track.Strings.Add('Track=' + trackData.Name);
  VE_Track.Strings.Add('Meters=' + trackData.Meters.ToString);
  LV_Cam.Clear;
  for camSet in trackData.CameraSets.Keys do
    for j := 0 to (trackData.CameraSets.Items[camSet]).Count - 1 do
    begin
      item := LV_Cam.Items.Add;
      item.Caption := camSet;
      item.SubItems.Add(trackData.CameraSets.Items[camSet][j]);
    end;
  LV_HUD.Clear;
  for j := 0 to trackData.HUDPages.Count - 1 do
    LV_HUD.AddItem(trackData.HUDPages.Strings[j], nil);
  trackData.Free;
end;

procedure TForm_main.OnBroadcastingEvent(Sender: TksBroadcastingProtocol;
  const event: TksBroadcastingEvent);
begin
  Memo_Log.Lines.Add(Format('EVENT: %s /CARIDX: %d /MS: %d / TYPE: %d',
    [event.messageText, event.carIndex, event.TimeMS,
    Integer(event.eventType)]));
end;

procedure TForm_main.OnLeaderboard(Sender: TksBroadcastingProtocol;
  Leaderboard: TksLeaderboard);
var
  item: TListItem;
  i: Integer;
begin
  try
    List_CarData.Clear;
    for i := 0 to Leaderboard.Count - 1 do
    begin
      item := List_CarData.Items.Add;
      item.Caption := Leaderboard.GetField(i, RaceNumber);
      item.SubItems.Add(Leaderboard.GetField(i, driverShortName));
      item.SubItems.Add(Leaderboard.GetField(i, bestTime));
      item.SubItems.Add(Leaderboard.GetField(i, gapToBestLap));
      item.SubItems.Add(Leaderboard.GetField(i, lastTime));
      item.SubItems.Add(Leaderboard.GetField(i, currentTime));
      item.SubItems.Add(Leaderboard.GetField(i, officialPos));
      item.SubItems.Add(Leaderboard.GetField(i, TrackPos));
      item.SubItems.Add(Leaderboard.GetField(i,
        TksLeaderboard.TField.CarLocation));
      item.SubItems.Add(Leaderboard.GetField(i, laps));
      item.SubItems.Add(Leaderboard.GetField(i, gapToNextCar));
    end;

  finally
    Leaderboard.Free;
  end;
end;

// ----------------------------------------------------------------------------
// Connection page
// ----------------------------------------------------------------------------

procedure TForm_main.Btn_ClearLogClick(Sender: TObject);
begin
  Memo_Log.Lines.Clear;
end;

procedure TForm_main.Btn_connectClick(Sender: TObject);
var
  ep: TNetEndPoint;
begin
  try
    ep.Family := 2;
    ep.SetAddress(Edt_Host.Text);
    ep.Port := StrToInt(Edt_Port.Text);
    Protocol.Start(ep, 'Delphi demo', Edt_ConnPwd.Text, 500,
      Edt_CommandPwd.Text);
    Memo_Log.Lines.Add('Registration request sent to ' + Edt_Host.Text + ':' +
      Edt_Port.Text);

  except
    on E: Exception do
    begin
      Memo_Log.Lines.Add('ERROR:');
      Memo_Log.Lines.Add(E.Message);
    end;
  end;
end;

procedure TForm_main.Btn_DefConnFieldClick(Sender: TObject);
begin
  Edt_CommandPwd.Text := '';
  Edt_ConnPwd.Text := '';
  Edt_Host.Text := '127.0.0.1';
  Edt_Port.Text := '9000';
end;

procedure TForm_main.Btn_DisconnectClick(Sender: TObject);
begin
  try
    Protocol.Stop;
    Memo_Log.Lines.Add('Unregistration requested');
    AI_Receiving.Animate := false;
  except
    on E: Exception do
    begin
      Memo_Log.Lines.Add('ERROR:');
      Memo_Log.Lines.Add(E.Message);
    end;
  end;
end;

// ----------------------------------------------------------------------------
// Track page
// ----------------------------------------------------------------------------

procedure TForm_main.LV_CamDblClick(Sender: TObject);
begin
  if (LV_Cam.Selected <> nil) then
  begin
    Memo_Log.Lines.Add('Requesting Camera: ' + LV_Cam.Selected.Caption + '/' +
      LV_Cam.Selected.SubItems[0]);
    Protocol.RequestFocus(LV_Cam.Selected.Caption, LV_Cam.Selected.SubItems[0]);
  end;
end;

procedure TForm_main.LV_HUDDblClick(Sender: TObject);
begin
  if (LV_HUD.Selected <> nil) then
  begin
    Memo_Log.Lines.Add('Requesting HUD page: ' + LV_HUD.Selected.Caption);
    Protocol.RequestHUDPage(LV_HUD.Selected.Caption);
  end;
end;

procedure TForm_main.Btn_ForceTrackDataClick(Sender: TObject);
begin
  Memo_Log.Lines.Add('Requesting track data');
  Protocol.RequestTrackData;
end;

// ----------------------------------------------------------------------------
// Plotter page
// ----------------------------------------------------------------------------

procedure TForm_main.Btn_ClearPlotterClick(Sender: TObject);
var
  aux: Integer;
begin
  with PlotBmp do
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clRed;
    Canvas.Rectangle(0, 0, Width, Height);
    aux := ClientRect.Height div 2;
    Canvas.MoveTo(0, aux);
    Canvas.LineTo(Width, aux);
    aux := Width div 2;
    Canvas.MoveTo(aux, 0);
    Canvas.LineTo(aux, Height);
  end;
  PB_Plotter.Refresh;
end;

procedure TForm_main.PB_PlotterPaint(Sender: TObject);
begin
  PB_Plotter.Canvas.StretchDraw(PB_Plotter.ClientRect, PlotBmp);
end;

procedure TForm_main.PlotCar(const carData: TKsCarData);
var
  x, y: Integer;
begin
  PlotBmp.Canvas.Brush.Style := bsSolid;
  PlotBmp.Canvas.Brush.Color := clBlack; // - carData.carIndex;
  PlotBmp.Canvas.Pen.Color := PlotBmp.Canvas.Brush.Color;
  x := (PlotBmp.Width div 2) + Trunc(carData.WorldPosX * 20);
  y := (PlotBmp.Height div 2) + Trunc(carData.WorldPosY * 20);
  PlotBmp.Canvas.Rectangle(x - 2, y - 2, x + 2, y + 2);
  PB_Plotter.Refresh;
end;

// ----------------------------------------------------------------------------
// Main form events
// ----------------------------------------------------------------------------

procedure TForm_main.FormCreate(Sender: TObject);
begin
  PlotBmp := TBitmap.Create;
  PlotBmp.Width := 500;
  PlotBmp.Height := 500;
  PlotBmp.Transparent := false;
  Protocol := TksUDPv4BroadcastingProtocol.Create;
  Protocol.OnRegistration := OnConnection;
  Protocol.OnEntryList := OnEntryList;
  // Protocol.OnCarData := OnCarDataUpdate;
  Protocol.OnLeaderboardUpdate := OnLeaderboard;
  Protocol.OnSessionData := OnSessionDataUpdate;
  Protocol.OnTrackData := OnTrackData;
  Protocol.OnBroadcastingEvent := OnBroadcastingEvent;
  Protocol.OnServerInactivity := OnServerInactivity;
  Protocol.MaxServerInactivityMs := 30000; // half a minute
  Memo_Log.Lines.Clear;
  Panel_connection.Enabled := true;
  Btn_DefConnFieldClick(nil);
{$IFDEF DEBUG}
  Edt_CommandPwd.Text := 'kagarro';
  Edt_ConnPwd.Text := 'kagarro';
  Edt_Host.Text := '192.168.1.160';
  Edt_Port.Text := '9001';
{$ENDIF}
  PC_main.ActivePageIndex := 0;
  List_CarData.Items.Clear;
  VE_Session.Strings.Clear;
  VE_Track.Strings.Clear;
  LV_HUD.Clear;
  LV_Cam.Clear;
  Grid_carEntries.RowCount := 0;
  Grid_drivers.RowCount := 0;
  AI_Receiving.Animate := false;
  PB_Plotter.Canvas.Brush.Style := TBrushStyle.bsSolid;
  Btn_ClearPlotter.Click;
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  Protocol.Free;
  PlotBmp.Free;
end;

procedure TForm_main.Grid_carEntriesDblClick(Sender: TObject);
var
  carNumber: Integer;
begin
  with Grid_carEntries do
    if (Row >= 0) and (Row < RowCount) then
      try
        Memo_Log.Lines.Add('Requesting camera focus on car # ' + Cells[0, Row]);
        carNumber := StrToInt(Cells[0, Row]);
        Protocol.RequestFocusOnRaceNumber(carNumber);
      except
      end;
end;

end.
