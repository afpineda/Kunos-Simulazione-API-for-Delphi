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

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.Grids, Vcl.ValEdit,
  ksBroadcasting.Protocol,
  ksBroadcasting.UDP, ksBroadcasting.Data;

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
    procedure FormCreate(Sender: TObject);
    procedure Btn_DefConnFieldClick(Sender: TObject);
    procedure Btn_ClearLogClick(Sender: TObject);
    procedure Btn_connectClick(Sender: TObject);
    procedure Btn_DisconnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LV_HUDDblClick(Sender: TObject);
    procedure LV_CamDblClick(Sender: TObject);
    procedure Btn_ForceTrackDataClick(Sender: TObject);
  private
    { Private declarations }
    UDP: TksUDPDelegate;
    Protocol: TksBroadcastingProtocol;
    procedure OnConnection(Sender: TksBroadcastingProtocol;
      const result: TKsRegistrationResult);
    procedure OnEntryList(Sender: TksBroadcastingProtocol;
      NewList: TksEntryList);
    procedure OnCarDataUpdate(Sender: TksBroadcastingProtocol;
      const carInfo: TKsCarData);
    procedure OnSessionDataUpdate(Sender: TksBroadcastingProtocol;
      const sessionData: TKsSessionData);
    procedure OnTrackData(Sender: TksBroadcastingProtocol;
      trackData: TksTrackData);
    procedure OnBroadcastingEvent(Sender: TksBroadcastingProtocol;
      const event: TksBroadcastingEvent);
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  Winapi.Winsock2,
  System.Net.Socket;

{$R *.dfm}
// ----------------------------------------------------------------------------
// TksUDPProtocol events
// ----------------------------------------------------------------------------

procedure TForm_main.OnConnection(Sender: TksBroadcastingProtocol;
  const result: TKsRegistrationResult);
begin
  Memo_Log.Lines.Add('Registration result: ' + result.ErrorMessage);
  with result do
    if (success and ReadOnly) then
      Memo_Log.Lines.Add('Connected as read-only')
    else if (success) then
      Memo_Log.Lines.Add('Connected')
    else
    begin
      Memo_Log.Lines.Add('Rejected');
    end;
  List_CarData.Items.Clear;
end;

procedure TForm_main.OnEntryList(Sender: TksBroadcastingProtocol;
  NewList: TksEntryList);
var
  i, j, rc: Integer;
begin
  // for i := 0 to Grid_carEntries.ColCount - 1 do
  // Grid_carEntries.Cols[i].Clear;
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
    for i := 0 to NewList.Count - 1 do
    begin
      Grid_carEntries.Cells[0, i + 1] := NewList.Items[i].RaceNumber.ToString;
      Grid_carEntries.Cells[1, i + 1] := NewList.Items[i].TeamName;
      Grid_carEntries.Cells[2, i + 1] := NewList[i].Drivers
        [NewList.Items[i].CurrentDriverIndex].ShortName;
      for j := 0 to NewList[i].Drivers.Count - 1 do
      begin
        Grid_drivers.RowCount := Grid_drivers.RowCount + 1;
        rc := Grid_drivers.RowCount - 1;
        Grid_drivers.Cells[0, rc] := NewList.Items[i].RaceNumber.ToString;
        Grid_drivers.Cells[1, rc] := NewList.Items[i].TeamName;
        Grid_drivers.Cells[2, rc] := NewList[i].Drivers[j].FirstName + ' ' +
          NewList[i].Drivers[j].LastName;
        Grid_drivers.Cells[3, rc] := NewList[i].Drivers[j].ShortName;
      end;
      Grid_drivers.FixedRows := 1;
    end;
  end
  else
    Memo_Log.Lines.Add('Entry list cleared');

  NewList.Free;
end;

procedure TForm_main.OnCarDataUpdate(Sender: TksBroadcastingProtocol;
  const carInfo: TKsCarData);
var
  item: TListItem;
begin
  Memo_Log.Lines.Add('Car data updated');
  if List_CarData.Items.Count > 250 then
    List_CarData.Items.Clear;
  item := List_CarData.Items.Add;
  item.Caption := carInfo.carIndex.ToString;
  item.SubItems.Add(carInfo.driverIndex.ToString);
  item.SubItems.Add(carInfo.DriverCount.ToString);
  item.SubItems.Add(carInfo.Position.ToString);
  item.SubItems.Add(carInfo.TrackPosition.ToString);
  item.SubItems.Add(Format('%2.1f%%', [carInfo.SplinePosition * 100]));
  item.SubItems.Add(carInfo.Laps.ToString);
  item.SubItems.Add(carInfo.LastLap.laptimeMS.ToString);
end;

procedure TForm_main.OnSessionDataUpdate(Sender: TksBroadcastingProtocol;
  const sessionData: TKsSessionData);
begin
  // Memo_Log.Lines.Add('Session data updated');
  VE_Session.Strings.Clear;
  with VE_Session.Strings do
  begin
    Add('Session Time=' + sessionData.SessionTime.ToString);
    Add('Remaining Time=' + sessionData.RemainingTime.ToString);
    Add('Rain Level=' + sessionData.RainLevel.ToString);
    Add('Clouds=' + sessionData.Clouds.ToString);
    Add('Wetness=' + sessionData.Wetness.ToString);
    Add('Best Lap=' + sessionData.BestSessionLap.laptimeMS.ToString);
    Add('Best car index=' + sessionData.BestLapCarIndex.ToString);
    Add('Focused car index=' + sessionData.FocusedCarIndex.ToString);
    Add('Camera set=' + sessionData.ActiveCameraSet);
    Add('Camera =' + sessionData.ActiveCamera);
    Add('Air temp=' + sessionData.AmbientTemp.ToString);
    Add('Track temp=' + sessionData.TrackTemp.ToString);
  end;
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
  Memo_Log.Lines.Add('BROADCASTING EVENT: ' + event.messageText);
end;

// ----------------------------------------------------------------------------
// Connection page
// ----------------------------------------------------------------------------

procedure TForm_main.Btn_ClearLogClick(Sender: TObject);
begin
  Memo_Log.Lines.Clear;
end;

procedure TForm_main.Btn_connectClick(Sender: TObject);
begin
  try
    UDP.RemoteEndPoint.Family := 2;
    UDP.RemoteEndPoint.SetAddress(Edt_Host.Text);
    UDP.RemoteEndPoint.Port := StrToInt(Edt_Port.Text);
    Protocol.Register('Delphi demo', Edt_ConnPwd.Text, 5000,
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
    Protocol.Unregister;
    Memo_Log.Lines.Add('Unregistration requested');
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
    // Protocol.RequestFocus(LV_Cam.Selected.Caption, LV_Cam.Selected.SubItems[0]);
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
// Main form events
// ----------------------------------------------------------------------------

procedure TForm_main.FormCreate(Sender: TObject);
begin
  UDP := TksUDPDelegate.Create;
  Protocol := TksBroadcastingProtocol.Create(UDP);
  Protocol.OnRegistration := OnConnection;
  Protocol.OnEntryList := OnEntryList;
  Protocol.OnCarData := OnCarDataUpdate;
  Protocol.OnSessionData := OnSessionDataUpdate;
  Protocol.OnTrackData := OnTrackData;
  Protocol.OnBroadcastingEvent := OnBroadcastingEvent;
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
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  Protocol.Free;
end;

end.
