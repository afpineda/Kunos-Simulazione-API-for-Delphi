unit PoCUDPImpl1_main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ksBroadcasting, ksBroadcasting.UDP, Vcl.ExtCtrls;

type
  TForm_main = class(TForm)
    MEmo_log: TMemo;
    Btn_Connect: TButton;
    Btn_disconnect: TButton;
    Btn_EntryList: TButton;
    Btn_ClearLog: TButton;
    Btn_Receive: TButton;
    Btn_TrackData: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Btn_ConnectClick(Sender: TObject);
    procedure Btn_disconnectClick(Sender: TObject);
    procedure Btn_EntryListClick(Sender: TObject);
    procedure Btn_ClearLogClick(Sender: TObject);
    procedure Btn_ReceiveClick(Sender: TObject);
    procedure Btn_TrackDataClick(Sender: TObject);
  private
    { Private declarations }
    UDP: TksUDPDelegate;
    ks: TksBroadcastingMsgHandler;
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  ksBroadcasting.Data,
  Winapi.Winsock2,
  System.Threading,
  System.Net.Socket;

{$R *.dfm}

type
  TBroadcastListener = class(TksBroadcastingMsgHandler)
  protected
    procedure Msg(const result: TKsRegistrationResult); overload; override;
    procedure Msg(const sessionData: TksSessionData); overload; override;
    procedure Msg(const carData: TksCarData); overload; override;
    procedure Msg(const carInfo: TKsCarInfo); overload; override;
    procedure Msg(const carEntryCount: integer); overload; override;
    procedure Msg(const trackData: TksTrackData); overload; override;
    procedure Msg(const event: TksBroadcastingEvent); overload; override;
  public
    procedure ProcessMessage;
  end;

procedure TBroadcastListener.Msg(const result: TKsRegistrationResult);
begin
  if result.Success then
    Form_main.MEmo_log.Lines.Add('Connected RONLY='+result.ReadOnly.ToString)
  else
    Form_main.MEmo_log.Lines.Add('Connection error: ' + result.ErrorMessage);
end;

procedure TBroadcastListener.Msg(const sessionData: TksSessionData);
begin
  Form_main.MEmo_log.Lines.Add('Session data ' +
    sessionData.EventIndex.ToString);
end;

procedure TBroadcastListener.Msg(const carData: TksCarData);
begin
  Form_main.MEmo_log.Lines.Add('car data ' + carData.carIndex.ToString);
end;

procedure TBroadcastListener.Msg(const carInfo: TKsCarInfo);
begin
  Form_main.MEmo_log.Lines.Add('Entry car info ' + carInfo.carIndex.ToString);
  carInfo.Free;
end;

procedure TBroadcastListener.Msg(const carEntryCount: integer);
begin
  Form_main.MEmo_log.Lines.Add('Entry count ' + carEntryCount.ToString);
end;

procedure TBroadcastListener.ProcessMessage;
begin
  inherited;
end;

procedure TBroadcastListener.Msg(const trackData: TksTrackData);
begin
  Form_main.MEmo_log.Lines.Add('Track: ' + trackData.Name);
  trackData.Free;
end;

procedure TBroadcastListener.Msg(const event: TksBroadcastingEvent);
begin
  Form_main.MEmo_log.Lines.Add('Event: ' + event.messageText);
end;

procedure TForm_main.Btn_ClearLogClick(Sender: TObject);
begin
  // MEmo_log.Lines.Clear;
  TBroadcastListener(ks).ProcessMessage;
end;

procedure TForm_main.Btn_ConnectClick(Sender: TObject);
begin
  ks.Register('TBroadcastListener', 'kagarro', 2000, 'kagarro');
end;

procedure TForm_main.Btn_disconnectClick(Sender: TObject);
begin
  ks.Unregister;
end;

procedure TForm_main.Btn_EntryListClick(Sender: TObject);
begin
  ks.RequestEntryList;
end;

procedure TForm_main.FormCreate(Sender: TObject);
begin
  UDP := TksUDPDelegate.Create;
  UDP.RemoteEndPoint.Family := 2;
  UDP.RemoteEndPoint.SetAddress('192.168.1.160');
  UDP.RemoteEndPoint.Port := 9001;
  ks := TBroadcastListener.Create(UDP);
  UDP.Start;
  MEmo_log.Clear;
end;

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  UDP.Stop;
end;

procedure TForm_main.Btn_ReceiveClick(Sender: TObject);
begin
  TBroadcastListener(ks).ProcessMessage;
end;

procedure TForm_main.Btn_TrackDataClick(Sender: TObject);
begin
  ks.RequestTrackData;
end;

end.
