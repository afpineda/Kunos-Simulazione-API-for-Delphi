object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'Msg Handler'
  ClientHeight = 306
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object MEmo_log: TMemo
    Left = 0
    Top = 122
    Width = 505
    Height = 184
    Align = alBottom
    Lines.Strings = (
      'MEmo_log')
    TabOrder = 0
  end
  object Btn_Connect: TButton
    Left = 20
    Top = 16
    Width = 75
    Height = 25
    Caption = 'connect'
    TabOrder = 1
    OnClick = Btn_ConnectClick
  end
  object Btn_disconnect: TButton
    Left = 108
    Top = 16
    Width = 75
    Height = 25
    Caption = 'disconnect'
    TabOrder = 2
    OnClick = Btn_disconnectClick
  end
  object Btn_EntryList: TButton
    Left = 20
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Entry list'
    TabOrder = 3
    OnClick = Btn_EntryListClick
  end
  object Btn_ClearLog: TButton
    Left = 201
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Clear log'
    TabOrder = 4
    OnClick = Btn_ClearLogClick
  end
  object Btn_Receive: TButton
    Left = 422
    Top = 16
    Width = 75
    Height = 25
    Caption = 'RECEIVE'
    TabOrder = 5
    OnClick = Btn_ReceiveClick
  end
  object Btn_TrackData: TButton
    Left = 108
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Track'
    TabOrder = 6
    OnClick = Btn_TrackDataClick
  end
end
