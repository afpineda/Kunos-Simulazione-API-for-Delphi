object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'Test de presiones automatizado (AC/ACC)'
  ClientHeight = 361
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo_log: TMemo
    Left = 0
    Top = 73
    Width = 384
    Height = 288
    Align = alClient
    Lines.Strings = (
      'Memo_log')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 73
    Align = alTop
    Caption = 'Panel_main'
    ShowCaption = False
    TabOrder = 1
    object Lbl_status: TLabel
      Left = 1
      Top = 18
      Width = 382
      Height = 24
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Lbl_status'
      ExplicitLeft = 2
      ExplicitTop = 24
    end
    object Chk_stayOnTop: TCheckBox
      Left = 1
      Top = 1
      Width = 382
      Height = 17
      Align = alTop
      Caption = 'Mantener ventana visible'
      TabOrder = 0
      OnClick = Chk_stayOnTopClick
    end
    object Btn_ClearLog: TButton
      Left = 1
      Top = 42
      Width = 382
      Height = 25
      Align = alTop
      Caption = 'Borrar log'
      TabOrder = 1
      OnClick = Btn_ClearLogClick
    end
  end
  object Timer_main: TTimer
    OnTimer = Timer_mainTimer
    Left = 640
    Top = 8
  end
end
