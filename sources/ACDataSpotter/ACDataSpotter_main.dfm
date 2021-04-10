object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'Data spotter: proof of concept'
  ClientHeight = 231
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo_main: TMemo
    Left = 0
    Top = 0
    Width = 370
    Height = 231
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Chk_stayOnTop: TCheckBox
    Left = 380
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Stay on top'
    TabOrder = 1
    OnClick = Chk_stayOnTopClick
  end
  object Btn_clear: TButton
    Left = 398
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = Btn_clearClick
  end
  object Timer_main: TTimer
    Interval = 500
    OnTimer = Timer_mainTimer
    Left = 428
    Top = 36
  end
end
