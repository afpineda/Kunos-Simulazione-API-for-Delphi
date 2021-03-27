object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'KS direct Memory API'
  ClientHeight = 532
  ClientWidth = 765
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PC_main: TPageControl
    Left = 0
    Top = 41
    Width = 765
    Height = 491
    ActivePage = Tab_Static
    Align = alClient
    TabOrder = 0
    object Tab_Static: TTabSheet
      Caption = 'Static'
      ImageIndex = 1
      object Memo_static: TMemo
        Left = 0
        Top = 0
        Width = 757
        Height = 463
        Align = alClient
        Lines.Strings = (
          'Memo_static')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object Tab_Graphics: TTabSheet
      Caption = 'Graphics'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo_graphics: TMemo
        Left = 0
        Top = 0
        Width = 757
        Height = 463
        Align = alClient
        Lines.Strings = (
          'Memo_static')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object Tab_Physics: TTabSheet
      Caption = 'Physics'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo_physics: TMemo
        Left = 0
        Top = 0
        Width = 757
        Height = 463
        Align = alClient
        Lines.Strings = (
          'Memo_static')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 765
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 1
    object Lbl_Info: TLabel
      Left = 138
      Top = 14
      Width = 495
      Height = 21
      AutoSize = False
      Caption = 'No data'
    end
    object Btn_Update1: TButton
      Left = 4
      Top = 10
      Width = 117
      Height = 25
      Caption = 'Update'
      Default = True
      TabOrder = 0
      OnClick = Btn_Update1Click
    end
  end
end
