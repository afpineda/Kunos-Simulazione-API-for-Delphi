object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'PoC autosave replay'
  ClientHeight = 364
  ClientWidth = 569
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
  object PC_main: TPageControl
    Left = 0
    Top = 0
    Width = 569
    Height = 364
    ActivePage = Page_ACCconfig
    Align = alClient
    TabOrder = 0
    object Page_log: TTabSheet
      Caption = 'Log'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo_log: TMemo
        Left = 0
        Top = 188
        Width = 561
        Height = 148
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'Memo_log')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object Btn_Reg: TButton
        Left = 12
        Top = 16
        Width = 125
        Height = 25
        Caption = 'Register/Unregister'
        TabOrder = 1
        OnClick = Btn_RegClick
      end
      object VE_main: TValueListEditor
        Left = 12
        Top = 54
        Width = 306
        Height = 119
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        TabOrder = 2
        ColWidths = (
          144
          156)
      end
    end
    object Page_sendinput: TTabSheet
      Caption = 'Send Input'
      ImageIndex = 1
      ExplicitLeft = 8
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Btn_SendToNotepad: TButton
        Left = 28
        Top = 36
        Width = 161
        Height = 25
        Caption = 'Btn_SendToNotepad'
        TabOrder = 0
        OnClick = Btn_SendToNotepadClick
      end
      object Btn_SendToAcc: TButton
        Left = 28
        Top = 72
        Width = 281
        Height = 25
        Caption = 'Send ESC to ACC'
        TabOrder = 1
        OnClick = Btn_SendToAccClick
      end
    end
    object Page_ACCconfig: TTabSheet
      Caption = 'Page_ACCconfig'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Btn_ReadBrdCfg: TButton
        Left = 8
        Top = 16
        Width = 153
        Height = 25
        Caption = 'Broadcasting cfg'
        TabOrder = 0
        OnClick = Btn_ReadBrdCfgClick
      end
    end
  end
  object Timer_main: TTimer
    OnTimer = Timer_mainTimer
    Left = 448
    Top = 32
  end
end
