object Form_main: TForm_main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Auto save replay'
  ClientHeight = 141
  ClientWidth = 434
  Color = clBtnFace
  Constraints.MaxHeight = 600
  Constraints.MaxWidth = 800
  Constraints.MinHeight = 200
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = Menu_main
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo_log: TMemo
    Left = 0
    Top = 0
    Width = 434
    Height = 122
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    Lines.Strings = (
      'Memo_log')
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitHeight = 139
  end
  object SB_main: TStatusBar
    Left = 0
    Top = 122
    Width = 434
    Height = 19
    Panels = <
      item
        Bevel = pbNone
        Text = 'status'
        Width = 450
      end>
    ExplicitTop = 191
  end
  object Menu_main: TMainMenu
    Left = 212
    Top = 8
    object Menu_Action: TMenuItem
      Caption = '&Action'
      object Menu_ClearLog: TMenuItem
        Caption = 'Clear &Log'
        OnClick = Menu_ClearLogClick
      end
      object Menu_disable: TMenuItem
        Caption = '&Disable'
        OnClick = Menu_disableClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Menu_Checks: TMenuItem
        Caption = 'Checks'
        object Menu_saveNow: TMenuItem
          Caption = 'Save replay now (for testing)'
          OnClick = Menu_saveNowClick
        end
        object Menu_ShowState: TMenuItem
          Caption = 'Show session state'
          OnClick = Menu_ShowStateClick
        end
      end
    end
    object Menu_About: TMenuItem
      Caption = 'About'
      OnClick = Menu_AboutClick
    end
  end
end
