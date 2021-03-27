object Form_main: TForm_main
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'AC/ACC Automated Tyre Pressure Test'
  ClientHeight = 410
  ClientWidth = 420
  Color = clSkyBlue
  Constraints.MinHeight = 400
  Constraints.MinWidth = 220
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  Menu = Menu_main
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object Status_main: TStatusBar
    Left = 0
    Top = 389
    Width = 420
    Height = 21
    Hint = 'test'
    Constraints.MinHeight = 21
    Panels = <
      item
        Text = 'TEST'
        Width = 500
      end>
    ParentShowHint = False
    ShowHint = True
  end
  object PC_main: TPageControl
    Left = 0
    Top = 0
    Width = 420
    Height = 389
    ActivePage = Page_Test
    Align = alClient
    TabOrder = 1
    object Page_Test: TTabSheet
      Caption = 'Test'
      object VLE_test: TValueListEditor
        Left = 0
        Top = 0
        Width = 412
        Height = 359
        Align = alClient
        Constraints.MinHeight = 45
        DefaultColWidth = 120
        DisplayOptions = [doAutoColResize]
        Enabled = False
        Options = [goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        Strings.Strings = (
          'car='
          'Tyre='
          'Front Left (cold)='
          'a='
          'a='
          'a='
          'Air Temp=0'
          'Road Temp=0')
        TabOrder = 0
        TitleCaptions.Strings = (
          'Tag'
          'Value')
        ColWidths = (
          203
          203)
      end
    end
    object Page_Results: TTabSheet
      Caption = 'Results'
      ImageIndex = 1
      OnShow = Page_ResultsShow
      object Lbl_NormalizedPressures: TLabel
        Left = 0
        Top = 135
        Width = 412
        Height = 16
        Align = alTop
        Alignment = taCenter
        Caption = 'Corrected cold tyre pressure'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 179
      end
      object Panel_TargetPSI: TPanel
        Left = 0
        Top = 0
        Width = 412
        Height = 135
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel_TargetPSI'
        ShowCaption = False
        TabOrder = 0
        DesignSize = (
          412
          135)
        object Lbl_target: TLabel
          Left = 0
          Top = 0
          Width = 412
          Height = 16
          Align = alTop
          Alignment = taCenter
          Caption = 'Target pressures'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 104
        end
        object Edit_Target_FL: TLabeledEdit
          Left = 4
          Top = 34
          Width = 49
          Height = 23
          EditLabel.Width = 77
          EditLabel.Height = 15
          EditLabel.Caption = 'Edit_Target_FL'
          TabOrder = 0
          Text = '00.0'
          OnEnter = Edit_Target_FLEnter
          OnExit = Edit_Target_FLExit
          OnKeyUp = Edit_Target_FLKeyUp
        end
        object Edit_Target_FR: TLabeledEdit
          Left = 311
          Top = 34
          Width = 49
          Height = 23
          Anchors = [akTop, akRight]
          EditLabel.Width = 77
          EditLabel.Height = 15
          EditLabel.Caption = 'Edit_Target_FL'
          TabOrder = 1
          Text = '00.0'
          OnEnter = Edit_Target_FLEnter
          OnExit = Edit_Target_FLExit
          OnKeyUp = Edit_Target_FLKeyUp
        end
        object Edit_Target_RL: TLabeledEdit
          Left = 4
          Top = 88
          Width = 49
          Height = 23
          EditLabel.Width = 77
          EditLabel.Height = 15
          EditLabel.Caption = 'Edit_Target_FL'
          TabOrder = 2
          Text = '00.0'
          OnEnter = Edit_Target_FLEnter
          OnExit = Edit_Target_FLExit
          OnKeyUp = Edit_Target_FLKeyUp
        end
        object Edit_Target_RR: TLabeledEdit
          Left = 311
          Top = 88
          Width = 49
          Height = 23
          Anchors = [akTop, akRight]
          EditLabel.Width = 77
          EditLabel.Height = 15
          EditLabel.Caption = 'Edit_Target_FL'
          TabOrder = 3
          Text = '00.0'
          OnEnter = Edit_Target_FLEnter
          OnExit = Edit_Target_FLExit
          OnKeyUp = Edit_Target_FLKeyUp
        end
        object Chk_Link: TCheckBox
          Left = 4
          Top = 117
          Width = 289
          Height = 20
          Caption = 'Link edit boxes'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
      end
      object VLE_Results: TValueListEditor
        Left = 0
        Top = 151
        Width = 412
        Height = 208
        Align = alClient
        Constraints.MinHeight = 45
        DefaultColWidth = 120
        DisplayOptions = [doAutoColResize]
        Enabled = False
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        Strings.Strings = (
          'car='
          'Tyre='
          'a='
          'a='
          'a='
          'a='
          'Air Temp=0'
          'Road Temp=0')
        TabOrder = 1
        TitleCaptions.Strings = (
          'Tag'
          'Value')
        ColWidths = (
          203
          203)
      end
    end
  end
  object Menu_main: TMainMenu
    Left = 36
    object Menu_action: TMenuItem
      Caption = '&Action'
      OnClick = Menu_actionClick
      object Menu_Restart: TMenuItem
        Caption = '&Restart test'
        OnClick = Menu_RestartClick
      end
      object Menu_DefaultTargetPSI: TMenuItem
        Caption = 'Set &target pressures to default'
        OnClick = Menu_DefaultTargetPSIClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Menu_copy: TMenuItem
        Caption = '&Copy results to clipboard'
        OnClick = Menu_copyClick
      end
    end
    object Menu_globalOptions: TMenuItem
      Caption = '&Options'
      object Menu_TestOptions: TMenuItem
        Caption = '&Pressure test options'
        object Menu_NormalizeAtFinishLine: TMenuItem
          Caption = 'Read hot pressures at &finish line'
          OnClick = Menu_NormalizeAtFinishLineClick
        end
        object Menu_NormalizeAtCarPos: TMenuItem
          Caption = 'Read hot pressures at current &car position'
          OnClick = Menu_NormalizeAtCarPosClick
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object Menu_CompleteAtTeleport: TMenuItem
          Caption = 'Complete test at garage &teleporting'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = Menu_CompleteAtTeleportClick
        end
        object Menu_Min2Laps: TMenuItem
          Caption = 'Complete test after &2 laps (qualification setups)'
          GroupIndex = 1
          RadioItem = True
          OnClick = Menu_Min2LapsClick
        end
        object Menu_Min5Laps: TMenuItem
          Caption = 'Complete test after &5 laps'
          GroupIndex = 1
          RadioItem = True
          OnClick = Menu_Min5LapsClick
        end
        object Menu_Min7Laps: TMenuItem
          Caption = 'Complete test after &7 laps (race setups)'
          GroupIndex = 1
          RadioItem = True
          OnClick = Menu_Min7LapsClick
        end
      end
      object Menu_AppOptions: TMenuItem
        Caption = 'App option&s'
        object Menu_stayOnTop: TMenuItem
          Caption = '&Keep this window visible'
          OnClick = Menu_stayOnTopClick
        end
        object Menu_CopyPasteOptions: TMenuItem
          Caption = 'Cop&y-paste options'
          OnClick = Menu_CopyPasteOptionsClick
        end
        object N1: TMenuItem
          Caption = '-'
        end
        object Menu_cleanReg: TMenuItem
          Caption = 'C&lear my Windows'#39' registry key (on exit)'
          OnClick = Menu_cleanRegClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Menu_About: TMenuItem
        Caption = 'About'
        OnClick = Menu_AboutClick
      end
    end
  end
  object Timer_main: TTimer
    Interval = 500
    OnTimer = Timer_mainTimer
    Left = 100
    Top = 65532
  end
end
