object Form_main: TForm_main
  Left = 0
  Top = 0
  Caption = 'KS Broadcast API demo'
  ClientHeight = 371
  ClientWidth = 602
  Color = clBtnFace
  Constraints.MinHeight = 410
  Constraints.MinWidth = 330
  DoubleBuffered = True
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
    Width = 602
    Height = 371
    ActivePage = Page_Plotter
    Align = alClient
    TabOrder = 0
    object Page_Connect: TTabSheet
      Caption = 'Connection'
      ExplicitWidth = 306
      object Panel_connection: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 201
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Panel_connection'
        ShowCaption = False
        TabOrder = 0
        ExplicitWidth = 306
        DesignSize = (
          594
          201)
        object Edt_CommandPwd: TLabeledEdit
          Left = 11
          Top = 168
          Width = 197
          Height = 21
          EditLabel.Width = 96
          EditLabel.Height = 13
          EditLabel.Caption = 'Command password'
          EditLabel.Layout = tlCenter
          TabOrder = 4
        end
        object Edt_ConnPwd: TLabeledEdit
          Left = 11
          Top = 116
          Width = 197
          Height = 21
          EditLabel.Width = 103
          EditLabel.Height = 13
          EditLabel.Caption = 'Connection password'
          EditLabel.Layout = tlCenter
          TabOrder = 3
        end
        object Edt_Port: TLabeledEdit
          Left = 11
          Top = 68
          Width = 82
          Height = 21
          EditLabel.Width = 82
          EditLabel.Height = 13
          EditLabel.Caption = 'UDP Port number'
          EditLabel.Layout = tlCenter
          NumbersOnly = True
          TabOrder = 1
        end
        object Edt_Host: TLabeledEdit
          Left = 11
          Top = 24
          Width = 197
          Height = 21
          EditLabel.Width = 138
          EditLabel.Height = 13
          EditLabel.Caption = 'ACC hostname or IP address'
          EditLabel.Layout = tlCenter
          TabOrder = 0
        end
        object Btn_DefConnField: TButton
          Left = 133
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Set defaults'
          TabOrder = 2
          OnClick = Btn_DefConnFieldClick
        end
        object AI_Receiving: TActivityIndicator
          Left = 552
          Top = 13
          Anchors = [akTop, akRight]
          FrameDelay = 100
          IndicatorType = aitRotatingSector
          ExplicitLeft = 264
        end
      end
      object Memo_Log: TMemo
        Left = 0
        Top = 242
        Width = 594
        Height = 101
        Align = alClient
        Lines.Strings = (
          'Memo_Log')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        ExplicitWidth = 306
      end
      object Panel_ConnectButtons: TPanel
        Left = 0
        Top = 201
        Width = 594
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 2
        ExplicitWidth = 306
        DesignSize = (
          594
          41)
        object Btn_connect: TButton
          Left = 11
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Connect'
          Default = True
          TabOrder = 0
          OnClick = Btn_connectClick
        end
        object Btn_Disconnect: TButton
          Left = 101
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Disconnect'
          TabOrder = 1
          OnClick = Btn_DisconnectClick
        end
        object Btn_ClearLog: TButton
          Left = 514
          Top = 6
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Clear Log'
          TabOrder = 2
          OnClick = Btn_ClearLogClick
          ExplicitLeft = 226
        end
      end
    end
    object Page_carEntries: TTabSheet
      Caption = 'Entries'
      ImageIndex = 1
      ExplicitWidth = 306
      object Lbl_drivers: TLabel
        Left = 0
        Top = 167
        Width = 594
        Height = 13
        Align = alTop
        Caption = 'Drivers'
        ExplicitWidth = 34
      end
      object Lbl_cars: TLabel
        Left = 0
        Top = 0
        Width = 594
        Height = 13
        Align = alTop
        Caption = 'Cars'
        ExplicitWidth = 22
      end
      object Splitter1: TSplitter
        Left = 0
        Top = 157
        Width = 594
        Height = 10
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 200
        ExplicitWidth = 420
      end
      object Grid_carEntries: TStringGrid
        Left = 0
        Top = 13
        Width = 594
        Height = 144
        Hint = 'Double click for camera focus'
        Align = alTop
        ColCount = 4
        FixedCols = 0
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnDblClick = Grid_carEntriesDblClick
        ExplicitWidth = 306
      end
      object Grid_drivers: TStringGrid
        Left = 0
        Top = 180
        Width = 594
        Height = 163
        Align = alClient
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
        TabOrder = 1
        ExplicitWidth = 306
      end
    end
    object Page_CarData: TTabSheet
      Caption = 'Car'
      ImageIndex = 2
      ExplicitWidth = 306
      object List_CarData: TListView
        Left = 0
        Top = 0
        Width = 594
        Height = 343
        Align = alClient
        Columns = <
          item
            Caption = 'Car Idx'
          end
          item
            Caption = 'Driver Idx'
            Width = 70
          end
          item
            Caption = 'X,Y,Yaw'
            Width = 80
          end
          item
            Caption = 'Official pos'
            Width = 70
          end
          item
            Caption = 'Track pos'
            Width = 60
          end
          item
            Caption = 'Lap pos'
            Width = 60
          end
          item
            Caption = 'Lap count'
            Width = 70
          end
          item
            Caption = 'Last lap time'
            Width = 110
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ExplicitWidth = 306
      end
    end
    object Page_SessionData: TTabSheet
      Caption = 'Session'
      ImageIndex = 3
      ExplicitWidth = 306
      object VE_Session: TValueListEditor
        Left = 0
        Top = 0
        Width = 594
        Height = 343
        Align = alClient
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goAlwaysShowEditor, goThumbTracking]
        TabOrder = 0
        ExplicitWidth = 306
        ColWidths = (
          150
          150)
      end
    end
    object Page_TrackData: TTabSheet
      Caption = 'Track'
      ImageIndex = 4
      ExplicitWidth = 306
      object Lbl_Hud: TLabel
        Left = 0
        Top = 74
        Width = 594
        Height = 13
        Align = alTop
        Caption = 'HUD Pages'
        ExplicitWidth = 53
      end
      object Lbl_Cams: TLabel
        Left = 0
        Top = 200
        Width = 594
        Height = 13
        Align = alTop
        Caption = 'Cameras'
        ExplicitWidth = 42
      end
      object Splitter2: TSplitter
        Left = 0
        Top = 190
        Width = 594
        Height = 10
        Cursor = crVSplit
        Align = alTop
        ExplicitLeft = 3
        ExplicitTop = 144
        ExplicitWidth = 306
      end
      object VE_Track: TValueListEditor
        Left = 0
        Top = 25
        Width = 594
        Height = 49
        Align = alTop
        DisplayOptions = [doAutoColResize, doKeyColFixed]
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect, goThumbTracking]
        ScrollBars = ssNone
        Strings.Strings = (
          'a=1'
          'b=2'
          'c=3')
        TabOrder = 0
        ExplicitWidth = 306
        ColWidths = (
          150
          150)
      end
      object LV_HUD: TListView
        Left = 0
        Top = 87
        Width = 594
        Height = 103
        Hint = 'Double click to request HUD page'
        Align = alTop
        Columns = <>
        Items.ItemData = {
          05440000000200000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
          0004740065007300740000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF0000
          0000047400650073007400}
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        ViewStyle = vsList
        OnDblClick = LV_HUDDblClick
        ExplicitWidth = 306
      end
      object LV_Cam: TListView
        Left = 0
        Top = 213
        Width = 594
        Height = 130
        Hint = 'Double click to request camera focus'
        Align = alClient
        Columns = <
          item
            Caption = 'Set'
            Width = 150
          end
          item
            Caption = 'Camera'
            Width = 150
          end>
        Items.ItemData = {
          05620000000200000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
          0004740065007300740004740065007300740068F3BF2300000000FFFFFFFFFF
          FFFFFF01000000FFFFFFFF000000000474006500730074000474006500730074
          00D8D0BF23FFFFFFFF}
        MultiSelect = True
        ReadOnly = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        ViewStyle = vsReport
        OnDblClick = LV_CamDblClick
        ExplicitWidth = 306
      end
      object Btn_ForceTrackData: TButton
        Left = 0
        Top = 0
        Width = 594
        Height = 25
        Align = alTop
        Caption = 'Force update'
        TabOrder = 3
        OnClick = Btn_ForceTrackDataClick
        ExplicitWidth = 306
      end
    end
    object Page_Plotter: TTabSheet
      Caption = 'Plotter'
      ImageIndex = 5
      ExplicitLeft = 8
      ExplicitTop = 28
      object PB_Plotter: TPaintBox
        Left = 0
        Top = 48
        Width = 594
        Height = 295
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        OnPaint = PB_PlotterPaint
      end
      object Btn_ClearPlotter: TButton
        Left = 3
        Top = 9
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 0
        OnClick = Btn_ClearPlotterClick
      end
    end
  end
  object Timer_activityCheck: TTimer
    OnTimer = Timer_activityCheckTimer
    Left = 492
    Top = 28
  end
end
