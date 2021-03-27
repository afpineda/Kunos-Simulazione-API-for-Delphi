object Form_CopyPasteOptions: TForm_CopyPasteOptions
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Copy/paste options'
  ClientHeight = 175
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Calibri'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    304
    175)
  PixelsPerInch = 96
  TextHeight = 15
  object Btn_up: TButton
    Left = 194
    Top = 44
    Width = 104
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Up'
    TabOrder = 0
    OnClick = Btn_upClick
  end
  object Btn_down: TButton
    Left = 194
    Top = 92
    Width = 104
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Down'
    TabOrder = 1
    OnClick = Btn_downClick
  end
  object Panel_fields: TPanel
    Left = 0
    Top = 0
    Width = 189
    Height = 175
    Align = alLeft
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Available fields'
    ShowCaption = False
    TabOrder = 2
    object Lbl_fields: TLabel
      Left = 1
      Top = 1
      Width = 187
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'Available fields'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Calibri'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 81
    end
    object List_fields: TCheckListBox
      Left = 1
      Top = 16
      Width = 187
      Height = 158
      Align = alClient
      ItemHeight = 15
      Items.Strings = (
        'a'
        'b'
        'c'
        'd'
        'e')
      TabOrder = 0
      OnClick = List_fieldsClick
    end
  end
end
