object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 327
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    426
    327)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Repo to merge'
  end
  object Label2: TLabel
    Left = 8
    Top = 278
    Width = 82
    Height = 13
    Caption = 'Destination Repo'
  end
  object eDestRepo: TEdit
    Left = 8
    Top = 298
    Width = 153
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 0
  end
  object btnGo: TButton
    Left = 343
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnGoClick
  end
  object lbSourceRepo: TListBox
    Left = 8
    Top = 27
    Width = 410
    Height = 134
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnAdd: TButton
    Left = 262
    Top = 296
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object cbBackup: TCheckBox
    Left = 198
    Top = 300
    Width = 58
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Backup'
    TabOrder = 4
    Visible = False
  end
  object btnSelect: TButton
    Left = 167
    Top = 296
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 5
    OnClick = btnSelectClick
  end
  object lbLog: TListBox
    Left = 8
    Top = 167
    Width = 410
    Height = 105
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
end
