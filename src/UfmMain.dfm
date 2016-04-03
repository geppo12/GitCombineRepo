object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 428
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    557
    428)
  PixelsPerInch = 120
  TextHeight = 17
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 92
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Repo to merge'
  end
  object Label2: TLabel
    Left = 10
    Top = 364
    Width = 104
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Destination Repo'
  end
  object eDestRepo: TEdit
    Left = 10
    Top = 390
    Width = 201
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    TabOrder = 0
  end
  object btnGo: TButton
    Left = 449
    Top = 387
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnGoClick
  end
  object lbSourceRepo: TListBox
    Left = 10
    Top = 35
    Width = 537
    Height = 176
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnAdd: TButton
    Left = 343
    Top = 387
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object cbBackup: TCheckBox
    Left = 259
    Top = 392
    Width = 76
    Height = 23
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'Backup'
    TabOrder = 4
    Visible = False
  end
  object btnSelect: TButton
    Left = 218
    Top = 387
    Width = 33
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 5
    OnClick = btnSelectClick
  end
  object lbLog: TListBox
    Left = 10
    Top = 218
    Width = 537
    Height = 138
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
end
