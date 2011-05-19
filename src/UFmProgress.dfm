object fmProgress: TfmProgress
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 106
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblRepo: TLabel
    Left = 8
    Top = 5
    Width = 35
    Height = 13
    Caption = 'lblRepo'
  end
  object lblFiles: TLabel
    Left = 8
    Top = 53
    Width = 31
    Height = 13
    Caption = 'lblFiles'
  end
  object pbRepo: TProgressBar
    Left = 8
    Top = 24
    Width = 393
    Height = 16
    TabOrder = 0
  end
  object pbFiles: TProgressBar
    Left = 8
    Top = 72
    Width = 393
    Height = 16
    TabOrder = 1
  end
end
