object fmProgress: TfmProgress
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Progress'
  ClientHeight = 51
  ClientWidth = 351
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblCount: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = 'lblCount'
  end
  object pbProgress: TProgressBar
    Left = 8
    Top = 27
    Width = 333
    Height = 16
    TabOrder = 0
  end
end
