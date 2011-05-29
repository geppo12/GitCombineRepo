object fmAbout: TfmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 135
  ClientWidth = 260
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblAppName: TLabel
    Left = 24
    Top = 24
    Width = 85
    Height = 13
    Caption = 'Git Combine Repo'
  end
  object Label1: TLabel
    Left = 24
    Top = 43
    Width = 162
    Height = 13
    Caption = #169' 2011 Ing Giuseppe Monteleone'
  end
  object btnOK: TButton
    Left = 179
    Top = 102
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object LinkLabel1: TLinkLabel
    Left = 24
    Top = 72
    Width = 129
    Height = 17
    Caption = 
      '<a href="mailto:info@ing-monteleone.com">info@ing-monteleone.com' +
      '</a>'
    TabOrder = 1
    OnLinkClick = LinkLabel1LinkClick
  end
  object Timer1: TTimer
    Interval = 10000
    OnTimer = Timer1Timer
    Left = 216
    Top = 8
  end
end
