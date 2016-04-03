object fmAbout: TfmAbout
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 177
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object lblAppName: TLabel
    Left = 31
    Top = 31
    Width = 110
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Git Combine Repo'
  end
  object Label1: TLabel
    Left = 31
    Top = 56
    Width = 243
    Height = 17
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = #169' 2011-2016 Ing Giuseppe Monteleone'
  end
  object btnOK: TButton
    Left = 234
    Top = 133
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object LinkLabel1: TLinkLabel
    Left = 31
    Top = 94
    Width = 165
    Height = 21
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
