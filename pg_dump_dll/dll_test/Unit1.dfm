object Form1: TForm1
  Left = 210
  Top = 103
  Width = 269
  Height = 145
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 112
    Top = 24
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 16
    Top = 20
    Width = 75
    Height = 25
    Caption = 'execute'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 60
    Width = 75
    Height = 25
    Caption = 'restore'
    TabOrder = 1
    OnClick = Button2Click
  end
end
