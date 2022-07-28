object PSQLConnForm: TPSQLConnForm
  Left = 381
  Top = 210
  BorderStyle = bsDialog
  Caption = 'TPSQLDatabase Editor...'
  ClientHeight = 240
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 25
    Width = 340
    Height = 176
    Align = alTop
    Shape = bsFrame
  end
  object laUser: TLabel
    Left = 8
    Top = 65
    Width = 39
    Height = 13
    Caption = '&User ID:'
    FocusControl = DBUserID
  end
  object laPass: TLabel
    Left = 8
    Top = 93
    Width = 49
    Height = 13
    Caption = '&Password:'
    FocusControl = DBPasswd
  end
  object laDbName: TLabel
    Left = 8
    Top = 36
    Width = 80
    Height = 13
    Caption = 'Database &Name:'
    FocusControl = DBName
  end
  object laHost: TLabel
    Left = 8
    Top = 122
    Width = 68
    Height = 13
    Caption = '&Host Name/IP'
    FocusControl = DBHost
  end
  object laPort: TLabel
    Left = 8
    Top = 150
    Width = 56
    Height = 13
    Caption = 'Server &Port:'
    FocusControl = DBPort
  end
  object DBUserID: TEdit
    Left = 96
    Top = 63
    Width = 233
    Height = 21
    TabOrder = 1
  end
  object DBPasswd: TEdit
    Left = 96
    Top = 91
    Width = 233
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object DBName: TEdit
    Left = 96
    Top = 34
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object DBHost: TEdit
    Left = 96
    Top = 120
    Width = 233
    Height = 21
    TabOrder = 3
  end
  object DBPort: TEdit
    Left = 96
    Top = 148
    Width = 233
    Height = 21
    TabOrder = 4
  end
  object DBLogin: TCheckBox
    Left = 8
    Top = 180
    Width = 97
    Height = 17
    Caption = '&Login Prompt:'
    TabOrder = 5
  end
  object OkBtn: TButton
    Left = 176
    Top = 206
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object CancelBtn: TButton
    Left = 260
    Top = 206
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 340
    Height = 25
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '  PostgreSQL Connection Options'
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
  end
end
