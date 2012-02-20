object ConnectDlg: TConnectDlg
  Left = 320
  Top = 288
  BorderStyle = bsDialog
  Caption = 'Postgres Connect Dialog'
  ClientHeight = 190
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
    Top = 0
    Width = 340
    Height = 153
    Align = alTop
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 8
    Top = 37
    Width = 39
    Height = 13
    Caption = 'User ID:'
  end
  object Label2: TLabel
    Left = 8
    Top = 65
    Width = 49
    Height = 13
    Caption = 'Password:'
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = 'Database Name:'
  end
  object Label4: TLabel
    Left = 8
    Top = 94
    Width = 68
    Height = 13
    Caption = 'Host Name/IP'
  end
  object Label5: TLabel
    Left = 8
    Top = 122
    Width = 56
    Height = 13
    Caption = 'Server Port:'
  end
  object DBUserID: TEdit
    Left = 96
    Top = 35
    Width = 233
    Height = 21
    TabOrder = 1
  end
  object DBPasswd: TEdit
    Left = 96
    Top = 63
    Width = 233
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
  end
  object DBName: TEdit
    Left = 96
    Top = 6
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object DBHost: TEdit
    Left = 96
    Top = 92
    Width = 233
    Height = 21
    TabOrder = 3
  end
  object DBPort: TEdit
    Left = 96
    Top = 120
    Width = 233
    Height = 21
    TabOrder = 4
  end
  object OkBtn: TButton
    Left = 176
    Top = 158
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 5
  end
  object CancelBtn: TButton
    Left = 260
    Top = 158
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
end
