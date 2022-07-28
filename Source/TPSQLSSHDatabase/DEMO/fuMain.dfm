object fmMain: TfmMain
  Left = 484
  Top = 299
  Width = 633
  Height = 434
  Caption = 'Main form'
  Color = clBtnFace
  Constraints.MinHeight = 434
  Constraints.MinWidth = 633
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    625
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = 'Database tables:'
  end
  object Label2: TLabel
    Left = 192
    Top = 8
    Width = 51
    Height = 13
    Caption = 'Table data'
  end
  object lbTables: TListBox
    Left = 8
    Top = 24
    Width = 177
    Height = 337
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbTablesClick
  end
  object dbgData: TDBGrid
    Left = 192
    Top = 24
    Width = 425
    Height = 305
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object dbnData: TDBNavigator
    Left = 192
    Top = 336
    Width = 430
    Height = 25
    DataSource = DataSource1
    Anchors = [akLeft, akRight, akBottom]
    Flat = True
    TabOrder = 2
  end
  object buConnect: TButton
    Left = 8
    Top = 368
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Connect'
    TabOrder = 3
    OnClick = buConnectClick
  end
  object buDisconnect: TButton
    Left = 88
    Top = 368
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 4
    OnClick = buDisconnectClick
  end
  object buClose: TButton
    Left = 544
    Top = 368
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 5
    OnClick = buCloseClick
  end
  object DataSource1: TDataSource
    DataSet = PSQLT
    Left = 256
    Top = 368
  end
  object PgSSHDB: TPgSSHDatabase
    Params.Strings = (
      'Port=5432')
    SSLMode = sslDisable
    UseSSL = False
    SSHPort = 0
    SSHTimeout = 120
    SSHUseCompression = False
    SSHCompressionLevel = 6
    Left = 176
    Top = 368
  end
  object PSQLT: TPSQLTable
    Database = PgSSHDB
    OIDAsInt = False
    ByteaAsEscString = False
    Left = 208
    Top = 368
  end
end
