object Form1: TForm1
  Left = 185
  Top = 101
  BorderStyle = bsDialog
  Caption = 'DAC for PostreSQL'#39's TPSQLMonitor component usage example'
  ClientHeight = 338
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object GBQuery: TGroupBox
    Left = 8
    Top = 40
    Width = 281
    Height = 273
    Caption = ' SQL Query '
    Enabled = False
    TabOrder = 0
    object resultLb: TLabel
      Left = 8
      Top = 128
      Width = 56
      Height = 13
      Caption = 'Query result'
    end
    object DBGrid: TDBGrid
      Left = 9
      Top = 145
      Width = 264
      Height = 120
      DataSource = DataSource
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object mSQL: TMemo
      Left = 8
      Top = 16
      Width = 265
      Height = 65
      Lines.Strings = (
        'SELECT version();')
      ScrollBars = ssBoth
      TabOrder = 1
    end
    object bOpen: TButton
      Left = 8
      Top = 88
      Width = 265
      Height = 25
      Caption = 'TPSQLQuery.Open()'
      TabOrder = 2
      OnClick = bOpenClick
    end
  end
  object bConnect: TButton
    Left = 8
    Top = 8
    Width = 153
    Height = 25
    Caption = 'Connect to database'
    TabOrder = 1
    OnClick = bConnectClick
  end
  object GBMonitor: TGroupBox
    Left = 296
    Top = 8
    Width = 273
    Height = 305
    Caption = ' SQL Monitor '
    TabOrder = 2
    object mMonitor: TMemo
      Left = 8
      Top = 16
      Width = 257
      Height = 281
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 319
    Width = 575
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Not connected'
  end
  object DataSource: TDataSource
    DataSet = PSQLQuery
    Left = 24
    Top = 192
  end
  object PSQLMonitor: TPSQLMonitor
    OnSQL = PSQLMonitorSQL
    TraceFlags = [tfQPrepare, tfQExecute, tfQFetch, tfConnect, tfTransact, tfMisc]
    Left = 336
    Top = 48
  end
  object PSQLQuery: TPSQLQuery
    Database = PSQLDatabase
    Options = [dsoUseGUIDField]
    Left = 32
  end
  object PSQLDatabase: TPSQLDatabase
    AfterConnect = PSQLDatabaseAfterConnect
    BeforeDisconnect = PSQLDatabaseBeforeDisconnect
    Params.Strings = (
      'user=postgres'
      'dbname=dbdemos'
      'connect_timeout=15'
      'port=5432'
      'sslmode=prefer'
      'host=localhost')
  end
end
