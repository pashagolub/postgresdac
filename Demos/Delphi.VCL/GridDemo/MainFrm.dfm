object Form1: TForm1
  Left = 245
  Top = 216
  Width = 696
  Height = 480
  Caption = 'Grid Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 33
    Width = 688
    Height = 347
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 380
    Width = 688
    Height = 25
    DataSource = DataSource1
    Align = alBottom
    Flat = True
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 405
    Width = 688
    Height = 41
    Align = alBottom
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 70
      Height = 13
      Caption = 'Row 0, Total 0'
    end
    object Closebtn: TButton
      Left = 608
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Exit'
      ModalResult = 2
      TabOrder = 0
      OnClick = ClosebtnClick
    end
    object Connectbtn: TButton
      Left = 528
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 1
      OnClick = ConnectbtnClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 33
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 3
    object StaticText1: TStaticText
      Left = 2
      Top = 2
      Width = 684
      Height = 29
      Align = alClient
      BorderStyle = sbsSunken
      Caption = 
        'This demo shows how to bind dbAware Objects, and get Record Numb' +
        'er and Record Count'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object Database1: TPSQLDatabase
    DatabaseName = 'dbdemos'
    UserName = 'postgres'
    Host = '<Enter Your Host>'
    Params.Strings = (
      'Port=5432'
      'Host=<Enter Your Host>'
      'DatabaseName=dbdemos'
      'UID=postgres')
    Left = 112
    Top = 416
  end
  object Table1: TPSQLTable
    Database = Database1
    AfterScroll = Table1AfterScroll
    TableName = '"customer"'
    Left = 144
    Top = 416
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 176
    Top = 416
  end
end
