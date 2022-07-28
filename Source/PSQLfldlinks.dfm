object PSQLLinkFields: TPSQLLinkFields
  Left = 440
  Top = 276
  ActiveControl = IndexList
  BorderStyle = bsDialog
  Caption = 'Field Link Designer'
  ClientHeight = 265
  ClientWidth = 352
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 33
    Width = 343
    Height = 190
    Shape = bsFrame
  end
  object Bevel2: TBevel
    Left = 350
    Top = 285
    Width = 341
    Height = 43
    Shape = bsFrame
  end
  object Label30: TLabel
    Left = 13
    Top = 40
    Width = 57
    Height = 13
    Caption = 'D&etail Fields'
    FocusControl = DetailList
    IsControl = True
  end
  object Label31: TLabel
    Left = 222
    Top = 38
    Width = 62
    Height = 13
    Caption = '&Master Fields'
    FocusControl = MasterList
    IsControl = True
  end
  object IndexLabel: TLabel
    Left = 4
    Top = 10
    Width = 83
    Height = 13
    Caption = 'A&vailable Indexes'
    FocusControl = IndexList
  end
  object Label2: TLabel
    Left = 12
    Top = 142
    Width = 61
    Height = 13
    Caption = '&Joined Fields'
    FocusControl = BindList
  end
  object DetailList: TListBox
    Left = 13
    Top = 55
    Width = 117
    Height = 69
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
    OnClick = BindingListClick
    IsControl = True
  end
  object MasterList: TListBox
    Left = 222
    Top = 54
    Width = 117
    Height = 69
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 2
    OnClick = BindingListClick
    IsControl = True
  end
  object BindList: TListBox
    Left = 12
    Top = 157
    Width = 242
    Height = 56
    IntegralHeight = True
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 3
    OnClick = BindListClick
    IsControl = True
  end
  object IndexList: TComboBox
    Left = 109
    Top = 7
    Width = 192
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = IndexListChange
    OnClick = IndexListChange
  end
  object AddButton: TButton
    Left = 138
    Top = 74
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 4
    OnClick = AddButtonClick
  end
  object DeleteButton: TButton
    Left = 263
    Top = 157
    Width = 75
    Height = 25
    Caption = '&Delete'
    TabOrder = 5
    OnClick = DeleteButtonClick
  end
  object ClearButton: TButton
    Left = 263
    Top = 188
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 6
    OnClick = ClearButtonClick
  end
  object Button1: TButton
    Left = 100
    Top = 231
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
    OnClick = BitBtn1Click
  end
  object Button2: TButton
    Left = 186
    Top = 231
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object Help: TButton
    Left = 272
    Top = 231
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 9
    OnClick = HelpClick
  end
end
