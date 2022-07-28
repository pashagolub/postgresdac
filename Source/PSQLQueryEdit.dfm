object SQLEditForm: TSQLEditForm
  Left = 297
  Top = 131
  Width = 750
  Height = 447
  Caption = 'SQL Editor'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MetaInfoSQLSplitter: TSplitter
    Left = 129
    Top = 19
    Width = 3
    Height = 355
    Cursor = crHSplit
    MinSize = 100
    OnCanResize = MetaInfoSQLSplitterCanResize
    OnMoved = MetaInfoSQLSplitterMoved
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 734
    Height = 19
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object TableListLabel: TLabel
      Left = 3
      Top = 4
      Width = 35
      Height = 13
      Caption = '&Tables:'
      FocusControl = TableList
    end
    object SQLLabel: TLabel
      Left = 166
      Top = 3
      Width = 24
      Height = 13
      Caption = '&SQL:'
    end
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 374
    Width = 734
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Image1: TImage
      Left = 51
      Top = 16
      Width = 10
      Height = 6
      AutoSize = True
      Picture.Data = {
        07544269746D6170A6000000424DA60000000000000076000000280000000A00
        000006000000010004000000000030000000CE0E0000C40E0000100000000000
        000000000000000080000080000000808000800000008000800080800000C0C0
        C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00333333333300000030999999030000003309999033000000333099033300
        000033330033330000003333333333000000}
      Transparent = True
      Visible = False
    end
    object OkButton: TButton
      Left = 487
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 572
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 657
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = HelpButtonClick
    end
    object chbSystemObjects: TCheckBox
      Left = 5
      Top = 8
      Width = 167
      Height = 17
      Caption = 'Sho&w system tables and fields'
      TabOrder = 3
      OnClick = chbSystemObjectsClick
    end
  end
  object MetaInfoPanel: TPanel
    Left = 0
    Top = 19
    Width = 129
    Height = 355
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object TableFieldsSplitter: TSplitter
      Left = 0
      Top = 115
      Width = 129
      Height = 3
      Cursor = crVSplit
      Align = alTop
      Beveled = True
      MinSize = 1
      OnCanResize = TableFieldsSplitterCanResize
    end
    object TableListPanel: TPanel
      Left = 0
      Top = 0
      Width = 129
      Height = 115
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object TableList: TListBox
        Left = 2
        Top = 0
        Width = 128
        Height = 87
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        OnClick = TableListClick
        OnDblClick = AddTableButtonClick
        OnEnter = SQLMemoExit
      end
      object AddTableButton: TButton
        Left = 2
        Top = 90
        Width = 126
        Height = 22
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Add T&able to SQL'
        TabOrder = 1
        OnClick = AddTableButtonClick
      end
    end
    object FieldsPanel: TPanel
      Left = 0
      Top = 118
      Width = 129
      Height = 237
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object FieldListLabel: TLabel
        Left = 3
        Top = 3
        Width = 30
        Height = 13
        Caption = '&Fields:'
        FocusControl = FieldList
      end
      object FieldList: TListBox
        Left = 0
        Top = 19
        Width = 128
        Height = 185
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnDblClick = AddFieldButtonClick
        OnEnter = SQLMemoExit
      end
      object AddFieldButton: TButton
        Left = 2
        Top = 210
        Width = 126
        Height = 22
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Add F&ield to SQL'
        TabOrder = 1
        OnClick = AddFieldButtonClick
      end
    end
  end
  object SQLMemoPanel: TPanel
    Left = 132
    Top = 19
    Width = 602
    Height = 355
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
  end
end
