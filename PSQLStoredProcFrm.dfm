object PSQLStoredProcProp: TPSQLStoredProcProp
  Left = 245
  Top = 145
  Width = 525
  Height = 295
  Caption = 'TPSQLStoredProc Editor...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    517
    261)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 25
    Align = alTop
    Alignment = taLeftJustify
    Caption = '  PostgreSQL Stored Procedure Options'
    Color = clGray
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object OkBtn: TButton
    Left = 328
    Top = 231
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 412
    Top = 231
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ListView1: TListView
    Left = 0
    Top = 25
    Width = 517
    Height = 192
    Align = alTop
    Columns = <
      item
        AutoSize = True
        Caption = 'Full Name'
      end
      item
        Caption = 'Overload ID'
        Width = 60
      end
      item
        AutoSize = True
        Caption = 'Arguments'
      end>
    GridLines = True
    HotTrack = True
    HotTrackStyles = [htHandPoint, htUnderlineHot]
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnColumnClick = ListView1ColumnClick
    OnCompare = ListView1Compare
    OnSelectItem = ListView1SelectItem
  end
end
