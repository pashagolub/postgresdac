object fmLogin: TfmLogin
  Left = 392
  Top = 286
  BorderStyle = bsDialog
  Caption = 'mySQL Logon'
  ClientHeight = 349
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    384
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = -1
    Top = 312
    Width = 385
    Height = 2
    Shape = bsTopLine
  end
  object pcProperties: TPageControl
    Left = 8
    Top = 8
    Width = 374
    Height = 297
    ActivePage = tsSSH
    TabOrder = 0
    object tsConnection: TTabSheet
      Caption = 'Connection'
      object laHost: TLabel
        Left = 12
        Top = 12
        Width = 33
        Height = 13
        AutoSize = False
        Caption = '&Host'
        FocusControl = edHost
        Transparent = True
        Layout = tlCenter
      end
      object laUser: TLabel
        Left = 12
        Top = 66
        Width = 33
        Height = 13
        AutoSize = False
        Caption = '&User'
        FocusControl = edUser
        Transparent = True
        Layout = tlCenter
      end
      object laPassword: TLabel
        Left = 231
        Top = 66
        Width = 46
        Height = 13
        AutoSize = False
        Caption = 'Pass&word'
        FocusControl = edPassword
        Transparent = True
        Layout = tlCenter
      end
      object laPort: TLabel
        Left = 231
        Top = 12
        Width = 33
        Height = 13
        AutoSize = False
        Caption = '&Port'
        FocusControl = edPort
        Transparent = True
        Layout = tlCenter
      end
      object laTimeOut: TLabel
        Left = 12
        Top = 149
        Width = 64
        Height = 13
        Caption = '&Timeout (sec)'
        FocusControl = edTimeOut
        Transparent = True
      end
      object laDBName: TLabel
        Left = 12
        Top = 176
        Width = 69
        Height = 21
        AutoSize = False
        Caption = '&Database'
        Transparent = True
        Layout = tlCenter
      end
      object bvOptions: TBevel
        Left = 8
        Top = 136
        Width = 354
        Height = 9
        Shape = bsTopLine
      end
      object edHost: TEdit
        Left = 12
        Top = 33
        Width = 209
        Height = 21
        TabOrder = 0
        Text = 'localhost'
      end
      object edUser: TEdit
        Left = 12
        Top = 87
        Width = 209
        Height = 21
        TabOrder = 2
        Text = 'root'
      end
      object edShowPassword: TCheckBox
        Left = 229
        Top = 112
        Width = 129
        Height = 21
        Caption = '&Show password chars'
        TabOrder = 4
        OnClick = edShowPasswordClick
      end
      object edPassword: TEdit
        Left = 231
        Top = 87
        Width = 129
        Height = 21
        PasswordChar = '*'
        TabOrder = 3
      end
      object edPort: TSpinEdit
        Left = 231
        Top = 33
        Width = 129
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 1
        Value = 3306
      end
      object edTimeOut: TSpinEdit
        Left = 100
        Top = 146
        Width = 85
        Height = 22
        MaxValue = 120
        MinValue = 1
        TabOrder = 5
        Value = 5
      end
      object edDBName: TEdit
        Left = 88
        Top = 176
        Width = 273
        Height = 21
        TabOrder = 6
      end
    end
    object tsSSH: TTabSheet
      Caption = 'SSH'
      object laSSHHost: TLabel
        Left = 12
        Top = 44
        Width = 61
        Height = 13
        AutoSize = False
        Caption = 'SSH &Host'
        Enabled = False
        Transparent = True
        Layout = tlCenter
      end
      object laSSHUser: TLabel
        Left = 12
        Top = 98
        Width = 93
        Height = 13
        AutoSize = False
        Caption = 'SSH &User'
        Enabled = False
        FocusControl = edSSHUser
        Transparent = True
        Layout = tlCenter
      end
      object laSSHPassword: TLabel
        Left = 231
        Top = 98
        Width = 114
        Height = 13
        AutoSize = False
        Caption = 'SSH Pass&word'
        Enabled = False
        FocusControl = edSSHPassword
        Transparent = True
        Layout = tlCenter
      end
      object laSSHPort: TLabel
        Left = 231
        Top = 44
        Width = 74
        Height = 13
        AutoSize = False
        Caption = 'SSH &Port'
        Enabled = False
        FocusControl = edSSHPort
        Transparent = True
        Layout = tlCenter
      end
      object laSSHTimeout: TLabel
        Left = 15
        Top = 225
        Width = 89
        Height = 13
        Caption = 'SSH &Timeout (sec)'
        Enabled = False
        FocusControl = edSSHTimeout
        Transparent = True
      end
      object laSSHCompression: TLabel
        Left = 24
        Top = 194
        Width = 94
        Height = 21
        AutoSize = False
        Caption = 'Compresssion Level'
        Enabled = False
        Transparent = True
        Layout = tlCenter
      end
      object edSSHUser: TEdit
        Left = 12
        Top = 119
        Width = 209
        Height = 21
        Enabled = False
        TabOrder = 2
      end
      object edSSHPassword: TEdit
        Left = 231
        Top = 119
        Width = 129
        Height = 21
        Enabled = False
        PasswordChar = '*'
        TabOrder = 3
      end
      object edSSHPort: TSpinEdit
        Left = 231
        Top = 65
        Width = 129
        Height = 22
        Enabled = False
        MaxValue = 1000
        MinValue = 0
        TabOrder = 1
        Value = 22
      end
      object edShowSSHPassword: TCheckBox
        Left = 229
        Top = 144
        Width = 129
        Height = 21
        Caption = '&Show password chars'
        Enabled = False
        TabOrder = 4
        OnClick = edShowSSHPasswordClick
      end
      object edSSHTimeout: TSpinEdit
        Left = 136
        Top = 221
        Width = 85
        Height = 22
        Enabled = False
        MaxValue = 512
        MinValue = 1
        TabOrder = 7
        Value = 120
      end
      object chEnableSSH: TCheckBox
        Left = 12
        Top = 12
        Width = 233
        Height = 21
        Caption = '&Enable SSH Tunneling'
        TabOrder = 0
        OnClick = chEnableSSHClick
      end
      object chSSHCompression: TCheckBox
        Left = 12
        Top = 173
        Width = 205
        Height = 21
        Caption = 'Use Compression'
        Enabled = False
        TabOrder = 5
        OnClick = chSSHCompressionClick
      end
      object edSSHCompression: TComboBox
        Left = 136
        Top = 194
        Width = 85
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 6
        Text = '6'
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9')
      end
      object edSSHHost: TEdit
        Left = 12
        Top = 65
        Width = 209
        Height = 21
        Enabled = False
        TabOrder = 8
        Text = 'localhost'
      end
    end
  end
  object buCancel: TButton
    Left = 304
    Top = 319
    Width = 75
    Height = 25
    HelpContext = 1001
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object buOk: TButton
    Left = 224
    Top = 319
    Width = 75
    Height = 25
    Action = aReady
    Anchors = [akRight, akBottom]
    Default = True
    TabOrder = 2
  end
  object alProfileProps: TActionList
    Left = 8
    Top = 320
    object aReady: TAction
      Caption = '&OK'
      OnExecute = aReadyExecute
      OnUpdate = aReadyUpdate
    end
  end
end
