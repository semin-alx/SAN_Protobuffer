object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Example08_KonturDiadoc'
  ClientHeight = 332
  ClientWidth = 595
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    595
    332)
  TextHeight = 15
  object Label1: TLabel
    Left = 31
    Top = 32
    Width = 43
    Height = 15
    Caption = 'API_KEY'
  end
  object Label2: TLabel
    Left = 18
    Top = 61
    Width = 56
    Height = 15
    Caption = 'User name'
  end
  object Label3: TLabel
    Left = 24
    Top = 90
    Width = 50
    Height = 15
    Caption = 'Password'
  end
  object edtApiKey: TEdit
    Left = 78
    Top = 29
    Width = 251
    Height = 23
    TabOrder = 0
  end
  object edtUserName: TEdit
    Left = 78
    Top = 58
    Width = 155
    Height = 23
    TabOrder = 1
  end
  object edtPassword: TEdit
    Left = 78
    Top = 87
    Width = 155
    Height = 23
    PasswordChar = '*'
    TabOrder = 2
  end
  object btnGetMyOrganizations: TButton
    Left = 360
    Top = 28
    Width = 225
    Height = 25
    Caption = 'GetMyOrganizations'
    TabOrder = 3
    OnClick = btnGetMyOrganizationsClick
  end
  object mmResult: TMemo
    Left = 8
    Top = 136
    Width = 577
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
    ExplicitWidth = 567
    ExplicitHeight = 167
  end
end
