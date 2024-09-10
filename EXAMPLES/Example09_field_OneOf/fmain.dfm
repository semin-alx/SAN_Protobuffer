object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Example02_first_message'
  ClientHeight = 320
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object btnSave: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 0
    OnClick = btnSaveClick
  end
  object btnLoad: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Read'
    TabOrder = 1
    OnClick = btnLoadClick
  end
  object mmContents: TMemo
    Left = 112
    Top = 17
    Width = 492
    Height = 288
    TabOrder = 2
  end
end
