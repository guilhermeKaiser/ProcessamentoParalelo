object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 202
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CheckListBox1: TCheckListBox
    Left = 8
    Top = 8
    Width = 169
    Height = 129
    Enabled = False
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 88
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object SpinEdit1: TSpinEdit
    Left = 183
    Top = 8
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 5
  end
  object CheckBox1: TCheckBox
    Left = 208
    Top = 80
    Width = 113
    Height = 17
    Caption = 'Cadeira Cabeleireiro'
    Enabled = False
    TabOrder = 3
  end
end
