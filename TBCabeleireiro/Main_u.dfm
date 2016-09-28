object Form1: TForm1
  Left = 663
  Top = 393
  Caption = 'Form1'
  ClientHeight = 248
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 208
    Top = 84
    Width = 97
    Height = 13
    Caption = 'Cadeira Cabeleireiro'
  end
  object Label2: TLabel
    Left = 8
    Top = 8
    Width = 116
    Height = 13
    Caption = 'Quantidade de Cadeiras'
  end
  object Label3: TLabel
    Left = 8
    Top = 32
    Width = 85
    Height = 13
    Caption = 'Tempo para corte'
  end
  object Label4: TLabel
    Left = 192
    Top = 8
    Width = 90
    Height = 13
    Caption = 'Tempo para dormir'
  end
  object Label5: TLabel
    Left = 192
    Top = 32
    Width = 118
    Height = 13
    Caption = 'Tempo para novo cliente'
  end
  object ListCadeirasClientes: TCheckListBox
    Left = 8
    Top = 57
    Width = 160
    Height = 128
    Enabled = False
    ItemHeight = 13
    TabOrder = 0
  end
  object btnIniciar: TButton
    Left = 93
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Iniciar'
    TabOrder = 1
    OnClick = btnIniciarClick
  end
  object SpinQtdCadeiras: TSpinEdit
    Left = 135
    Top = 5
    Width = 33
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 2
    Value = 5
  end
  object cbCadeiraCabeleireiro: TCheckBox
    Left = 192
    Top = 119
    Width = 160
    Height = 17
    Caption = 'Cadeira Cabeleireiro'
    Enabled = False
    TabOrder = 3
  end
  object SpinTempCorte: TSpinEdit
    Left = 135
    Top = 29
    Width = 33
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 4
    Value = 5
  end
  object SpinTempDormir: TSpinEdit
    Left = 319
    Top = 5
    Width = 33
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 5
    Value = 5
  end
  object SpinTempNovoCliente: TSpinEdit
    Left = 319
    Top = 29
    Width = 33
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 6
    Value = 5
  end
  object btnParar: TButton
    Left = 237
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Parar'
    Enabled = False
    TabOrder = 7
    OnClick = btnPararClick
  end
end
