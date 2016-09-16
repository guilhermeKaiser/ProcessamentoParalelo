unit Main_u;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
    CheckListBox1: TCheckListBox;
    Button1: TButton;
    SpinEdit1: TSpinEdit;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    QuantidadeCadeiras: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Cabeleireiro_u, Cliente_u;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  vCabeleireiro: Cabeleireiro;
  vCliente: Cliente;
begin
  SpinEdit1.Enabled := False;
  try
    QuantidadeCadeiras := SpinEdit1.Value;

    for i := 0 to QuantidadeCadeiras - 1 do
    begin
      CheckListBox1.AddItem('-1', CheckListBox1);
      CheckListBox1.Checked[i] := False;
//      CheckListBox1.AddItem(IntToStr(5-i), CheckListBox1);
//      CheckListBox1.Checked[i] := True;
    end;

    vCabeleireiro := Cabeleireiro.Create(True, True, CheckListBox1, QuantidadeCadeiras, CheckBox1);
    vCliente := Cliente.Create(True, True, CheckListBox1, QuantidadeCadeiras, CheckBox1);
    vCabeleireiro.Resume;
    vCliente.Resume;



  finally
    SpinEdit1.Enabled := True;
  end;
end;

end.
