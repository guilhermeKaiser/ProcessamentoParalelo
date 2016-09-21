unit Main_u;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.Samples.Spin, Cabeleireiro_u, Cliente_u, System.SyncObjs;

type
  TForm1 = class(TForm)
    ListCadeirasClientes: TCheckListBox;
    btnIniciar: TButton;
    SpinQtdCadeiras: TSpinEdit;
    cbCadeiraCabeleireiro: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    SpinTempCorte: TSpinEdit;
    Label3: TLabel;
    Label4: TLabel;
    SpinTempDormir: TSpinEdit;
    Label5: TLabel;
    SpinTempNovoCliente: TSpinEdit;
    btnParar: TButton;
    procedure btnIniciarClick(Sender: TObject);
    procedure btnPararClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    QuantidadeCadeiras: Integer;
    vCabeleireiro: Cabeleireiro;
    vCliente: Cliente;
    CS: TCriticalSection;
  public
    { Public declarations }
    procedure BloquearBotões(Const ABloqueia: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BloquearBotões(const ABloqueia: Boolean);
begin
  SpinQtdCadeiras.Enabled := not ABloqueia;
  SpinTempCorte.Enabled := not ABloqueia;
  SpinTempDormir.Enabled := not ABloqueia;
  SpinTempNovoCliente.Enabled := not ABloqueia;
end;

procedure TForm1.btnIniciarClick(Sender: TObject);
var
  i: Integer;
begin
  BloquearBotões(True);
  cbCadeiraCabeleireiro.Checked := False;
  ListCadeirasClientes.Clear;

  QuantidadeCadeiras := SpinQtdCadeiras.Value;

  for i := 0 to QuantidadeCadeiras - 1 do
  begin
    ListCadeirasClientes.AddItem('-1', ListCadeirasClientes);
    ListCadeirasClientes.Checked[i] := False;
  end;

  vCabeleireiro := Cabeleireiro.Create(True, True, ListCadeirasClientes, QuantidadeCadeiras, cbCadeiraCabeleireiro, CS);
  vCabeleireiro.TempoParaCorte := SpinTempCorte.Value;
  vCabeleireiro.TempoParaDormir := SpinTempDormir.Value;

  vCliente := Cliente.Create(True, True, ListCadeirasClientes, QuantidadeCadeiras, cbCadeiraCabeleireiro, CS);
  vCliente.TempoParaNovoCliente := SpinTempNovoCliente.Value;

  vCabeleireiro.Resume;
  vCliente.Resume;
end;

procedure TForm1.btnPararClick(Sender: TObject);
begin
  try
    vCabeleireiro.ProgramaExecutando := False;
    vCabeleireiro.ProgramaExecutando := False;
  finally
    BloquearBotões(False);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CS := TCriticalSection.Create;
end;

end.
