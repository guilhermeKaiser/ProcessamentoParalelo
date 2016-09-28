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
    procedure BloquearBotoes(Const ABloqueia: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BloquearBotoes(const ABloqueia: Boolean);
begin
  SpinQtdCadeiras.Enabled := not ABloqueia;
  SpinTempCorte.Enabled := not ABloqueia;
  SpinTempDormir.Enabled := not ABloqueia;
  SpinTempNovoCliente.Enabled := not ABloqueia;
  btnIniciar.Enabled := not ABloqueia;
  btnParar.Enabled := ABloqueia;
end;

procedure TForm1.btnIniciarClick(Sender: TObject);
var
  i: Integer;
begin
  BloquearBotoes(True);
  cbCadeiraCabeleireiro.Checked := False;
  cbCadeiraCabeleireiro.Caption := 'Cadeira Livre';
  ListCadeirasClientes.Clear;

  QuantidadeCadeiras := SpinQtdCadeiras.Value;
  for i := 0 to QuantidadeCadeiras - 1 do
  begin
    ListCadeirasClientes.AddItem('0', ListCadeirasClientes);
    ListCadeirasClientes.Checked[i] := False;
  end;

  vCabeleireiro := Cabeleireiro.Create(True, True, ListCadeirasClientes,
                                       QuantidadeCadeiras, cbCadeiraCabeleireiro, CS);
  vCabeleireiro.TempoParaCorte := SpinTempCorte.Value;
  vCabeleireiro.TempoParaDormir := SpinTempDormir.Value;

  vCliente := Cliente.Create(True, True, ListCadeirasClientes,
                             QuantidadeCadeiras, cbCadeiraCabeleireiro, CS);
  vCliente.TempoParaNovoCliente := SpinTempNovoCliente.Value;
  vCliente.Prioridade := 1;

  vCabeleireiro.ProgramaExecutando := True;
  vCabeleireiro.ProgramaExecutando := True;
  vCliente.Start;
  vCabeleireiro.Start;
end;

procedure TForm1.btnPararClick(Sender: TObject);
begin
  try
    vCabeleireiro.ProgramaExecutando := False;
    vCabeleireiro.Terminate;
    vCliente.ProgramaExecutando := False;
    vCliente.Terminate;
  finally
    BloquearBotoes(False);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CS := TCriticalSection.Create;
end;

end.
