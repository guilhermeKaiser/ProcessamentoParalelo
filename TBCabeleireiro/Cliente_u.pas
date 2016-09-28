unit Cliente_u;

interface

uses
  System.Classes, Vcl.CheckLst, System.SysUtils, Vcl.StdCtrls, System.SyncObjs, Vcl.Forms;

type
  Cliente = class(TThread)
  private
    { Private declarations }
    FilaClientes: TCheckListBox;
    QuantidadeCadeiras: Integer;
    CadeiraCabeleireiro: TCheckBox;
    SecaoCritica: TCriticalSection;
    FTempoParaNovoCliente: Integer;
    FProgramaExecutando: Boolean;
    FPrioridade: Integer;
    procedure setProgramaExecutando(const Value: Boolean);
    procedure setTempoParaNovoCliente(const Value: Integer);
    procedure setPrioridade(const Value: Integer);

    procedure AtualizaStatus;
    procedure BuscaReservaCadeira(Const APrioridadeAtendimento: Integer; Const AVaiDiretoFilaEspera: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(const ACreateSuspended: Boolean; const AProgramaExecutando: boolean;
                       Var AFilaClientes: TCheckListBox; const AQuantidadeCadeiras: Integer;
                       Var ACadeiraCabeleireiro: TCheckBox; Var ASecaoCritica: TCriticalSection);
    property TempoParaNovoCliente: Integer read FTempoParaNovoCliente write setTempoParaNovoCliente;
    property ProgramaExecutando: Boolean read FProgramaExecutando write setProgramaExecutando;
    property Prioridade: Integer read FPrioridade write setPrioridade;
  end;

implementation

{ Cliente }

procedure Cliente.AtualizaStatus;
begin
  FilaClientes.Update;
  CadeiraCabeleireiro.Update;
end;

procedure Cliente.BuscaReservaCadeira(const APrioridadeAtendimento: Integer; Const AVaiDiretoFilaEspera: Boolean);
var
  i, vCadeiraVazia: Integer;
  vAlguemJaEsperando: Boolean;
begin
  Application.ProcessMessages;
  vAlguemJaEsperando := False;

  if not AVaiDiretoFilaEspera then
    for i := 0 to QuantidadeCadeiras - 1 do
    begin
      if FilaClientes.Checked[i] then
      begin
        vAlguemJaEsperando := True;
        Break;
      end;
    end;

  if (vAlguemJaEsperando) or (CadeiraCabeleireiro.Checked) or (AVaiDiretoFilaEspera) then
  begin
    vCadeiraVazia := -1;
    for i := 0 to QuantidadeCadeiras - 1 do
    begin
      if not FilaClientes.Checked[i] then
      begin
        FilaClientes.Checked[i] := True;
        FilaClientes.Items[i] := IntToStr(APrioridadeAtendimento);
        AtualizaStatus;
        break;
      end;
    end;
  end
  else
  begin
    SecaoCritica.Acquire;
    try
      CadeiraCabeleireiro.Checked := True;
      CadeiraCabeleireiro.Caption := 'Ocupada por Cliente';
      Synchronize(AtualizaStatus);
    finally
      SecaoCritica.Release;
    end;

    //Caso o caption não seja o que foi tentado setar a cima, significa que alguém estava usando a seção crítica
    //e por isso não foi possível usá-la, com isso chama-se  a função BuscaReservaCadeira que agora irá mandar
    // o cliente direto para a Fila de espera
    if CadeiraCabeleireiro.Caption <> 'Ocupada por Cliente' then
    begin
      BuscaReservaCadeira(APrioridadeAtendimento, True);
    end;
  end;
end;

constructor Cliente.Create(const ACreateSuspended, AProgramaExecutando: boolean;
  Var AFilaClientes: TCheckListBox; const AQuantidadeCadeiras: Integer;
  Var ACadeiraCabeleireiro: TCheckBox; Var ASecaoCritica: TCriticalSection);
begin
  Self.ProgramaExecutando := AProgramaExecutando;
  Self.FilaClientes := AFilaClientes;
  Self.QuantidadeCadeiras := AQuantidadeCadeiras;
  Self.CadeiraCabeleireiro := ACadeiraCabeleireiro;
  Self.SecaoCritica := ASecaoCritica;
  inherited Create(ACreateSuspended);
end;

procedure Cliente.Execute;
var
  vCadeiraVazia: Integer;
begin
  while ProgramaExecutando do
  begin
    AtualizaStatus;
    if (Random(1) + 1) = 1 then
    begin
      Self.FPrioridade := Prioridade + 1;
      BuscaReservaCadeira(Prioridade, False);
    end;
    AtualizaStatus;
    Sleep(TempoParaNovoCliente * 1000);
  end;
end;

procedure Cliente.setPrioridade(const Value: Integer);
begin
  FPrioridade := Value;
end;

procedure Cliente.setProgramaExecutando(const Value: Boolean);
begin
  FProgramaExecutando := Value;
end;

procedure Cliente.setTempoParaNovoCliente(const Value: Integer);
begin
  FTempoParaNovoCliente := Value;
end;

end.
