unit Cliente_u;

interface

uses
  System.Classes, Vcl.CheckLst, System.SysUtils, Vcl.StdCtrls;

type
  Cliente = class(TThread)
  private
    { Private declarations }
    ProgramaExecutando: Boolean;
    FilaClientes: TCheckListBox;
    QuantidadeCadeiras: Integer;
    CadeiraCabeleireiro: TCheckBox;

    procedure BuscaReservaCadeira (Const APrioridadeAtendimento: Integer);
  protected
    procedure Execute; override;
  public
  constructor Create(const ACreateSuspended: Boolean; const AProgramaExecutando: boolean;
                       const AFilaClientes: TCheckListBox; const AQuantidadeCadeiras: Integer;
                       const ACadeiraCabeleireiro: TCheckBox);
  end;

implementation

{
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure Cliente.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end;

    or

    Synchronize(
      procedure
      begin
        Form1.Caption := 'Updated in thread via an anonymous method'
      end
      )
    );

  where an anonymous method is passed.

  Similarly, the developer can call the Queue method with similar parameters as
  above, instead passing another TThread class as the first parameter, putting
  the calling thread in a queue with the other thread.

}

{ Cliente }

procedure Cliente.BuscaReservaCadeira(const APrioridadeAtendimento: Integer);
var
  i, vCadeiraVazia: Integer;
begin
  if not CadeiraCabeleireiro.Checked then
  begin
    CadeiraCabeleireiro.Checked := True;
  end
  else
  begin
    vCadeiraVazia := -1;
    for i := 0 to QuantidadeCadeiras - 1 do
    begin
      if not FilaClientes.Checked[i] then
      begin
        FilaClientes.Checked[i] := True;
        FilaClientes.Items[i] := IntToStr(APrioridadeAtendimento);
        break;
      end;
    end;
  end;
end;

constructor Cliente.Create(const ACreateSuspended, AProgramaExecutando: boolean;
  const AFilaClientes: TCheckListBox; const AQuantidadeCadeiras: Integer;
  const ACadeiraCabeleireiro: TCheckBox);
begin
  Self.ProgramaExecutando := AProgramaExecutando;
  Self.FilaClientes := AFilaClientes;
  Self.QuantidadeCadeiras := AQuantidadeCadeiras;
  Self.CadeiraCabeleireiro := ACadeiraCabeleireiro;
  inherited Create(ACreateSuspended);
end;

procedure Cliente.Execute;
var
  vPrioridade, vCadeiraVazia: Integer;
begin
  vPrioridade := 1;
  while ProgramaExecutando do
  begin
    if (Random(1) + 1) = 1 then
    begin
      Inc(vPrioridade);
      BuscaReservaCadeira(vPrioridade);
    end;
    Sleep(1000);
  end;
end;

end.
