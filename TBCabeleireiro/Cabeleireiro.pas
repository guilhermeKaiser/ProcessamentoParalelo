unit Cabeleireiro;

interface

uses
  System.Classes;

type
  Cabeleireiro = class(TThread)
  private
    { Private declarations }
    ProgramaExecutando: Boolean;
    FilaClientes: array of array of integer;
    QuantidadeCadeiras: Integer;

    function ExisteClienteEsperando: Boolean;
    procedure Dormir;
    function ProximoCliente: Integer;

  protected
    procedure Execute; override;
  end;

implementation

{
  Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure Cabeleireiro.UpdateCaption;
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

{ Cabeleireiro }

function Cabeleireiro.ProximoCliente: Integer;
var
  i, vMenor: Integer;
begin
  vMenor := FilaClientes[1,0];
  for i := 0 to QuantidadeCadeiras do
  begin
    if vMenor > FilaClientes[1,i] then
      vMenor := FilaClientes[1,i];
  end;
end;

procedure Cabeleireiro.Dormir;
begin
  Sleep(4000);
end;

procedure Cabeleireiro.Execute;
begin
  while ProgramaExecutando do
  begin
    if ExisteClienteEsperando then
    begin

    end
    else
    begin
      Dormir;
    end;

  end;
end;

function Cabeleireiro.ExisteClienteEsperando: Boolean;
begin
//
end;

end.
