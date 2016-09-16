program CabeleireiroDorminhoco;

uses
  Vcl.Forms,
  Main_u in 'Main_u.pas' {Form1},
  Cabeleireiro_u in 'Cabeleireiro_u.pas',
  Cliente_u in 'Cliente_u.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
