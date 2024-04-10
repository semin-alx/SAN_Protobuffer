program example07;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {Form1},
  proto in 'proto.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
