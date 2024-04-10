unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, semin64.protobuf;

type
  TForm1 = class(TForm)
    btnSave: TButton;
    btnLoad: TButton;
    mmContents: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
begin
  // Здесь мы загрузим ранее записанный бинарный файл, десериализуем его и выведем
  // данные в mmContents
end;

procedure TForm1.btnSaveClick(Sender: TObject);
begin
  // Здесь мы заполним данными в соответствии с определенным ранее типом
  // Protocol buffer и сериализуем данные в бинарный файл
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Здесь мы определим наш тип Protocol buffer
end;

end.
