unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, semin64.protobuf;

type
  TForm1 = class(TForm)
    btnSave: TButton;
    btnLoad: TButton;
    mmContents: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FMyMessageType: TsanPBMessageType;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  MyMsg: TsanPBMessage;
  PerPage: integer;
  SamplesField: TsanPBField;
  Value: integer;
  I: integer;
begin

  // Создадим объект, который будет работать с данными
  // тип которых описан в FTeamType
  MyMsg:= FMyMessageType.CreateInstance;

  try

    mmContents.Clear;

    MyMsg.LoadFromFile('data.bin');

    PerPage:= MyMsg.FieldByName('per_page').GetValueAsInt32;
    mmContents.Lines.Add('per_page: ' + IntToStr(PerPage));

    SamplesField:= MyMsg.FieldByName('samples');

    for I := 1 to SamplesField.RecordCount do begin
      Value:= SamplesField.GetValueAsInt32(I-1);
      mmContents.Lines.Add('samples: ' + IntToStr(Value));
    end;

  finally
    MyMsg.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  MyMsg: TsanPBMessage;
begin

  MyMsg:= FMyMessageType.CreateInstance;

  try

    // Специально не заполняем, чтобы при чтении получить Default value
    //MyMsg.FieldByName('per_page').AppendValueAsInt32(100);

    MyMsg.FieldByName('samples').AppendValueAsInt32(100);
    MyMsg.FieldByName('samples').AppendValueAsInt32(200);
    MyMsg.FieldByName('samples').AppendValueAsInt32(300);

    // Сериализуем в бинарный файл data.bin
    MyMsg.SaveToFile('data.bin');

    ShowMessage('OK');

  finally
    MyMsg.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  // message MyMessage {
  //   optional int32 per_page = 1 [default = 10];
  //   repeated int32 samples = 2 [packed = true];
  // }

  FMyMessageType:= TsanPBMessageType.Create(nil, 'MyMessage');

  FMyMessageType.AddFieldDef(ftoOptional, ftInt32,  nil, 'per_page', 1);
  FMyMessageType.FieldDefByName('per_page').DefaultValue:= 10;

  FMyMessageType.AddFieldDef(ftoRepeated, ftInt32, nil, 'samples', 2);
  FMyMessageType.FieldDefByName('samples').DataPacked:= True;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMyMessageType.Free;
end;

end.
