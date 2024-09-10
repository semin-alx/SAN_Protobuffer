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
    FSampleMessage: TsanPBMessageType;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  SampleMessageObj: TsanPBMessage;
begin

  SampleMessageObj:= FSampleMessage.CreateInstance;

  try
    SampleMessageObj.LoadFromFile('SampleMessage.bin');
    SampleMessageObj.DbgMessageToStrings(mmContents.Lines);
  finally
    SampleMessageObj.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  SampleMessageObj: TsanPBMessage;
begin

  SampleMessageObj:= FSampleMessage.CreateInstance;

  try

    SampleMessageObj.FieldByName('id').AppendValue(100);
    SampleMessageObj.FieldByName('name').AppendValue('test');  // Field id will be cleared automatically

    SampleMessageObj.SaveToFile('SampleMessage.bin');

    ShowMessage('OK');

  finally
    SampleMessageObj.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FieldNameIndex: integer;
  FieldIdIndex: integer;
begin

//  message SampleMessage {
//    oneof test_oneof {
//      int32 id = 1;
//      string name = 2;
//    }
//  }

  FSampleMessage:= TsanPBMessageType.Create(nil, 'SampleMessage');

  FieldIdIndex:= FSampleMessage.AddFieldDef(ftoOptional, ftInt32, nil, 'id', 1);
  FieldNameIndex:= FSampleMessage.AddFieldDef(ftoOptional, ftString,  nil, 'name', 2);

  FSampleMessage.FieldDef[FieldNameIndex].OneOfName:= 'test_oneof';
  FSampleMessage.FieldDef[FieldIdIndex].OneOfName:= 'test_oneof';

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSampleMessage.Free;
end;

end.
