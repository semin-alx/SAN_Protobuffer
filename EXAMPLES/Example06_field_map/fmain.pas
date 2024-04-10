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
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    FTest6Type: TsanPBMessageType;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  Test6Msg: TsanPBMessage;
  EntryMsg: TsanPBMessage;
  I: integer;
begin

  mmContents.Clear;

  Test6Msg:= FTest6Type.CreateInstance;
  try

    Test6Msg.LoadFromFile('test6.bin');

    EntryMsg:= Test6Msg.MessageByName('g');

    for I := 1 to EntryMsg.RecordCount do begin
      EntryMsg.RecordIndex:= I-1;
      mmContents.Lines.Add('key: ' + EntryMsg.FieldByName('key').GetValueAsString);
      mmContents.Lines.Add('value: ' + EntryMsg.FieldByName('value').GetValueAsString)
    end;

  finally
    Test6Msg.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  Test6Msg: TsanPBMessage;
  EntryMsg: TsanPBMessage;
begin

  Test6Msg:= FTest6Type.CreateInstance;
  try

    EntryMsg:= Test6Msg.MessageByName('g');

    EntryMsg.Append;
    EntryMsg.FieldByName('key').AppendValueAsString('A001');
    EntryMsg.FieldByName('value').AppendValueAsInt32(100);

    EntryMsg.Append;
    EntryMsg.FieldByName('key').AppendValueAsString('A002');
    EntryMsg.FieldByName('value').AppendValueAsInt32(200);

    EntryMsg.Append;
    EntryMsg.FieldByName('key').AppendValueAsString('A003');
    EntryMsg.FieldByName('value').AppendValueAsInt32(300);

    Test6Msg.SaveToFile('test6.bin');

    ShowMessage('Ok');

  finally
    Test6Msg.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  EntryType: TsanPBMessageType;
begin

//  message Test6 {
//    map<string, int32> g = 7;
//  }

//  message Test6 {
//    message g_Entry {
//      optional string key = 1;
//      optional int32 value = 2;
//    }
//    repeated g_Entry g = 7;
//  }

  FTest6Type:= TsanPBMessageType.Create(nil, 'Test6');
  EntryType:= TsanPBMessageType.Create(FTest6Type, 'g_Entry');

  EntryType.AddFieldDef(ftoOptional, ftString, nil, 'key', 1);
  EntryType.AddFieldDef(ftoOptional, ftInt32, nil, 'value', 2);

  FTest6Type.AddFieldDef(ftoRepeated, ftMessage, EntryType, 'g', 7);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTest6Type.Free;
end;

end.
