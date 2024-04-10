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
    FShirtType: TsanPBMessageType;
    FColorType: TsanPBEnumType;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  ShirtMsg: TsanPBMessage;
  Price: single;
  ColorEnumNum: integer;
  ColorEnumName: string;
begin

  mmContents.Clear;

  ShirtMsg:= FShirtType.CreateInstance;

  try

    ShirtMsg.LoadFromFile('shirt.bin');

    Price:= ShirtMsg.FieldByName('price').GetValueAsFloat;
    ColorEnumNum:= ShirtMsg.FieldByName('color').GetValueAsInt32;
    ColorEnumName:= ShirtMsg.FieldByName('color').GetValueAsString;

    mmContents.Lines.Add('price: ' + FloatToStr(price));
    mmContents.Lines.Add('color: ' + IntToStr(ColorEnumNum));
    mmContents.Lines.Add('color: ' + ColorEnumName);

  finally
    ShirtMsg.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  ShirtMsg: TsanPBMessage;
begin

  ShirtMsg:= FShirtType.CreateInstance;

  try

    ShirtMsg.FieldByName('price').AppendValueAsFloat(10.5);
    ShirtMsg.FieldByName('color').AppendValueAsString('COLOR_RED');
    ShirtMsg.SaveToFile('shirt.bin');

    ShowMessage('Ok');

  finally
    ShirtMsg.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

//  enum Color {
//  	COLOR_WHITE = 0;
//  	COLOR_RED   = 1;
//  	COLOR_GREEN = 2;
//  }

//  message Shirt {
//  	optional float price = 1;
//  	optional Color color = 2;
//  }

  FColorType:= TsanPBEnumType.Create(nil, 'Color');
  FColorType.AddEnumItem(0, 'COLOR_WHITE');
  FColorType.AddEnumItem(1, 'COLOR_RED');
  FColorType.AddEnumItem(2, 'COLOR_GREEN');

  FShirtType:= TsanPBMessageType.Create(nil, 'Shirt');
  FShirtType.AddFieldDef(ftoOptional, ftFloat, nil, 'price', 1);
  FShirtType.AddFieldDef(ftoOptional, ftEnum, FColorType, 'color', 2);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FShirtType.Free;
  FColorType.Free;
end;

end.
