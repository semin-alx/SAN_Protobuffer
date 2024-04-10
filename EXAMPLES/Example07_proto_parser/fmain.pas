unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, semin64.protobuf, proto;

type
  TForm1 = class(TForm)
    btnSave: TButton;
    btnLoad: TButton;
    mmContents: TMemo;
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    procedure LoadFirmList(FirmList: TsanPBMessage);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  FirmList: TsanPBMessage;
begin

  FirmList:= CreateProtoInstance('FirmList');

  try

    FirmList.LoadFromFile('firm_list.bin');
    FirmList.DbgMessageToStrings(mmContents.Lines);
  
  finally
    FirmList.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  FirmList: TsanPBMessage;
begin

  FirmList:= CreateProtoInstance('FirmList');

  try

    LoadFirmList(FirmList);
    FirmList.SaveToFile('firm_list.bin');
  
  finally
    FirmList.Free;
  end;

end;

procedure TForm1.LoadFirmList(FirmList: TsanPBMessage);
var
  Firm, Address: TsanPBMessage;
begin

  Firm:= FirmList.MessageByName('firms');
  
  Firm.Append;
  Firm.FieldByName('frm_name').AppendValueAsString('Olimp');
  Firm.FieldByName('frm_type').AppendValueAsString('PREMIUM_BUYER');

  Address:= Firm.MessageByName('address');
  
  Address.Append;
  Address.FieldByName('zip_code').AppendValueAsString('123456');
  Address.FieldByName('city').AppendValueAsString('Moscow');
  Address.FieldByName('street').AppendValueAsString('Ryasansky pr-t');
  Address.FieldByName('building').AppendValueAsString('55/1');

  Firm.Append;
  Firm.FieldByName('frm_name').AppendValueAsString('Chamomile');
  Firm.FieldByName('frm_type').AppendValueAsString('BUYER');
  Address.Append;
  Address.FieldByName('zip_code').AppendValueAsString('142184');
  Address.FieldByName('region').AppendValueAsString('MO');
  Address.FieldByName('city').AppendValueAsString('Klimovsk');
  Address.FieldByName('street').AppendValueAsString('Prospect oktyabrya');
  Address.FieldByName('building').AppendValueAsString('9');
  
end;

end.
