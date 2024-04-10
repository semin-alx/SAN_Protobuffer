unit proto;

{=============================================================================
  This file was generated automatically by the ProtoBufParser for Delphi v.1.0
  Date: 26.03.2024 22:53:15

  Files: 
        C:\MyProjects\EXAMPLES\Examples07_parser\proto\address.proto
        C:\MyProjects\EXAMPLES\Examples07_parser\proto\firm.proto
        C:\MyProjects\EXAMPLES\Examples07_parser\proto\firm_list.proto

  Available types: 
        Address
        Firm
        FirmList

 =============================================================================}

interface

uses System.Classes, System.Generics.Collections, semin64.protobuf;

// Creating a TsanPBMessage object by full name
// Don't forget to call Free after using it
function CreateProtoInstance(ProtoName: string): TsanPBMessage;

implementation

const

  PROTO_MESSAGE_NAMES: array[0..2] of string = (
    'Address',
    'Firm',
    'FirmList'
  );

  PROTO_ENUM_NAMES: array[0..0] of string = (
    'FirmType'
  );

var
  ProtoTypeList: TList<TsanPBCustomType>;

function GetProtoType(ProtoTypeName: string): TsanPBCustomType;
var
  ProtoType: TsanPBCustomType;
begin
  Result:= nil;
  for ProtoType in ProtoTypeList do begin
    if ProtoType.Name = ProtoTypeName then begin
      Result:= ProtoType;
      break;
    end;
  end;
end;

function CreateProtoInstance(ProtoName: string): TsanPBMessage;
var
  ProtoType: TsanPBCustomType;
begin
  ProtoType:= GetProtoType(ProtoName);
  if Assigned(ProtoType) and (ProtoType is TsanPBMessageType) then begin
    Result:= TsanPBMessageType(ProtoType).CreateInstance;
  end else begin
    Result:= nil;
  end;
end;

function GetMapType(KeyType: TsanPBFieldType; ValueType: TsanPBFieldType;
  ValueCustomType: TsanPBCustomType): TsanPBMessageType;
begin
  Result:= TsanPBMessageType.Create(nil, '');
  Result.AddFieldDef(ftoRequired, KeyType, nil, 'key', 1);
  Result.AddFieldDef(ftoRequired, ValueType, ValueCustomType, 'value', 2);
  ProtoTypeList.Add(Result);
end;

// C:\MyProjects\EXAMPLES\Examples07_parser\proto\address.proto
// Address
procedure DefineMessageFields_1;
begin
  with TsanPBMessageType(GetProtoType('Address')) do begin
    AddFieldDef(ftoOptional, ftString, nil, 'zip_code', 1);
    AddFieldDef(ftoOptional, ftString, nil, 'region', 2);
    AddFieldDef(ftoOptional, ftString, nil, 'city', 3);
    AddFieldDef(ftoOptional, ftString, nil, 'street', 4);
    AddFieldDef(ftoOptional, ftString, nil, 'building', 5);
  end;
end;

// C:\MyProjects\EXAMPLES\Examples07_parser\proto\firm.proto
// Firm
procedure DefineMessageFields_2;
begin
  with TsanPBMessageType(GetProtoType('Firm')) do begin
    AddFieldDef(ftoRequired, ftString, nil, 'frm_name', 1);
    AddFieldDef(ftoOptional, ftMessage, GetProtoType('Address'), 'address', 2);
    AddFieldDef(ftoOptional, ftEnum, GetProtoType('FirmType'), 'frm_type', 3);
  end;
end;

// C:\MyProjects\EXAMPLES\Examples07_parser\proto\firm_list.proto
// FirmList
procedure DefineMessageFields_3;
begin
  with TsanPBMessageType(GetProtoType('FirmList')) do begin
    AddFieldDef(ftoRepeated, ftMessage, GetProtoType('Firm'), 'firms', 1);
  end;
end;

// C:\MyProjects\EXAMPLES\Examples07_parser\proto\firm.proto
// FirmType
procedure DefineEnumItems_1;
begin
  with TsanPBEnumType(GetProtoType('FirmType')) do begin
    AddEnumItem(0, 'SELLER');
    AddEnumItem(1, 'BUYER');
    AddEnumItem(2, 'PREMIUM_BUYER');
  end;
end;

procedure CreateTypes;
var
  I: integer;
begin

  ProtoTypeList:= TList<TsanPBCustomType>.Create;

  for I:= 1 to Length(PROTO_MESSAGE_NAMES) do begin
    ProtoTypeList.Add(TsanPBMessageType.Create(nil, PROTO_MESSAGE_NAMES[I-1]));
  end;

  for I:= 1 to Length(PROTO_ENUM_NAMES) do begin
    ProtoTypeList.Add(TsanPBEnumType.Create(nil, PROTO_ENUM_NAMES[I-1]));
  end;

  DefineMessageFields_1; //Address
  DefineMessageFields_2; //Firm
  DefineMessageFields_3; //FirmList

  DefineEnumItems_1; //FirmType

end;

procedure FreeTypes;
var
  ProtoType: TsanPBCustomType;
begin
  for ProtoType in ProtoTypeList do ProtoType.Free;
  ProtoTypeList.Free;
end;

initialization
  CreateTypes;

finalization
  FreeTypes;

end.
