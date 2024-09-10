unit semin64.protobuf;

{*****************************************************************}
{                                                                 }
{                          Semin64 library                        }
{                                                                 }
{                   Copyright(c) 2024 Semin Alexey                }
{                        All rights reserved                      }
{                                                                 }
{*****************************************************************}

interface

uses System.Classes, System.SysUtils, System.Generics.Collections,
  System.Variants, semin64.memory;

type

  // При добавлении нового типа, нужно учитывать порядок перечисления в объявлении,
  // простые типы должны быть до ftMessage, а типы, которые реализуются в наследниках
  // от TsanPBCustomType должны быть после ftMessage. Особенность реализации
  // доп. проверки в TsanPBMessageType.AddFieldDef
  TsanPBFieldType = (// Простые типы
                     ftInt32, ftSint32, ftFixed32, ftSfixed32, ftUInt32,
                     ftInt64, ftSint64, ftFixed64, ftSfixed64, ftUInt64,
                     ftFloat, ftDouble, ftBoolean, ftString, ftBytes,
                     ftMessage,
                     // Типы от TsanPBCustomType
                     ftEnum);

  TsanPBWireType = (wtVarint = 0,
                    wt64bit = 1,
                    wtLengthDelimited = 2,
                    wtStartGroup = 3,
                    wtEndGroup = 4,
                    wt32bit = 5);

  TsanPBFieldTypeOption = (ftoRequired, ftoOptional, ftoRepeated);

  TsanPBFieldDef = class;
  TsanPBMessage = class;

  TsanPBEnumItem = record
    Value: integer;
    Descr: string;
  end;

  // Базовый класс для всех классов данной библиотеки
  // позволяет автоматически освобождать все подчиненные
  // объекты. Подобный функционал есть у TComponent, но тот тянет
  // много лишнего функционала
  TsanPBObject = class(TObject)
  private
    FOwner: TsanPBObject;
    FChildren: TList;
    procedure RegisterChild(Child: TsanPBObject);
    procedure UnRegisterChild(Child: TsanPBObject);
    procedure FreeChildren;
  protected
    constructor Create(OwnerA: TsanPBObject);
  public
    destructor Destroy; override;
    property Owner: TsanPBObject read FOwner;
  end;

  // Данный класс используется для создания классов описывающие proto-типы
  // содержащие набор каких-нибудь подтипов (Например: TsanPBMessageType)
  // или других элементов (Например: TsanPBEnumType)
  TsanPBCustomType = class(TsanPBObject)
  private
    FName: string;
    FFieldType: TsanPBFieldType;
  protected
    constructor Create(OwnerA: TsanPBCustomType; FieldType: TsanPBFieldType; TypeNameA: string);
  public
    property Name: string read FName;
    property FieldType: TsanPBFieldType read FFieldType;
  end;

  // Данный класс реализует описание proto-типа Enum
  TsanPBEnumType = class(TsanPBCustomType)
  private
    FEnumList: TList<TsanPBEnumItem>;
    function IndexOf(EnumValue: integer): integer; overload;
    function IndexOf(EnumDescr: string): integer;  overload;
  public
    constructor Create(OwnerA: TsanPBCustomType; TypeNameA: string);
    destructor Destroy; override;
    procedure AddEnumItem(Value: integer; Descr: string);
    function EnumToString(Value: integer): string;
    function StringToEnum(Value: string): integer;
  end;

  // Данный класс реализует описание proto-типа Message
  TsanPBMessageType = class(TsanPBCustomType)
  private
    FFieldDefs: TList<TsanPBFieldDef>;
    function GetFieldDefsCount: integer;
    function GetFieldDef(Index: integer): TsanPBFieldDef;
  public

    constructor Create(OwnerA: TsanPBCustomType; TypeNameA: string);
    destructor Destroy; override;

    function AddFieldDef(Option: TsanPBFieldTypeOption;
                         FieldType:  TsanPBFieldType;
                         CustomType: TsanPBCustomType;
                         FieldName: string;
                         StoreIndex: integer): integer;

    // Функция создает объект для работы с данными на основе описанного типа
    function CreateInstance(OwnerA: TsanPBMessage = nil): TsanPBMessage;

    function FieldDefByName(FieldName: string): TsanPBFieldDef;

    property FieldDefsCount: integer read GetFieldDefsCount;
    property FieldDef[Index: integer]: TsanPBFieldDef read GetFieldDef;

  end;

  // Базовый класс, реализующий описание строки поля
  // Например: repeated string a = 1;
  TsanPBFieldDef = class(TsanPBObject)
  private
    FFieldName: string;
    FStoreIndex: integer;
    FOption: TsanPBFieldTypeOption;
    FFieldType: TsanPBFieldType;
    FCustomType: TsanPBCustomType;
    FDataPacket: Boolean;
    FDefaultValue: Variant;
    FOneOfName: string;
    function GetFieldTypeName: string;
  public
    constructor Create(AOwner: TsanPBMessageType);
    property FieldType: TsanPBFieldType read FFieldType write FFieldType;
    property CustomType: TsanPBCustomType read FCustomType write FCustomType;
    property FieldName: string read FFieldName write FFieldName;
    property StoreIndex: integer read FStoreIndex write FStoreIndex;
    property Option: TsanPBFieldTypeOption read FOption write FOption;
    property DataPacked: Boolean read FDataPacket write FDataPacket;
    property DefaultValue: Variant read FDefaultValue write FDefaultValue;
    property FieldTypeName: string read GetFieldTypeName;
    property OneOfName: string read FOneOfName write FOneOfName;
  end;

  PsanPBData = ^TsanPBData;
  PsanPBContext = ^TsanPBContext;

  // Данный класс является базовым для создания класса, который будет работать
  // с данными по указанному proto-типу
  // Данные любого proto-типа можно представить в виде списка значений
  // Например: string FileName = 2  - это список с одним значением
  //           repeated CloudSignFile Files - это пустой список или список с любым
  //           кол-вом значений
  // Список значений реализуется через связанный список на основе TsanPBContext
  // Список из TsanPBContext - это цепочка полей, а от каждого поля идет
  // цепочка данных (TsanPBData)
  // при написании наследника, необходимо реализовать:
  //   ReadData  - метод чтения бинарных данных из Stream
  //   WriteData - метод записи бинарных данных в Stream
  //   Реализовать соотвествующие типу методы: AppendValue... и GetValue...
  //   Зарегистрировать класс методом RegistryFieldClass в секции initialization
  TsanPBField = class(TsanPBObject)
  private

    // Указатель на область данных (связанный список на основе TsanPBContext)
    FContextPtr: PsanPBContext;

    // Описание типа поля
    FFieldDef: TsanPBFieldDef;

    // Как только мы привяжем область данных (Связанный список из TsanPBContext)
    // с данным объектом методом SetContext, то FValues будет содержать
    // указатели PsanPBData на каждое значение
    FValues: TList;

    // Область данных выделяется с помощью MemoryManager, но MemoryManager
    // создается только один для "корневого" объекта TsanPBField
    // Тот, кто является "хозяином" MemoryManager, тот его создает и освобождает
    FIsOwnerMemoryManager: Boolean;
    FMemoryManager: TsanStackMemoryManager;

    FTempStream: TMemoryStream;

    procedure CheckRecordIndex(Index: integer);
    procedure ClearOneOfFields;
    procedure SetDefaultOneOfFields;

    function IsOneOf: Boolean;

  protected
    function AllocPBData: PsanPBData;
    function GetPBData(RecordIndex: integer): PsanPBData;
    procedure SetContext(pContext: PsanPBContext);

    function ReadVarint(Stream: TStream): UInt64;
    procedure WriteVarint(Stream: TStream; Value: UInt64);
    function DecodeZigzag(Value: Uint64): Int64;
    function EncodeZigzag(Value: Int64): UInt64;
    procedure SeekStream(Stream: TStream; Count: Longint);
    procedure ReadStream(Stream: TStream; var Buffer; Count: Longint);
    procedure ReadHdr(Stream: TStream; var WireType: TsanPBWireType; var StoreIndex: integer);
    procedure WriteHdr(Stream: TStream; WireType: TsanPBWireType; StoreIndex: integer);
    procedure Clear;

    function GetRecordCount: integer; virtual;
    procedure BeforeWrite; virtual;
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual; abstract;

    function InternalAppendValue(Value: Variant): integer; virtual;  abstract;
    function InternalGetValue(RecordIndex: integer = 0): Variant; virtual; abstract;
    function GetEmptyValue: Variant; virtual; abstract;

    property ContextPtr: PsanPBContext read FContextPtr write SetContext;
    property MemoryManager: TsanStackMemoryManager read FMemoryManager;
    property TempStream: TMemoryStream read FTempStream;

  public

    constructor Create(AOwner: TsanPBField; FieldDefA: TsanPBFieldDef); virtual;
    destructor Destroy; override;

    function AppendValueAsInt32(Value: integer): integer;
    function AppendValueAsUInt32(Value: cardinal): integer;
    function AppendValueAsInt64(Value: Int64): integer;
    function AppendValueAsUInt64(Value: UInt64): integer;
    function AppendValueAsFloat(Value: single): integer;
    function AppendValueAsDouble(Value: double): integer;
    function AppendValueAsBoolean(Value: Boolean): integer;
    function AppendValueAsString(Value: string): integer;
    function AppendValueAsBytes(Value: TBytes): integer;
    function AppendValueFromStream(Stream: TStream): integer;

    function GetValueAsInt32(RecordIndex: integer = 0): integer;
    function GetValueAsUInt32(RecordIndex: integer = 0): cardinal;
    function GetValueAsInt64(RecordIndex: integer = 0): Int64;
    function GetValueAsUInt64(RecordIndex: integer = 0): UInt64;
    function GetValueAsFloat(RecordIndex: integer = 0): single;
    function GetValueAsDouble(RecordIndex: integer = 0): double;
    function GetValueAsBoolean(RecordIndex: integer = 0): Boolean;
    function GetValueAsString(RecordIndex: integer = 0): string; virtual;
    function GetValueAsBytes(RecordIndex: integer = 0): TBytes;

    function AppendValue(Value: Variant): integer;
    function GetValue(RecordIndex: integer = 0): Variant;

    function IsEmpty: Boolean;
    property RecordCount: integer read GetRecordCount;
    property FieldDef: TsanPBFieldDef read FFieldDef;

  end;

  TsanPBFieldClass = class of TsanPBField;

  TsanPBFixSizeField = class(TsanPBField)
  private
    procedure WritePackedData(Stream: TStream);
    procedure WriteUnPackedData(Stream: TStream);
  protected
    procedure ReadFixSizeData(Stream: TStream); virtual; abstract;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); virtual; abstract;
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); override;
    procedure WriteData(Stream: TStream); override;
    function GetEmptyValue: Variant; override;
  end;

  TsanPBInt32Field = class(TsanPBFixSizeField)
  protected
    procedure CheckRange(Value: Int64);
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBUInt32Field = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBInt64Field = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBUInt64Field = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBFloatField = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBDoubleField = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBBooleanField = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function GetEmptyValue: Variant; override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBStringField = class(TsanPBField)
  protected
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); override;
    procedure WriteData(Stream: TStream); override;
    function GetEmptyValue: Variant; override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
  end;

  TsanPBBytesField = class(TsanPBField)
  protected
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); override;
    procedure WriteData(Stream: TStream); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
    function GetEmptyValue: Variant; override;
  public
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBData = record
    pNext: PsanPBData;
    pStorage: Pointer;
  end;

  TsanPBContext = record
    Field: TsanPBField;
    pNext:  PsanPBContext;
    pData:  PsanPBData;
  end;

  TsanPBEnumField = class(TsanPBInt32Field)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
    function InternalAppendValue(Value: Variant): integer; override;
  public
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBMessage = class(TsanPBField)
  private
    FFields: TList;
    FCreatedFields: Boolean;
    FFieldDefStub: TsanPBFieldDef;
    FMessageType: TsanPBMessageType;
    FRecordIndex: integer;
    procedure CreateFields;
    function CreateField(FieldDef: TsanPBFieldDef): TsanPBField;
    function CreateContext: PsanPBContext;
    procedure SetMsgContext(pContext: PsanPBContext);
    procedure MessageToText(Level: integer; Text: TStrings);
    function SpaceOffset(Level: integer): string;
    procedure SkipData(Stream: TStream; WireType: TsanPBWireType; Size: integer);
    function IsRoot: Boolean;
    procedure WriteRecord(Stream: TStream);
  protected
    function GetRecordCount: integer; override;
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); override;
    procedure WriteData(Stream: TStream); override;
    function InternalAppendValue(Value: Variant): integer; override;
    function InternalGetValue(RecordIndex: integer = 0): Variant; override;
    function GetEmptyValue: Variant; override;
  public

    constructor Create(AOwner: TsanPBField; FieldDefA: TsanPBFieldDef); override;
    constructor CreateByMsgType(AOwner: TsanPBField; MessageType: TsanPBMessageType);
    destructor Destroy; override;

    function FieldByName(FieldName: string): TsanPBField;         // Если поле не найдено, будет исключение
    function FieldByStoreIndex(StoreIndex: integer): TsanPBField; // Если поле по StoreIndex не найдено, будет nil
    function MessageByName(FieldName: string): TsanPBMessage;

    procedure Append;
    procedure MoveTo(Index: integer);

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);

    procedure Clear;

    procedure DbgMessageToStrings(Text: TStrings);

    property RecordIndex: integer read FRecordIndex write MoveTo;

  end;

implementation

const
  SHOW_BYTES_LIMIT = 30;

resourcestring
  ERR_CUSTOMTYPE_IS_NULL = 'The parameter CustomType is nil for %s.%s';
  ERR_INVALID_CUSTOM_TYPE = 'Not the appropriate type of TsanPBCustomType for %s.%s';
  ERR_UNKNOWN_FIELD_TYPE = 'Unknown field type';
  ERR_OWNER_NOT_IS_MESSAGEFIELD = 'AOwner object must be TsanPBMessageField or nil';
  ERR_FIELD_NOT_FOUND = 'Field %s not found';
  ERR_FIELDDEF_NOT_FOUND = 'FieldDef %s not found';
  ERR_FIELD_NOT_FOUND_SI = 'Field (StoreIndex = %d) not found';
  ERR_METHOD_ONLY_FOR_ROOT_MESSAGE = 'This method only for root message';
  ERR_METHOD_ONLY_FOR_NOT_ROOT = 'The method is not applicable for the root message';
  ERR_UNEXPECTED_END_OF_DATA = 'Unexpected end of data';
  ERR_UNSUPPORTED_WIRE_TYPE = 'Unsupported wire type: %d';
  ERR_UNSUPPERTED_WIRETYPE_FOR_FIELD = 'Unsupported wire type %d for field: %s';
  ERR_VALUE_TOO_BIG_FOR_FIELD = 'Value is to big for field %s';
  ERR_READER_IS_NOT_DONE = 'Reader for this field type is not done';
  ERR_WRITER_IS_NOT_DONE = 'Writer for this field type is not done';
  ERR_DUPLICATE_ENUM_VALUE = 'Duplicate enum value %s';
  ERR_ENUM_DESCR_NOT_FOUND = 'Enum %s not found for type %s';
  ERR_RECORD_INDEX_OUT_OF_RANGE = 'Record index out of range';
  ERR_FIELD_IS_NOT_MESSAGE = 'Field %s is not message type';

  ERR_EXPECT_INT32_VALUE = 'Expect Int32 value';
  ERR_EXPECT_UINT32_VALUE = 'Expect UInt32 value';
  ERR_EXPECT_INT64_VALUE = 'Expect Int64 value';
  ERR_EXPECT_UINT64_VALUE = 'Expect UInt64 value';
  ERR_EXPECT_SINGLE_VALUE = 'Expect Single value';
  ERR_EXPECT_DOUBLE_VALUE = 'Expect Double value';
  ERR_EXPECT_BOOLEAN_VALUE = 'Expect Boolean value';
  ERR_EXPECT_TBYTES_VALUE = 'Expect TBytes value';
  ERR_EXPECT_ENUM_INDEX_OR_NAME = 'Expect enum index or name';
  ERR_ONEOF_FIELD_WRONG_OPTION = 'A oneof field %s can be only ftoOptional';

  ERR_NO_DATA = 'No data';
  ERR_NOT_APPLICABLE = 'Not applicable';

  UNKNOWN_ENUM_VALUE = 'Unknown enum value (%d) for type %s';

type
  TsanPBFieldClassRegItem = record
    FieldClass: TsanPBFieldClass;
    WireType: TsanPBWireType;
  end;

var
  FieldClasses: array[Low(TsanPBFieldType)..High(TsanPBFieldType)] of TsanPBFieldClassRegItem;

procedure RegistryFieldClass(FieldType: TsanPBFieldType; WireType: TsanPBWireType; FieldClass: TsanPBFieldClass);
begin
  FieldClasses[FieldType].FieldClass:= FieldClass;
  FieldClasses[FieldType].WireType:= WireType;
end;

function FieldTypeToString(FieldType: TsanPBFieldType): string;
begin

  case FieldType of
    ftInt32:    Result:= 'Int32';
    ftSint32:   Result:= 'Sint32';
    ftFixed32:  Result:= 'Fixed32';
    ftSfixed32: Result:= 'Sfixed32';
    ftUInt32:   Result:= 'UInt32';
    ftInt64:    Result:= 'Int64';
    ftSint64:   Result:= 'Sint64';
    ftFixed64:  Result:= 'Fixed64';
    ftSfixed64: Result:= 'Sfixed64';
    ftUInt64:   Result:= 'UInt64';
    ftFloat:    Result:= 'Float';
    ftDouble:   Result:= 'Double';
    ftBoolean:  Result:= 'Boolean';
    ftString:   Result:= 'String';
    ftBytes:    Result:= 'Bytes';
    ftMessage:  Result:= 'Message';
    ftEnum:     Result:= 'Enum';
    else begin
      raise Exception.Create(ERR_UNKNOWN_FIELD_TYPE);
    end;
  end;

end;

{ TsanPBObject }

constructor TsanPBObject.Create(OwnerA: TsanPBObject);
begin

  FChildren:= nil;
  FOwner:= OwnerA;

  if Assigned(FOwner) then FOwner.RegisterChild(Self);

end;

destructor TsanPBObject.Destroy;
begin

  if Assigned(FChildren) then begin
    FreeChildren;
    FChildren.Free;
  end;

  if Assigned(FOwner) then FOwner.UnRegisterChild(Self);

  inherited;
end;

procedure TsanPBObject.FreeChildren;
begin
  while FChildren.Count > 0 do TsanPBObject(FChildren[0]).Free;
end;

procedure TsanPBObject.RegisterChild(Child: TsanPBObject);
begin

  if Not Assigned(FChildren) then begin
    FChildren:= TList.Create;
  end;

  FChildren.Add(Child);

end;

procedure TsanPBObject.UnRegisterChild(Child: TsanPBObject);
var
  Index: integer;
begin

  Index:= FChildren.IndexOf(Child);
  if Index = -1 then raise Exception.Create('error in architecture');

  FChildren.Delete(Index);

end;

{ TsanPBMessageType }

function TsanPBMessageType.AddFieldDef(Option: TsanPBFieldTypeOption;
  FieldType:  TsanPBFieldType; CustomType: TsanPBCustomType;
  FieldName: string; StoreIndex: integer): integer;
var
  FieldDef: TsanPBFieldDef;
begin

  if (FieldType >= ftMessage) then begin
    if Not Assigned(CustomType)
      then raise Exception.Create(Format(ERR_CUSTOMTYPE_IS_NULL, [Name, FieldName]));
  end;

  if Assigned(CustomType) then begin
    if CustomType.FieldType <> FieldType then begin
      raise Exception.Create(Format(ERR_INVALID_CUSTOM_TYPE, [Name, FieldName]));
    end;
  end;

  FieldDef:= TsanPBFieldDef.Create(Self);
  FieldDef.Option:= Option;
  FieldDef.FieldName:= FieldName;
  FieldDef.FieldType:= FieldType;
  FieldDef.CustomType:= CustomType;
  FieldDef.StoreIndex:= StoreIndex;

  Result:= FFieldDefs.Add(FieldDef);

end;

constructor TsanPBMessageType.Create(OwnerA: TsanPBCustomType; TypeNameA: string);
begin
  inherited Create(OwnerA, ftMessage, TypeNameA);
  FFieldDefs:= TList<TsanPBFieldDef>.Create;
end;

function TsanPBMessageType.CreateInstance(OwnerA: TsanPBMessage): TsanPBMessage;
begin
  Result:= TsanPBMessage.CreateByMsgType(OwnerA, Self);
end;

destructor TsanPBMessageType.Destroy;
begin
  FFieldDefs.Free;
  inherited;
end;

function TsanPBMessageType.FieldDefByName(FieldName: string): TsanPBFieldDef;
begin

  for Result in FFieldDefs do begin
    if Result.FieldName = FieldName
      then Exit;
  end;

  raise Exception.Create(Format(ERR_FIELDDEF_NOT_FOUND, [FieldName]));

end;

function TsanPBMessageType.GetFieldDefsCount: integer;
begin
  Result:= FFieldDefs.Count;
end;

function TsanPBMessageType.GetFieldDef(Index: integer): TsanPBFieldDef;
begin
  Result:= FFieldDefs.Items[Index];
end;

{ TsanPBFieldDef }

constructor TsanPBFieldDef.Create(AOwner: TsanPBMessageType);
begin
  inherited Create(AOwner);
  FFieldName:= '';
  FStoreIndex:= -1;
  FOption:= ftoOptional;
  FFieldType:= ftMessage;
  FCustomType:= nil;
  FDataPacket:= True;
  FDefaultValue:= Unassigned;
  FOneOfName:= '';
end;

{ TsanPBMessageField }

procedure TsanPBMessage.Append;
var
  pData: PsanPBData;
begin

  if IsRoot then begin
    raise Exception.Create(ERR_METHOD_ONLY_FOR_NOT_ROOT);
  end;

  pData:= AllocPBData;
  pData^.pStorage:= CreateContext;

  FRecordIndex:= RecordCount - 1;

end;

procedure TsanPBMessage.Clear;
begin

  if Assigned(Owner) then begin
    raise Exception.Create(ERR_METHOD_ONLY_FOR_ROOT_MESSAGE);
  end;

  MemoryManager.Clear;
  CreateContext;
  FRecordIndex:= -1;

end;

constructor TsanPBMessage.Create(AOwner: TsanPBField; FieldDefA: TsanPBFieldDef);
begin

  inherited Create(AOwner, FieldDefA);

  FFieldDefStub:= nil;
  FMessageType:= TsanPBMessageType(FieldDefA.CustomType);

  FFields:= TList.Create;
  FCreatedFields:= False;
  FRecordIndex:= -1;

  if IsRoot then begin
    CreateContext;
    FRecordIndex:= 0;
  end;

end;

constructor TsanPBMessage.CreateByMsgType(AOwner: TsanPBField;
  MessageType: TsanPBMessageType);
var
  FieldDefStubA: TsanPBFieldDef;
begin

  FieldDefStubA:= TsanPBFieldDef.Create(nil);
  FieldDefStubA.FieldType:= ftMessage;
  FieldDefStubA.CustomType:= MessageType;

  Create(AOwner, FieldDefStubA);

  FFieldDefStub:= FieldDefStubA;

  if Not Assigned(AOwner) then begin
    CreateContext;
  end;

end;

function TsanPBMessage.CreateContext: PsanPBContext;
var
  I: integer;
  pPrev: PsanPBContext;
  PCur:  PsanPBContext;
begin

  if Not FCreatedFields then begin
    CreateFields;
  end;

  pPrev:= nil;
  Result:= nil;

  for I := 1 to FFields.Count do begin

    PCur:= MemoryManager.GetMem(SizeOf(TsanPBContext));

    PCur^.Field:= TsanPBField(FFields.Items[I-1]);
    PCur^.pNext:= nil;
    PCur^.pData:= nil;

    if Assigned(pPrev) then begin
      pPrev^.pNext:= PCur;
    end;

    pPrev:=  PCur;

    if I = 1 then begin
      Result:= PCur;
    end;

  end;

  SetMsgContext(Result);

end;

function TsanPBMessage.CreateField(FieldDef: TsanPBFieldDef): TsanPBField;
var
  FieldClass: TsanPBFieldClass;
begin

  FieldClass:= FieldClasses[FieldDef.FFieldType].FieldClass;

  if Not Assigned(FieldClass) then begin
    raise Exception.Create(ERR_UNKNOWN_FIELD_TYPE);
  end;

  Result:= FieldClass.Create(Self, FieldDef);

end;

procedure TsanPBMessage.CreateFields;
var
  I: integer;
begin

  for I := 1 to FMessageType.GetFieldDefsCount do begin
    FFields.Add(CreateField(FMessageType.FieldDef[I-1]));
  end;

  FCreatedFields:= True;

end;

procedure TsanPBMessage.DbgMessageToStrings(Text: TStrings);
begin
  Text.BeginUpdate;
  try
    Text.Clear;
    MessageToText(0, Text);
  finally
    Text.EndUpdate;
  end;
end;

destructor TsanPBMessage.Destroy;
begin
  FFields.Free;
  if Assigned(FFieldDefStub) then FFieldDefStub.Free;
  inherited;
end;

function TsanPBMessage.FieldByName(FieldName: string): TsanPBField;
var
  I: integer;
begin

  if RecordCount = 0 then begin
    raise Exception.Create(ERR_NO_DATA);
  end;

  Result:= nil;

  for I := 1 to FFields.Count do begin
    if TsanPBField(FFields.Items[I-1]).FieldDef.FieldName = FieldName then begin
      Result:= TsanPBField(FFields.Items[I-1]);
      break;
    end;
  end;

  if Not Assigned(Result)
    then raise Exception.Create(Format(ERR_FIELD_NOT_FOUND, [FieldName]));

end;

function TsanPBMessage.FieldByStoreIndex(StoreIndex: integer): TsanPBField;
var
  I: integer;
begin

  Result:= nil;

  for I := 1 to FFields.Count do begin
    if TsanPBField(FFields.Items[I-1]).FieldDef.StoreIndex = StoreIndex then begin
      Result:= TsanPBField(FFields.Items[I-1]);
      break;
    end;
  end;

end;

function TsanPBMessage.GetEmptyValue: Variant;
begin
  raise Exception.Create(ERR_NOT_APPLICABLE);
end;

function TsanPBMessage.GetRecordCount: integer;
begin
  if IsRoot
    then Result:= 1
    else Result:= inherited;
end;

function TsanPBMessage.InternalAppendValue(Value: Variant): integer;
begin
  raise Exception.Create(ERR_NOT_APPLICABLE);
end;

function TsanPBMessage.InternalGetValue(RecordIndex: integer): Variant;
begin
  raise Exception.Create(ERR_NOT_APPLICABLE);
end;

function TsanPBMessage.IsRoot: Boolean;
begin
  Result:= (Owner = nil);
end;

procedure TsanPBMessage.ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64);
var
  Pos1: Int64;
  ProcessedSize: integer;
  WireTypeA: TsanPBWireType;
  StoreIndex: integer;
  DataLen: Int64;
  Field: TsanPBField;
begin

  if Not IsRoot then Append;

  Pos1:= Stream.Position;
  ProcessedSize:= 0;

  while ProcessedSize < Size do begin

    ReadHdr(Stream, WireTypeA, StoreIndex);

    if WireTypeA = wtLengthDelimited then begin
      DataLen:= ReadVarint(Stream);
    end else begin
      DataLen:= 0;
    end;

    Field:= FieldByStoreIndex(StoreIndex);

    if Assigned(Field) then begin
      Field.ReadData(Stream, WireTypeA, DataLen);
    end else begin
      SkipData(Stream, WireTypeA, DataLen);
    end;

    ProcessedSize:= Stream.Position - Pos1;

  end;

end;

procedure TsanPBMessage.LoadFromFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TsanPBMessage.LoadFromStream(Stream: TStream);
begin

  if Not IsRoot then begin
    raise Exception.Create(ERR_METHOD_ONLY_FOR_ROOT_MESSAGE);
  end;

  FRecordIndex:= -1;
  Stream.Seek(0, soBeginning);
  ReadData(Stream, wtLengthDelimited, Stream.Size);

end;

function TsanPBMessage.MessageByName(FieldName: string): TsanPBMessage;
var
  Field: TsanPBField;
begin

  Field:= FieldByName(FieldName);

  if not (Field is TsanPBMessage)
    then raise Exception.Create(Format(ERR_FIELD_IS_NOT_MESSAGE, [FieldName]));

  Result:= TsanPBMessage(Field);

end;

procedure TsanPBMessage.MessageToText(Level: integer; Text: TStrings);
var
  I: integer;
  J: integer;
  Field: TsanPBField;
begin

  if IsRoot then begin
    Text.Add(Format('%s%s', [SpaceOffset(Level),
                             FMessageType.Name]));
  end else begin
    Text.Add(Format('%s%s (%s)', [SpaceOffset(Level),
                                 FieldDef.FieldName,
                                 FMessageType.Name]));
  end;

  for I := 1 to FFields.Count do begin

    Field:= TsanPBField(FFields[I-1]);

    for J := 1 to Field.RecordCount do begin
      if Field.FieldDef.FieldType = ftMessage then begin
        TsanPBMessage(Field).MoveTo(J-1);
        TsanPBMessage(Field).MessageToText(Level+1, Text);
      end else begin
        Text.Add(Format('%s%s (%s): %s', [SpaceOffset(Level+1),
                                         Field.FieldDef.FieldName,
                                         Field.FieldDef.FieldTypeName,
                                         Field.GetValueAsString(J-1)]));
      end;
    end;

  end;

end;

procedure TsanPBMessage.MoveTo(Index: integer);
var
  pData: PsanPBData;
begin

  if IsRoot then begin
    raise Exception.Create(ERR_METHOD_ONLY_FOR_NOT_ROOT);
  end;

  pData:= GetPBData(Index);
  SetMsgContext(PsanPBContext(pData^.pStorage));
  FRecordIndex:= Index;

end;

procedure TsanPBMessage.WriteRecord(Stream: TStream);
var
  I: integer;
  Size: Int64;
begin

  TempStream.Seek(0, soFromBeginning);

  for I := 1 to FFields.Count do begin
    TsanPBField(FFields[I-1]).BeforeWrite;
  end;

  for I := 1 to FFields.Count do begin
    TsanPBField(FFields[I-1]).WriteData(TempStream);
  end;

  Size:= TempStream.Position;

  if Size > 0 then begin

    if Not IsRoot then begin
      WriteHdr(Stream, wtLengthDelimited, FieldDef.StoreIndex);
      WriteVarint(Stream, Size);
    end;

    TempStream.Seek(0, soFromBeginning);
    Stream.CopyFrom(TempStream, Size);

  end;

  TempStream.Clear;

end;

procedure TsanPBMessage.WriteData(Stream: TStream);
var
  nRecIndex: integer;
begin

  if IsRoot then begin
    WriteRecord(Stream);
  end else begin
    for nRecIndex := 0 to RecordCount-1 do begin
      MoveTo(nRecIndex);
      WriteRecord(Stream);
    end;
  end;

end;

procedure TsanPBMessage.SaveToFile(FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TsanPBMessage.SaveToStream(Stream: TStream);
begin
  WriteData(Stream);
end;

procedure TsanPBMessage.SetMsgContext(pContext: PsanPBContext);
var
  P: PsanPBContext;
begin

  P:= pContext;

  while Assigned(P) do begin
    P^.Field.ContextPtr:= P;
    P:= P^.pNext;
  end;

end;

procedure TsanPBMessage.SkipData(Stream: TStream; WireType: TsanPBWireType;
  Size: integer);
begin

  if (Size > 0) then begin
    SeekStream(Stream, Size);
  end else begin
    case WireType of
      wtVarint: ReadVarint(Stream);
      wt64bit:  SeekStream(Stream, 8);
      wt32bit:  SeekStream(Stream, 4);
      else begin
        raise Exception.Create(Format(ERR_UNSUPPORTED_WIRE_TYPE, [Integer(WireType)]));
      end;
    end;
  end;

end;

function TsanPBMessage.SpaceOffset(Level: integer): string;
begin
  Result:= StringOfChar(' ', Level * 2);
end;

function TsanPBFieldDef.GetFieldTypeName: string;
begin
  if Assigned(FCustomType) then begin
    Result:= FCustomType.Name;
  end else begin
    Result:= FieldTypeToString(FFieldType);
  end;
end;

{ TsanPBField }

function TsanPBField.AllocPBData: PsanPBData;
var
  pLast: PsanPBData;
begin

  if (FValues.Count > 0) and (FFieldDef.Option <> ftoRepeated) then begin
    Result:= FValues[0];
    Exit;
  end;

  if FValues.Count = 0 then begin
    pLast:= nil;
  end else begin
    pLast:= FValues[FValues.Count-1];
  end;

  Result:= MemoryManager.GetMem(SizeOf(TsanPBData));

  if Not Assigned(pLast) then begin
    ContextPtr^.pData:= Result;
  end else begin
    pLast.pNext:= Result;
  end;

  Result^.pNext:= nil;
  Result^.pStorage:= nil;

  FValues.Add(Result);

end;

function TsanPBField.AppendValue(Value: Variant): integer;
begin
  if IsOneOf then ClearOneOfFields;
  Result:= InternalAppendValue(Value);
end;

function TsanPBField.AppendValueAsBoolean(Value: Boolean): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsBytes(Value: TBytes): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsDouble(Value: double): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsFloat(Value: single): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsInt32(Value: integer): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsInt64(Value: Int64): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsString(Value: string): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsUInt32(Value: cardinal): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueAsUInt64(Value: UInt64): integer;
begin
  Result:= AppendValue(Value);
end;

function TsanPBField.AppendValueFromStream(Stream: TStream): integer;
var
  Bytes: TBytes;
begin
  Stream.Position := 0;
  SetLength(Bytes, Stream.Size);
  Stream.Read(pointer(Bytes)^, Stream.Size);
  Result:= AppendValue(Bytes);
end;

procedure TsanPBField.BeforeWrite;
begin
  if IsOneOf then SetDefaultOneOfFields;
end;

procedure TsanPBField.CheckRecordIndex(Index: integer);
begin
  // Index со значением 0 допустим, даже если RecordCount
  // вернет 0, в этом случае, мы вернем значение по умолчанию
  if (Index <> 0) and ((Index < 0) or (Index >= RecordCount)) then begin
    raise Exception.Create(ERR_RECORD_INDEX_OUT_OF_RANGE);
  end;
end;

procedure TsanPBField.Clear;
begin
  FValues.Clear;
end;

constructor TsanPBField.Create(AOwner: TsanPBField; FieldDefA: TsanPBFieldDef);
begin

  if Assigned(AOwner) and not (AOwner is TsanPBMessage)
    then raise Exception.Create(ERR_OWNER_NOT_IS_MESSAGEFIELD);

  inherited Create(AOwner);

  FValues:= TList.Create;
  FFieldDef:= FieldDefA;

  if Assigned(AOwner) then begin
    FMemoryManager:= TsanPBMessage(AOwner).FMemoryManager;
    FIsOwnerMemoryManager:= False;
  end else begin
    FMemoryManager:= TsanStackMemoryManager.Create;
    FIsOwnerMemoryManager:= True;
  end;

  FTempStream:= TMemoryStream.Create;

end;

destructor TsanPBField.Destroy;
begin
  FValues.Free;
  if FIsOwnerMemoryManager then begin
    FMemoryManager.Free;
  end;
  FTempStream.Free;
  inherited;
end;

function TsanPBField.GetRecordCount: integer;
begin
  Result:= FValues.Count;
end;

function TsanPBField.GetPBData(RecordIndex: integer): PsanPBData;
begin
  Result:= FValues[RecordIndex];
end;

function TsanPBField.GetValue(RecordIndex: integer): Variant;
begin

  CheckRecordIndex(RecordIndex);

  if IsEmpty then begin
    if Not VarIsEmpty(FieldDef.DefaultValue) then begin
      Result:= FieldDef.DefaultValue;
    end else begin
      Result:= GetEmptyValue;
    end;
  end else begin
    Result:= InternalGetValue(RecordIndex);
  end;

end;

function TsanPBField.GetValueAsBoolean(RecordIndex: integer): Boolean;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsBytes(RecordIndex: integer): TBytes;
begin
  CheckRecordIndex(RecordIndex);
  SetLength(Result, 0);
end;

function TsanPBField.GetValueAsDouble(RecordIndex: integer): double;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsFloat(RecordIndex: integer): single;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsInt32(RecordIndex: integer): integer;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsInt64(RecordIndex: integer): Int64;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsUInt32(RecordIndex: integer): cardinal;
begin
  Result:= GetValue(RecordIndex);
end;

function TsanPBField.GetValueAsUInt64(RecordIndex: integer): UInt64;
begin
  Result:= GetValue(RecordIndex);
end;

procedure TsanPBField.ClearOneOfFields;
var
  OwnerMessage: TsanPBMessage;
  Field: TsanPBField;
  I: integer;
begin

  if FFieldDef.FOption <> ftoOptional then begin
    raise Exception.Create(Format(ERR_ONEOF_FIELD_WRONG_OPTION,
      [FFieldDef.FieldName]));
  end;

  if Assigned(Owner) then begin

    OwnerMessage:= TsanPBMessage(Owner);

    for I:= 1 to OwnerMessage.FFields.Count do begin

      Field:= TsanPBField(OwnerMessage.FFields.Items[I-1]);

      if (Field <> Self)
          and (Field.FieldDef.OneOfName = FieldDef.OneOfName)
      then begin
        Field.Clear;
      end;

    end;

  end;

end;

procedure TsanPBField.SetDefaultOneOfFields;
var
  OwnerMessage: TsanPBMessage;
  Field: TsanPBField;
  IsAllFieldsEmpty: Boolean;
  I: integer;
begin

  if Assigned(Owner) and (Not VarIsEmpty(FieldDef.DefaultValue)) then begin

    IsAllFieldsEmpty:= True;

    OwnerMessage:= TsanPBMessage(Owner);
    for I:= 1 to OwnerMessage.FFields.Count do begin
      Field:= TsanPBField(OwnerMessage.FFields.Items[I-1]);
      if Field.FieldDef.OneOfName = FieldDef.OneOfName then begin
        IsAllFieldsEmpty:= IsAllFieldsEmpty and Field.IsEmpty;
      end;
    end;

    if IsAllFieldsEmpty then begin
      AppendValue(FieldDef.DefaultValue);
    end;

  end;

end;

function TsanPBField.IsEmpty: Boolean;
begin
  Result:= RecordCount = 0;
end;

function TsanPBField.IsOneOf: Boolean;
begin
  Result:= FFieldDef.OneOfName <> '';
end;

procedure TsanPBField.ReadHdr(Stream: TStream; var WireType: TsanPBWireType;
  var StoreIndex: integer);
var
  Hdr: UInt64;
begin

  Hdr:= ReadVarint(Stream);

  StoreIndex:= Integer(Hdr shr 3);
  WireType:= TsanPBWireType(Hdr and $07);

end;

procedure TsanPBField.ReadStream(Stream: TStream; var Buffer; Count: Longint);
var
  ReadBytes: Longint;
begin
  ReadBytes:= Stream.Read(Buffer, Count);
  if ReadBytes < Count then begin
    raise Exception.Create(ERR_UNEXPECTED_END_OF_DATA);
  end;
end;

function TsanPBField.ReadVarint(Stream: TStream): Uint64;
var
  ReceivedBytes: array[0..9] of Byte;
  I: integer;
  B: Uint64;
begin

  FillChar(ReceivedBytes, SizeOf(ReceivedBytes), 0);

  for I := 1 to 10 do begin

    ReadStream(Stream, ReceivedBytes[I-1], 1);

    if (ReceivedBytes[I-1] and $80) = 0 then begin
      break;
    end;

  end;

  Result:= 0;

  for I := 1 to 10 do begin
    B:= (ReceivedBytes[I-1] and $7F);
    B:= B shl (7*(I-1));
    Result:= Result or B;
  end;

end;

function TsanPBField.DecodeZigzag(Value: Uint64): Int64;
begin

  // Original 	Encoded As
  //  0 	        0
  // -1 	        1
  //  1 	        2
  // -2 	        3

  if (Value and $01) = 0 then begin
    Result:= Value shr 1;
  end else begin
    Result:= -(Value shr 1) - 1;
  end;

end;

function TsanPBField.EncodeZigzag(Value: Int64): UInt64;
begin

  if Value < 0 then begin
    Result:= (Abs(Value) shl 1) - 1;
  end else begin
    Result:= Value shl 1;
  end;

end;

procedure TsanPBField.SeekStream(Stream: TStream; Count: Longint);
var
  Pos1: Int64;
begin

  Pos1:= Stream.Position;

  Stream.Seek(Count, soCurrent);

  if Stream.Position - Pos1 < Count then begin
    raise Exception.Create(ERR_UNEXPECTED_END_OF_DATA);
  end;

end;

procedure TsanPBField.SetContext(pContext: PsanPBContext);
var
  P: PsanPBData;
begin

  FContextPtr:= pContext;

  FValues.Clear;

  if Assigned(FContextPtr) then begin

    P:= FContextPtr^.pData;

    while Assigned(P) do begin
      FValues.Add(P);
      P:= P^.pNext;
    end;

  end;

end;

procedure TsanPBField.WriteHdr(Stream: TStream; WireType: TsanPBWireType;
  StoreIndex: integer);
var
  Hdr: UInt64;
begin

  Hdr:= UInt64(StoreIndex) shl 3;
  Hdr:= Hdr or UInt64(WireType);
  WriteVarint(Stream, Hdr);

end;

procedure TsanPBField.WriteVarint(Stream: TStream; Value: UInt64);
var
  SendBytes: array[0..9] of Byte;
  I: integer;
  V: UInt64;
  Flag: Boolean;
  Size: integer;
begin

  V:= Value;

  for I := 1 to 10 do begin
    if I > 1 then V:= V shr 7;
    SendBytes[I-1]:= Byte(V and $7F);
  end;

  Flag:= False;
  Size:= 10;

  for I:= 9 downto 1 do begin
    Flag:= Flag or (SendBytes[I] > 0);
    if Flag then begin
      SendBytes[I-1]:= SendBytes[I-1] or $80;
    end else begin
      Dec(Size);
    end;
  end;

  Stream.Write(SendBytes, Size);

end;

{ TsanPBInt32Field }

procedure TsanPBInt32Field.CheckRange(Value: Int64);
begin
  if (Value < -2147483648) or (Value > 2147483647) then begin
        raise Exception.Create(Format(ERR_VALUE_TOO_BIG_FOR_FIELD,
          [FieldDef.FieldName]));
  end;
end;

function TsanPBInt32Field.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: integer;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_INT32_VALUE);
    end;
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(Integer));
  Move(ValueA, pData^.pStorage^, SizeOf(Integer));
  Result:= FValues.Count - 1;

end;

function TsanPBInt32Field.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: integer;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(Integer));
  Result:= ValueA;
end;

procedure TsanPBInt32Field.ReadFixSizeData(Stream: TStream);
var
  V: Int64;
  R: integer;
begin

  R:= 0;

  case FieldDef.FieldType of

    ftInt32:
    begin
      V:= Int64(ReadVarint(Stream));
      CheckRange(V);
      R:= Integer(V);
    end;

    ftSint32:
    begin
      V:= ReadVarint(Stream);
      V:= DecodeZigzag(V);
      CheckRange(V);
      R:= Integer(V);
    end;

    ftSfixed32:
    begin
      ReadStream(Stream, R, 4);
    end;

    else begin
      raise Exception.Create(ERR_READER_IS_NOT_DONE);
    end;

  end;

  AppendValue(R);

end;

procedure TsanPBInt32Field.WriteFixSizeData(Stream: TStream; RecordIndex: integer);
var
  V: integer;
begin

  V:= GetValue(RecordIndex);

  case FieldDef.FieldType of

    ftInt32:
    begin
      WriteVarint(Stream, V);
    end;

    ftSint32:
    begin
      V:= Integer(EncodeZigzag(V));
      WriteVarint(Stream, V);
    end;

    ftSfixed32:
    begin
      Stream.Write(V, 4);
    end;

    else begin
      raise Exception.Create(ERR_WRITER_IS_NOT_DONE);
    end;

  end;

end;

{ TsanPBUInt32Field }
function TsanPBUInt32Field.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: Cardinal;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_UINT32_VALUE);
    end;
  end;

  // For some reason, the compiler does not check range for Cardinal type
  if (Value < 0) or (Value > 4294967295) then begin
    raise Exception.Create(ERR_EXPECT_UINT32_VALUE);
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(Cardinal));
  Move(ValueA, pData^.pStorage^, SizeOf(Cardinal));
  Result:= FValues.Count - 1;

end;

function TsanPBUInt32Field.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: Cardinal;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(cardinal));
  Result:= ValueA;
end;

procedure TsanPBUInt32Field.ReadFixSizeData(Stream: TStream);
var
  V: UInt64;
  R: Cardinal;

  procedure CheckRange(Value: UInt64);
  begin
    if (Value > 4294967295) then begin
        raise Exception.Create(Format(ERR_VALUE_TOO_BIG_FOR_FIELD,
          [V, FieldDef.FieldName]));
      end
  end;

begin

  R:= 0;

  case FieldDef.FieldType of

    ftUInt32:
    begin
      V:= ReadVarint(Stream);
      CheckRange(V);
      R:= Cardinal(V);
    end;

    ftFixed32:
    begin
      ReadStream(Stream, R, 4);
    end;

    else begin
      raise Exception.Create(ERR_READER_IS_NOT_DONE);
    end;

  end;

  AppendValue(R);

end;

procedure TsanPBUInt32Field.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: cardinal;
begin

  V:= GetValue(RecordIndex);

  case FieldDef.FieldType of

    ftUInt32:
    begin
      WriteVarint(Stream, V);
    end;

    ftFixed32:
    begin
      Stream.Write(V, 4);
    end;

    else begin
      raise Exception.Create(ERR_WRITER_IS_NOT_DONE);
    end;

  end;

end;

{ TsanPBInt64Field }
function TsanPBInt64Field.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: Int64;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_INT64_VALUE);
    end;
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(Int64));
  Move(ValueA, pData^.pStorage^, SizeOf(Int64));
  Result:= FValues.Count - 1;

end;

function TsanPBInt64Field.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: Int64;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(Int64));
  Result:= ValueA;
end;

procedure TsanPBInt64Field.ReadFixSizeData(Stream: TStream);
var
  R: Int64;
begin

  R:= 0;

  case FieldDef.FieldType of

    ftInt64:
    begin
      R:= ReadVarint(Stream);
    end;

    ftSint64:
    begin
      R:= ReadVarint(Stream);
      R:= DecodeZigzag(R);
    end;

    ftSfixed64:
    begin
      ReadStream(Stream, R, 8);
    end;

    else begin
      raise Exception.Create(ERR_READER_IS_NOT_DONE);
    end;

  end;

  AppendValue(R);

end;

procedure TsanPBInt64Field.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: Int64;
begin

  V:= GetValue(RecordIndex);

  case FieldDef.FieldType of

    ftInt64:
    begin
      WriteVarint(Stream, V);
    end;

    ftSint64:
    begin
      V:= Integer(EncodeZigzag(V));
      WriteVarint(Stream, V);
    end;

    ftSfixed64:
    begin
      Stream.Write(V, 8);
    end;

    else begin
      raise Exception.Create(ERR_WRITER_IS_NOT_DONE);
    end;

  end;

end;

{ TsanPBUInt64Field }
function TsanPBUInt64Field.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: UInt64;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_UINT64_VALUE);
    end;
  end;

  if (Value < 0) then begin
    raise Exception.Create(ERR_EXPECT_UINT64_VALUE);
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(UInt64));
  Move(ValueA, pData^.pStorage^, SizeOf(UInt64));
  Result:= FValues.Count - 1;

end;

function TsanPBUInt64Field.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: UInt64;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(UInt64));
  Result:= ValueA;
end;

procedure TsanPBUInt64Field.ReadFixSizeData(Stream: TStream);
var
  R: UInt64;
begin

  R:= 0;

  case FieldDef.FieldType of

    ftUInt64:
    begin
      R:= ReadVarint(Stream);
    end;

    ftFixed64:
    begin
      ReadStream(Stream, R, 8);
    end;

    else begin
      raise Exception.Create(ERR_READER_IS_NOT_DONE);
    end;

  end;

  AppendValue(R);

end;

procedure TsanPBUInt64Field.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: UInt64;
begin

  V:= GetValue(RecordIndex);

  case FieldDef.FieldType of

    ftUInt64:
    begin
      WriteVarint(Stream, V);
    end;

    ftFixed64:
    begin
      Stream.Write(V, 8);
    end;

    else begin
      raise Exception.Create(ERR_WRITER_IS_NOT_DONE);
    end;

  end;

end;

{ TsanPBDoubleField }
function TsanPBDoubleField.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: double;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_DOUBLE_VALUE);
    end;
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(double));
  Move(ValueA, pData^.pStorage^, SizeOf(double));
  Result:= FValues.Count - 1;

end;

function TsanPBDoubleField.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: double;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(double));
  Result:= ValueA;
end;

procedure TsanPBDoubleField.ReadFixSizeData(Stream: TStream);
var
  R: double;
begin

  ReadStream(Stream, R, Sizeof(double));
  AppendValue(R);

end;

procedure TsanPBDoubleField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: double;
begin
  V:= GetValue(RecordIndex);
  Stream.Write(V, Sizeof(double));
end;

{ TsanPBStringField }

function TsanPBStringField.GetEmptyValue: Variant;
begin
  Result:= '';
end;

function TsanPBStringField.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: string;
begin
  ValueA:= Value;
  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(ValueA);
  Result:= FValues.Count - 1;
end;

function TsanPBStringField.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
begin
  pData:= GetPBData(RecordIndex);
  Result:= String(PChar(pData^.pStorage));
end;

procedure TsanPBStringField.ReadData(Stream: TStream; WireType: TsanPBWireType;
  Size: Int64);
var
  P: PAnsiChar;
begin
  GetMem(P, Size+1);
  try
    FillChar(P^, Size+1, 0);
    ReadStream(Stream, P^, Size);
    AppendValue(Utf8ToString(P));
  finally
    FreeMem(P);
  end;
end;

procedure TsanPBStringField.WriteData(Stream: TStream);
var
  I: integer;
  Utf8Str: RawByteString;
  P: PAnsiChar;
  Size: integer;
begin

  for I := 1 to RecordCount do begin

    Utf8Str:= Utf8Encode(GetValue(I-1));

    P:= @Utf8Str[1];
    Size:= Length(Utf8Str);

    WriteHdr(Stream, wtLengthDelimited, FieldDef.StoreIndex);
    WriteVarint(Stream, Size);
    Stream.WriteBuffer(P^, Size);

  end;

end;

{ TsanPBBooleanField }
function TsanPBBooleanField.GetEmptyValue: Variant;
begin
  Result:= False;
end;

function TsanPBBooleanField.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: boolean;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_BOOLEAN_VALUE);
    end;
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(boolean));
  Move(ValueA, pData^.pStorage^, SizeOf(boolean));
  Result:= FValues.Count - 1;

end;

function TsanPBBooleanField.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: Boolean;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(Boolean));
  Result:= ValueA;
end;

procedure TsanPBBooleanField.ReadFixSizeData(Stream: TStream);
var
  R: Byte;
begin
  ReadStream(Stream, R, 1);
  AppendValue(Boolean(R));
end;

procedure TsanPBBooleanField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: Byte;
begin
  V:= Byte(GetValueAsBoolean(RecordIndex));
  Stream.Write(V, 1);
end;

{ TsanPBBytesField }
function TsanPBBytesField.GetEmptyValue: Variant;
var
  EmptyValue: TBytes;
begin
  SetLength(EmptyValue, 0);
  Result:= EmptyValue;
end;

function TsanPBBytesField.GetValueAsString(RecordIndex: integer): string;
var
  Data: TBytes;
  I: integer;
begin

  Data:= GetValue(RecordIndex);

  Result:= '[';

  for I := 1 to Length(Data) do begin

    if I > SHOW_BYTES_LIMIT then begin
      Result:= Result + '...';
      break;
    end else begin
      Result:= Result + '0x' + IntToHex(Data[I-1], 2) + ' ';
    end;

  end;

  Result:= Result + ']';

end;

function TsanPBBytesField.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  Size: integer;
  pSrc, pDst: PByte;
  ValueA: TBytes;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_TBYTES_VALUE);
    end;
  end;

  pData:= AllocPBData;

  Size:= Length(ValueA);

  if Size > 0 then begin

    pData^.pStorage:= FMemoryManager.GetMem(Size + SizeOf(Size));
    Move(Size, pData^.pStorage^, SizeOf(Size));

    pSrc:= @ValueA[0];
    pDst:= Pointer(Int64(pData^.pStorage) + SizeOf(Size));

    Move(pSrc^, pDst^, Size);

  end else begin
    pData^.pStorage:= nil;
  end;

  Result:= FValues.Count - 1;

end;

function TsanPBBytesField.InternalGetValue(RecordIndex: integer): Variant;
var
  pSize: PInteger;
  pData: PsanPBData;
  pSrc, pDst: PByte;
  ValueA: TBytes;
begin

  pData:= GetPBData(RecordIndex);

  pSize:= pData^.pStorage;
  SetLength(ValueA, pSize^);

  pSrc:= Pointer(Int64(pData^.pStorage) + SizeOf(pSize^));
  pDst:= @ValueA[0];

  Move(pSrc^, pDst^, pSize^);

  Result:= ValueA;

end;

procedure TsanPBBytesField.ReadData(Stream: TStream; WireType: TsanPBWireType;
  Size: Int64);
var
  R: TBytes;
  P: PByte;
begin

  SetLength(R, Size);

  P:= @R[0];

  ReadStream(Stream, P^, Size);
  AppendValue(R);

end;

procedure TsanPBBytesField.WriteData(Stream: TStream);
var
  I: integer;
  Buf: TBytes;
  P: PByte;
  Size: integer;
begin

  for I := 1 to RecordCount do begin

    Buf:= GetValue(I-1);

    P:= @Buf[0];
    Size:= Length(Buf);

    WriteHdr(Stream, wtLengthDelimited, FieldDef.StoreIndex);
    WriteVarint(Stream, Size);
    Stream.WriteBuffer(P^, Size);

  end;

end;

{ TsanPBNumberField }

function TsanPBFixSizeField.GetEmptyValue: Variant;
begin
  Result:= 0;
end;

procedure TsanPBFixSizeField.ReadData(Stream: TStream; WireType: TsanPBWireType;
  Size: Int64);
var
  Pos1: Int64;
  ProcessedSize: Int64;
begin

  if Size > 0 then begin

    // Packed data
    Pos1:= Stream.Position;
    ProcessedSize:= 0;

    while ProcessedSize < Size do begin
      ReadFixSizeData(Stream);
      ProcessedSize:= Stream.Position - Pos1;
    end;

  end else begin
    ReadFixSizeData(Stream);
  end;

end;

{ TsanPBFloatField }
function TsanPBFloatField.InternalAppendValue(Value: Variant): integer;
var
  pData: PsanPBData;
  ValueA: single;
begin

  try
    ValueA:= Value;
  except
    on E: EVariantError do begin
      raise Exception.Create(ERR_EXPECT_SINGLE_VALUE);
    end;
  end;

  pData:= AllocPBData;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(single));
  Move(ValueA, pData^.pStorage^, SizeOf(single));
  Result:= FValues.Count - 1;

end;

function TsanPBFloatField.InternalGetValue(RecordIndex: integer): Variant;
var
  pData: PsanPBData;
  ValueA: single;
begin
  pData:= GetPBData(RecordIndex);
  Move(pData^.pStorage^, ValueA, Sizeof(single));
  Result:= ValueA;
end;

procedure TsanPBFloatField.ReadFixSizeData(Stream: TStream);
var
  R: single;
begin

  ReadStream(Stream, R, Sizeof(single));
  AppendValueAsFloat(R);

end;

procedure TsanPBFloatField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: single;
begin
  V:= GetValueAsFloat(RecordIndex);
  Stream.Write(V, Sizeof(single));
end;

procedure TsanPBFixSizeField.WriteData(Stream: TStream);
var
  Size: Int64;
begin

  if (FieldDef.Option = ftoRepeated) and (FieldDef.DataPacked) then begin
    TempStream.Seek(0, soFromBeginning);
    WritePackedData(TempStream);
    Size:= TempStream.Position;
    WriteHdr(Stream, wtLengthDelimited, FieldDef.StoreIndex);
    WriteVarint(Stream, Size);
    TempStream.Seek(0, soFromBeginning);
    Stream.CopyFrom(TempStream, Size);
    TempStream.Clear;
  end else begin
    WriteUnPackedData(Stream);
  end;

end;

procedure TsanPBFixSizeField.WritePackedData(Stream: TStream);
var
  I: integer;
begin

  for I := 1 to RecordCount do begin
    WriteFixSizeData(Stream, I-1);
  end;

end;

procedure TsanPBFixSizeField.WriteUnPackedData(Stream: TStream);
var
  I: integer;
  StoreIndex: integer;
  WireType: TsanPBWireType;
begin

  StoreIndex:= FieldDef.StoreIndex;
  WireType:= FieldClasses[FieldDef.FieldType].WireType;

  for I := 1 to RecordCount do begin
    WriteHdr(Stream, WireType, StoreIndex);
    WriteFixSizeData(Stream, I-1);
  end;

end;

{ TsanPBEnumField }
function TsanPBEnumField.GetValueAsString(RecordIndex: integer): string;
var
  EnumType: TsanPBEnumType;
  EnumValue: integer;
begin
  EnumType:= TsanPBEnumType(FieldDef.FCustomType);
  EnumValue:= GetValueAsInt32(RecordIndex);
  Result:= EnumType.EnumToString(EnumValue);
end;

function TsanPBEnumField.InternalAppendValue(Value: Variant): integer;
var
  EnumType: TsanPBEnumType;
begin

  if VarIsStr(Value) then begin
    EnumType:= TsanPBEnumType(FieldDef.FCustomType);
    Result:= inherited InternalAppendValue(EnumType.StringToEnum(Value));
  end else
  if VarIsNumeric(Value) then begin
    Result:= inherited InternalAppendValue(Value);
  end else begin
    raise Exception.Create(ERR_EXPECT_ENUM_INDEX_OR_NAME);
  end;

end;

procedure TsanPBEnumField.ReadFixSizeData(Stream: TStream);
var
  V: Int64;
  R: integer;
begin

  V:= Int64(ReadVarint(Stream));
  CheckRange(V);
  R:= Integer(V);

  AppendValue(R);

end;

procedure TsanPBEnumField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: integer;
begin
  V:= GetValue(RecordIndex);
  WriteVarint(Stream, V);
end;

{ TsanPBEnumType }

procedure TsanPBEnumType.AddEnumItem(Value: integer; Descr: string);
var
  EnumItem: TsanPBEnumItem;
begin

  if IndexOf(Value) <> -1 then begin
    raise Exception.Create(Format(ERR_DUPLICATE_ENUM_VALUE, [IntToStr(Value)]));
  end;

  if IndexOf(Descr) <> -1 then begin
    raise Exception.Create(Format(ERR_DUPLICATE_ENUM_VALUE, [Descr]));
  end;

  EnumItem.Value:= Value;
  EnumItem.Descr:= Descr;
  FEnumList.Add(EnumItem);

end;

constructor TsanPBEnumType.Create(OwnerA: TsanPBCustomType; TypeNameA: string);
begin
  inherited Create(OwnerA, ftEnum, TypeNameA);
  FEnumList:= TList<TsanPBEnumItem>.Create;
end;

destructor TsanPBEnumType.Destroy;
begin
  FEnumList.Free;
  inherited;
end;

function TsanPBEnumType.EnumToString(Value: integer): string;
var
  EnumIndex: integer;
begin

  EnumIndex:= IndexOf(Value);

  if (EnumIndex = -1) then begin
    Result:= Format(UNKNOWN_ENUM_VALUE, [Value, Name]);
  end else begin
    Result:= Format('%s (%d)', [FEnumList.Items[EnumIndex].Descr,
                                FEnumList.Items[EnumIndex].Value]);
  end;

end;

function TsanPBEnumType.IndexOf(EnumValue: integer): integer;
var
  I: integer;
begin
  Result:= -1;
  for I := 1 to FEnumList.Count do begin
    if FEnumList.Items[I-1].Value = EnumValue then begin
      Result:= I-1;
      break;
    end;
  end;
end;

function TsanPBEnumType.IndexOf(EnumDescr: string): integer;
var
  I: integer;
  EnumDescrU: string;
begin

  Result:= -1;
  EnumDescrU:= UpperCase(EnumDescr);

  for I := 1 to FEnumList.Count do begin
    if UpperCase(FEnumList.Items[I-1].Descr) = EnumDescrU then begin
      Result:= I-1;
      break;
    end;
  end;

end;

function TsanPBEnumType.StringToEnum(Value: string): integer;
var
  EnumIndex: integer;
begin

  EnumIndex:= IndexOf(Value);

  if EnumIndex = -1 then begin
    raise Exception.Create(Format(ERR_ENUM_DESCR_NOT_FOUND, [Value, Name]));
  end;

  Result:= FEnumList.Items[EnumIndex].Value;

end;

{ TsanPBCustomType }

constructor TsanPBCustomType.Create(OwnerA: TsanPBCustomType;
  FieldType: TsanPBFieldType; TypeNameA: string);
begin
  inherited Create(OwnerA);
  FName:= TypeNameA;
  FFieldType:= FieldType;
end;

initialization

  RegistryFieldClass(ftInt32,    wtVarint, TsanPBInt32Field);
  RegistryFieldClass(ftSint32,   wtVarint, TsanPBInt32Field);
  RegistryFieldClass(ftSfixed32, wt32bit,  TsanPBInt32Field);
  RegistryFieldClass(ftEnum,     wtVarint, TsanPBEnumField);

  RegistryFieldClass(ftUInt32,   wtVarint, TsanPBUInt32Field);
  RegistryFieldClass(ftFixed32,  wt32bit,  TsanPBUInt32Field);

  RegistryFieldClass(ftInt64,    wtVarint, TsanPBInt64Field);
  RegistryFieldClass(ftSint64,   wtVarint, TsanPBInt64Field);
  RegistryFieldClass(ftSfixed64, wt64bit,  TsanPBInt64Field);

  RegistryFieldClass(ftUInt64,   wtVarint, TsanPBUInt64Field);
  RegistryFieldClass(ftFixed64,  wt64bit,  TsanPBUInt64Field);

  RegistryFieldClass(ftFloat,    wt32bit, TsanPBFloatField);
  RegistryFieldClass(ftDouble,   wt64bit, TsanPBDoubleField);

  RegistryFieldClass(ftBoolean,  wtVarint, TsanPBBooleanField);

  RegistryFieldClass(ftString,   wtLengthDelimited, TsanPBStringField);
  RegistryFieldClass(ftBytes,    wtLengthDelimited, TsanPBBytesField);
  RegistryFieldClass(ftMessage,  wtLengthDelimited, TsanPBMessage);

end.
