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

    procedure ErrorNotApplicable;
    procedure CheckRecordIndex(Index: integer);
    function GetDefaultValue(const AllowedTypes: array of TVarType): Variant;
  protected
    function AppendValue: PsanPBData;
    function GetValue(RecordIndex: integer): PsanPBData;
    procedure SetContext(pContext: PsanPBContext);

    function ReadVarint(Stream: TStream): UInt64;
    procedure WriteVarint(Stream: TStream; Value: UInt64);
    function DecodeZigzag(Value: Uint64): Int64;
    function EncodeZigzag(Value: Int64): UInt64;
    procedure SeekStream(Stream: TStream; Count: Longint);
    procedure ReadStream(Stream: TStream; var Buffer; Count: Longint);
    procedure ReadHdr(Stream: TStream; var WireType: TsanPBWireType; var StoreIndex: integer);
    procedure WriteHdr(Stream: TStream; WireType: TsanPBWireType; StoreIndex: integer);

    function GetRecordCount: integer; virtual;
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual; abstract;

    property ContextPtr: PsanPBContext read FContextPtr write SetContext;
    property MemoryManager: TsanStackMemoryManager read FMemoryManager;
    property TempStream: TMemoryStream read FTempStream;

  public

    constructor Create(AOwner: TsanPBField; FieldDefA: TsanPBFieldDef); virtual;
    destructor Destroy; override;

    function AppendValueAsInt32(Value: integer): integer; virtual;
    function AppendValueAsUInt32(Value: cardinal): integer; virtual;
    function AppendValueAsInt64(Value: Int64): integer; virtual;
    function AppendValueAsUInt64(Value: UInt64): integer; virtual;
    function AppendValueAsFloat(Value: single): integer; virtual;
    function AppendValueAsDouble(Value: double): integer; virtual;
    function AppendValueAsBoolean(Value: Boolean): integer; virtual;
    function AppendValueAsString(Value: string): integer; virtual;
    function AppendValueAsBytes(Value: TBytes): integer; virtual;
    function AppendValueFromStream(Stream: TStream): integer; virtual;

    function GetValueAsInt32(RecordIndex: integer = 0): integer; virtual;
    function GetValueAsUInt32(RecordIndex: integer = 0): cardinal; virtual;
    function GetValueAsInt64(RecordIndex: integer = 0): Int64; virtual;
    function GetValueAsUInt64(RecordIndex: integer = 0): UInt64; virtual;
    function GetValueAsFloat(RecordIndex: integer = 0): single; virtual;
    function GetValueAsDouble(RecordIndex: integer = 0): double; virtual;
    function GetValueAsBoolean(RecordIndex: integer = 0): Boolean; virtual;
    function GetValueAsString(RecordIndex: integer = 0): string; virtual;
    function GetValueAsBytes(RecordIndex: integer = 0): TBytes; virtual;

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
  end;

  TsanPBInt32Field = class(TsanPBFixSizeField)
  protected
    procedure CheckRange(Value: Int64);
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsInt32(Value: integer): integer; override;
    function GetValueAsInt32(RecordIndex: integer): integer; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBUInt32Field = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsUInt32(Value: cardinal): integer; override;
    function GetValueAsUInt32(RecordIndex: integer): cardinal; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBInt64Field = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsInt64(Value: Int64): integer; override;
    function GetValueAsInt64(RecordIndex: integer): Int64; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBUInt64Field = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsUInt64(Value: UInt64): integer; override;
    function GetValueAsUInt64(RecordIndex: integer): UInt64; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBFloatField = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsFloat(Value: single): integer; override;
    function GetValueAsFloat(RecordIndex: integer): single; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBDoubleField = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsDouble(Value: double): integer; override;
    function GetValueAsDouble(RecordIndex: integer): double; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBBooleanField = class(TsanPBFixSizeField)
  protected
    procedure ReadFixSizeData(Stream: TStream); override;
    procedure WriteFixSizeData(Stream: TStream; RecordIndex: integer); override;
  public
    function AppendValueAsBoolean(Value: Boolean): integer; override;
    function GetValueAsBoolean(RecordIndex: integer): Boolean; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBStringField = class(TsanPBField)
  protected
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); override;
    procedure WriteData(Stream: TStream); override;
  public
    function AppendValueAsString(Value: string): integer; override;
    function GetValueAsString(RecordIndex: integer): string; override;
  end;

  TsanPBBytesField = class(TsanPBField)
  protected
    procedure ReadData(Stream: TStream; WireType: TsanPBWireType; Size: Int64); override;
    procedure WriteData(Stream: TStream); override;
  public
    function AppendValueAsBytes(Value: TBytes): integer; override;
    function AppendValueFromStream(Stream: TStream): integer; override;
    function GetValueAsString(RecordIndex: integer): string; override;
    function GetValueAsBytes(RecordIndex: integer): TBytes; override;
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
  public
    function AppendValueAsString(Value: string): integer; override;
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
  ERR_NOT_APPLICABLE_TYPE = 'Not applicable for this type';
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
  ERR_INVALID_DEFAULT_VALUE_TYPE = 'Invalid default value type: %s';
  ERR_FIELD_IS_NOT_MESSAGE = 'Field %s is not message type';
  ERR_NO_DATA = 'No data';

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
end;

{ TsanPBMessageField }

procedure TsanPBMessage.Append;
var
  pData: PsanPBData;
begin

  if IsRoot then begin
    raise Exception.Create(ERR_METHOD_ONLY_FOR_NOT_ROOT);
  end;

  pData:= AppendValue;
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

function TsanPBMessage.GetRecordCount: integer;
begin
  if IsRoot
    then Result:= 1
    else Result:= inherited;
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

  pData:= GetValue(Index);
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

function TsanPBField.AppendValue: PsanPBData;
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

function TsanPBField.AppendValueAsBoolean(Value: Boolean): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsBytes(Value: TBytes): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsDouble(Value: double): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsFloat(Value: single): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsInt32(Value: integer): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsInt64(Value: Int64): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsString(Value: string): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsUInt32(Value: cardinal): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueAsUInt64(Value: UInt64): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

function TsanPBField.AppendValueFromStream(Stream: TStream): integer;
begin
  Result:= 0;
  ErrorNotApplicable;
end;

procedure TsanPBField.CheckRecordIndex(Index: integer);
begin
  // Index со значением 0 допустим, даже если RecordCount
  // вернет 0, в этом случае, мы вернем значение по умолчанию
  if (Index <> 0) and ((Index < 0) or (Index >= RecordCount)) then begin
    raise Exception.Create(ERR_RECORD_INDEX_OUT_OF_RANGE);
  end;
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

procedure TsanPBField.ErrorNotApplicable;
begin
  raise Exception.Create(ERR_NOT_APPLICABLE_TYPE);
end;

function TsanPBField.GetDefaultValue(const AllowedTypes: array of TVarType): Variant;
var
  ValueType: TVarType;
  AllowedType: TVarType;
begin

  ValueType:= VarType(FieldDef.DefaultValue);

  for AllowedType in AllowedTypes do begin
    if ValueType = AllowedType then begin
      Result:= FieldDef.DefaultValue;
      Exit;
    end;
  end;

  raise Exception.Create(Format(ERR_INVALID_DEFAULT_VALUE_TYPE,
    [VarTypeAsText(ValueType)]));

end;

function TsanPBField.GetRecordCount: integer;
begin
  Result:= FValues.Count;
end;

function TsanPBField.GetValue(RecordIndex: integer): PsanPBData;
begin
  Result:= FValues[RecordIndex];
end;

function TsanPBField.GetValueAsBoolean(RecordIndex: integer): Boolean;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= False;
  end else begin
    Result:= GetDefaultValue([varBoolean]);
  end;

end;

function TsanPBField.GetValueAsBytes(RecordIndex: integer): TBytes;
begin
  CheckRecordIndex(RecordIndex);
  SetLength(Result, 0);
end;

function TsanPBField.GetValueAsDouble(RecordIndex: integer): double;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= 0;
  end else begin
    Result:= GetDefaultValue([varShortInt, varSmallint, varInteger,
                              varByte, varWord, varLongWord, varInt64,
                              varSingle, varCurrency, varDouble]);
  end;

end;

function TsanPBField.GetValueAsFloat(RecordIndex: integer): single;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= 0;
  end else begin
    // varDouble - здесь это не ошибка, т.к. если в FieldDef.DefaultValue
    // установить значение, которое соответствует типу Single
    // функция VarType(FieldDef.DefaultValue) вернет не varSingle, а
    // varDouble. Вообще, чтобы я не прописывал в FieldDef.DefaultValue
    // функция VarType всегда возвращала varCurrency или varDouble
    Result:= GetDefaultValue([varShortInt, varSmallint, varInteger,
                              varByte, varWord, varLongWord, varInt64,
                              varSingle, varCurrency, varDouble]);
  end;

end;

function TsanPBField.GetValueAsInt32(RecordIndex: integer): integer;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= 0;
  end else begin
    Result:= GetDefaultValue([varShortInt, varSmallint, varInteger,
                              varByte, varWord, varLongWord]);
  end;

end;

function TsanPBField.GetValueAsInt64(RecordIndex: integer): Int64;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= 0;
  end else begin
    Result:= GetDefaultValue([varShortInt, varSmallint, varInteger,
                              varByte, varWord, varLongWord, varInt64]);
  end;

end;

function TsanPBField.GetValueAsString(RecordIndex: integer): string;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= '';
  end else begin
    Result:= GetDefaultValue([varString, varUString]);
  end;

end;

function TsanPBField.GetValueAsUInt32(RecordIndex: integer): cardinal;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= 0;
  end else begin
    Result:= GetDefaultValue([varByte, varWord, varLongWord]);
  end;

end;

function TsanPBField.GetValueAsUInt64(RecordIndex: integer): UInt64;
begin

  CheckRecordIndex(RecordIndex);

  if VarIsEmpty(FieldDef.DefaultValue) then begin
    Result:= 0;
  end else begin
    Result:= GetDefaultValue([varByte, varWord, varLongWord, varInt64]);
  end;

end;

function TsanPBField.IsEmpty: Boolean;
begin
  Result:= RecordCount = 0;
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

function TsanPBInt32Field.AppendValueAsInt32(Value: integer): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(Integer));
  Move(Value, pData^.pStorage^, SizeOf(Integer));
  Result:= FValues.Count - 1;
end;

procedure TsanPBInt32Field.CheckRange(Value: Int64);
begin
  if (Value < -2147483648) or (Value > 2147483647) then begin
        raise Exception.Create(Format(ERR_VALUE_TOO_BIG_FOR_FIELD,
          [FieldDef.FieldName]));
  end;
end;

function TsanPBInt32Field.GetValueAsInt32(RecordIndex: integer): integer;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(Integer));
  end;

end;

function TsanPBInt32Field.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= IntToStr(GetValueAsInt32(RecordIndex));
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

  AppendValueAsInt32(R);

end;

procedure TsanPBInt32Field.WriteFixSizeData(Stream: TStream; RecordIndex: integer);
var
  V: integer;
begin

  V:= GetValueAsInt32(RecordIndex);

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

function TsanPBUInt32Field.AppendValueAsUInt32(Value: cardinal): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(cardinal));
  Move(Value, pData^.pStorage^, SizeOf(cardinal));
  Result:= FValues.Count - 1;
end;

function TsanPBUInt32Field.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= UIntToStr(GetValueAsUInt32(RecordIndex));
end;

function TsanPBUInt32Field.GetValueAsUInt32(RecordIndex: integer): cardinal;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(cardinal));
  end;

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

  AppendValueAsUInt32(R);

end;

procedure TsanPBUInt32Field.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: cardinal;
begin

  V:= GetValueAsUInt32(RecordIndex);

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

function TsanPBInt64Field.AppendValueAsInt64(Value: Int64): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(Int64));
  Move(Value, pData^.pStorage^, SizeOf(Int64));
  Result:= FValues.Count - 1;
end;

function TsanPBInt64Field.GetValueAsInt64(RecordIndex: integer): Int64;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(Int64));
  end;

end;

function TsanPBInt64Field.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= IntToStr(GetValueAsInt64(RecordIndex));
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

  AppendValueAsInt64(R);

end;

procedure TsanPBInt64Field.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: Int64;
begin

  V:= GetValueAsInt64(RecordIndex);

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

function TsanPBUInt64Field.AppendValueAsUInt64(Value: UInt64): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(UInt64));
  Move(Value, pData^.pStorage^, SizeOf(UInt64));
  Result:= FValues.Count - 1;
end;

function TsanPBUInt64Field.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= UIntToStr(GetValueAsUInt64(RecordIndex));
end;

function TsanPBUInt64Field.GetValueAsUInt64(RecordIndex: integer): UInt64;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(UInt64));
  end;

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

  AppendValueAsUInt64(R);

end;

procedure TsanPBUInt64Field.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: UInt64;
begin

  V:= GetValueAsUInt64(RecordIndex);

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

function TsanPBDoubleField.AppendValueAsDouble(Value: double): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(double));
  Move(Value, pData^.pStorage^, SizeOf(double));
  Result:= FValues.Count - 1;
end;

function TsanPBDoubleField.GetValueAsDouble(RecordIndex: integer): double;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(double));
  end;

end;

function TsanPBDoubleField.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= FloatToStr(GetValueAsDouble(RecordIndex));
end;

procedure TsanPBDoubleField.ReadFixSizeData(Stream: TStream);
var
  R: double;
begin

  ReadStream(Stream, R, 8);
  AppendValueAsDouble(R);

end;

procedure TsanPBDoubleField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: double;
begin
  V:= GetValueAsDouble(RecordIndex);
  Stream.Write(V, 8);
end;

{ TsanPBStringField }

function TsanPBStringField.AppendValueAsString(Value: string): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(Value);
  Result:= FValues.Count - 1;
end;

function TsanPBStringField.GetValueAsString(RecordIndex: integer): string;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Result:= PChar(pData^.pStorage);
  end;

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
    AppendValueAsString(Utf8ToString(P));
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

    Utf8Str:= Utf8Encode(GetValueAsString(I-1));

    P:= @Utf8Str[1];
    Size:= Length(Utf8Str);

    WriteHdr(Stream, wtLengthDelimited, FieldDef.StoreIndex);
    WriteVarint(Stream, Size);
    Stream.WriteBuffer(P^, Size);

  end;

end;

{ TsanPBBooleanField }

function TsanPBBooleanField.AppendValueAsBoolean(Value: Boolean): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(Boolean));
  Move(Value, pData^.pStorage^, SizeOf(Boolean));
  Result:= FValues.Count - 1;
end;

function TsanPBBooleanField.GetValueAsBoolean(RecordIndex: integer): Boolean;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(Boolean));
  end;

end;

function TsanPBBooleanField.GetValueAsString(RecordIndex: integer): string;
begin
  if GetValueAsBoolean(RecordIndex) then begin
    Result:= 'True';
  end else begin
    Result:= 'False';
  end;
end;

procedure TsanPBBooleanField.ReadFixSizeData(Stream: TStream);
var
  R: Byte;
begin
  ReadStream(Stream, R, 1);
  AppendValueAsBoolean(Boolean(R));
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

function TsanPBBytesField.AppendValueAsBytes(Value: TBytes): integer;
var
  pData: PsanPBData;
  Size: integer;
  pSrc, pDst: PByte;
begin

  pData:= AppendValue;

  Size:= Length(Value);

  if Size > 0 then begin

    pData^.pStorage:= FMemoryManager.GetMem(Size + SizeOf(Size));
    Move(Size, pData^.pStorage^, SizeOf(Size));

    pSrc:= @Value[0];
    pDst:= Pointer(Int64(pData^.pStorage) + SizeOf(Size));

    Move(pSrc^, pDst^, Size);

  end else begin
    pData^.pStorage:= nil;
  end;

  Result:= FValues.Count - 1;

end;

function TsanPBBytesField.AppendValueFromStream(Stream: TStream): integer;
var
  Bytes: TBytes;
begin
  Stream.Position := 0;
  SetLength(Bytes, Stream.Size);
  Stream.Read(pointer(Bytes)^, Stream.Size);
  Result:= AppendValueAsBytes(Bytes);
end;

function TsanPBBytesField.GetValueAsBytes(RecordIndex: integer): TBytes;
var
  pSize: PInteger;
  pData: PsanPBData;
  pSrc, pDst: PByte;
begin

  Result:= inherited;
  if IsEmpty then Exit;

  pData:= GetValue(RecordIndex);

  pSize:= pData^.pStorage;
  SetLength(Result, pSize^);

  pSrc:= Pointer(Int64(pData^.pStorage) + SizeOf(pSize^));
  pDst:= @Result[0];

  Move(pSrc^, pDst^, pSize^);

end;

function TsanPBBytesField.GetValueAsString(RecordIndex: integer): string;
var
  Data: TBytes;
  I: integer;
begin

  Data:= GetValueAsBytes(RecordIndex);

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

procedure TsanPBBytesField.ReadData(Stream: TStream; WireType: TsanPBWireType;
  Size: Int64);
var
  R: TBytes;
  P: PByte;
begin

  SetLength(R, Size);

  P:= @R[0];

  ReadStream(Stream, P^, Size);
  AppendValueAsBytes(R);

end;

procedure TsanPBBytesField.WriteData(Stream: TStream);
var
  I: integer;
  Buf: TBytes;
  P: PByte;
  Size: integer;
begin

  for I := 1 to RecordCount do begin

    Buf:= GetValueAsBytes(I-1);

    P:= @Buf[0];
    Size:= Length(Buf);

    WriteHdr(Stream, wtLengthDelimited, FieldDef.StoreIndex);
    WriteVarint(Stream, Size);
    Stream.WriteBuffer(P^, Size);

  end;

end;

{ TsanPBNumberField }

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

function TsanPBFloatField.AppendValueAsFloat(Value: single): integer;
var
  pData: PsanPBData;
begin
  pData:= AppendValue;
  pData^.pStorage:= FMemoryManager.GetMem(SizeOf(single));
  Move(Value, pData^.pStorage^, SizeOf(single));
  Result:= FValues.Count - 1;
end;

function TsanPBFloatField.GetValueAsFloat(RecordIndex: integer): single;
var
  pData: PsanPBData;
begin

  Result:= inherited;

  if Not IsEmpty then begin
    pData:= GetValue(RecordIndex);
    Move(pData^.pStorage^, Result, Sizeof(single));
  end;

end;

function TsanPBFloatField.GetValueAsString(RecordIndex: integer): string;
begin
  Result:= FloatToStr(GetValueAsFloat(RecordIndex));
end;

procedure TsanPBFloatField.ReadFixSizeData(Stream: TStream);
var
  R: single;
begin

  ReadStream(Stream, R, 4);
  AppendValueAsFloat(R);

end;

procedure TsanPBFloatField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: single;
begin
  V:= GetValueAsFloat(RecordIndex);
  Stream.Write(V, 4);
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

function TsanPBEnumField.AppendValueAsString(Value: string): integer;
var
  EnumType: TsanPBEnumType;
begin
  EnumType:= TsanPBEnumType(FieldDef.FCustomType);
  Result:= AppendValueAsInt32(EnumType.StringToEnum(Value));
end;

function TsanPBEnumField.GetValueAsString(RecordIndex: integer): string;
var
  EnumType: TsanPBEnumType;
  EnumValue: integer;
begin
  EnumType:= TsanPBEnumType(FieldDef.FCustomType);
  EnumValue:= GetValueAsInt32(RecordIndex);
  Result:= EnumType.EnumToString(EnumValue);
end;

procedure TsanPBEnumField.ReadFixSizeData(Stream: TStream);
var
  V: Int64;
  R: integer;
begin

  V:= Int64(ReadVarint(Stream));
  CheckRange(V);
  R:= Integer(V);

  AppendValueAsInt32(R);

end;

procedure TsanPBEnumField.WriteFixSizeData(Stream: TStream;
  RecordIndex: integer);
var
  V: integer;
begin
  V:= GetValueAsInt32(RecordIndex);
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
