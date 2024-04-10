unit semin64.memory;

{*****************************************************************}
{                                                                 }
{                          Semin64 library                        }
{                                                                 }
{                   Copyright(c) 2024 Semin Alexey                }
{                        All rights reserved                      }
{                                                                 }
{*****************************************************************}

{
    TsanStackMemoryManager - стековый менеджер памяти
  выделяет память блоками кратными размеру страниц в
  ОС с помощью функции VirtualAlloc. Выделение/освобождение
  памяти работает по принцепу стека, например, выделяем блоки:

  a:= Mng.GetMem(100);
  b:= Mng.GetMem(15);
  c:= Mng.GetMem(77);
  d:= Mng.GetMem(20);
  e:= Mng.GetMem(23);

  Теперь освободим блок c: Mng.FreeMem(c);
  В этом случае, будут освобождены блоки c, d, e

  Этот механизм очень удобен, когда мы выделяем много раз память
  и хотим разом все освободить начиная с определенного блока.

}


interface

uses winapi.windows, System.Classes, System.SysUtils;

type

  PsanStackMemoryPage = ^TsanStackMemoryPage;
  TsanStackMemoryPage = record
    PrevPageCursor: Cardinal;
    Address: Pointer;
    Size: Cardinal;
  end;

  TsanStackMemoryManager = class(TObject)
  private
    FMinPageSize: Cardinal;
    FPages: TList;
    FActivePageIndex: integer;
    FActivePagePtr: PsanStackMemoryPage;
    FActivePageCursor: Cardinal;
    FCheckLeakMemory: Boolean;
    function GetMinPageSize: Cardinal;
    function GetActivePageFreeSpace: Cardinal;
    procedure AllocNewPage(NeedSize: Cardinal);
    procedure AllocPageMemory(pPage: PsanStackMemoryPage; AllocSize: Cardinal);
    procedure FreePageMemory(pPage: PsanStackMemoryPage);
    procedure FreePages;
    function FindPage(P: Pointer): integer;
    function GetPageCount: integer;
    function GetTotalMemory: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMem(Value: string): PChar;    overload;
    function GetMem(Size: Cardinal): Pointer; overload;
    procedure FreeMem(P: Pointer);
    procedure Clear;

    // Запись всей памяти менеджера в текстовый файл (Для отладки)
    procedure DumpToFile(FileName: string);

    // Когда true, если при освобождении менеджера обнаружится неосвобожденная
    // память, будет вызвано исключение (по умолчанию выключено). Метод Clear
    // не производит проверку на утечку.
    property CheckLeakMemory: Boolean read FCheckLeakMemory write FCheckLeakMemory;

    // Для статистики
    // Кол-во страниц в менеджере. Это не страницы памяти в Windows
    // это страницы памяти, которые менеджер выделяет в процессе работы
    // Каждая страница имеет свой размер
    property PageCount: integer read GetPageCount;

    // Общая выделенная память менеджером у Windows
    property TotalMemory: integer read GetTotalMemory;

  end;

  // Снятие дампа с памяти и записи его в читабельном виде в текстовой файл
  procedure DumpToTxtFile(P: Pointer; Count: Cardinal; FileName: string; Append: Boolean = False);

implementation

function OpenLog(LogFile: string): THandle;
const
  ORIGIN_END = 2;
begin

  Result:= System.SysUtils.FileOpen(LogFile, fmOpenWrite);
  if Result = INVALID_HANDLE_VALUE then begin
    Result:= System.SysUtils.FileCreate(LogFile);
  end;

  if Result = INVALID_HANDLE_VALUE then begin
    raise Exception.Create(Format('Error create file %s', [LogFile]));
  end;

  System.SysUtils.FileSeek(Result, 0, ORIGIN_END);

end;

procedure AddLog(LogHandle: THandle; Text: string);
var
  AnsiText: AnsiString;
begin
  AnsiText:= AnsiString(Text + #13#10);
  System.SysUtils.FileWrite(LogHandle, AnsiText[1], Length(AnsiText));
end;

procedure CloseLog(LogHandle: THandle);
begin
  System.SysUtils.FileClose(LogHandle);
end;

procedure InternalDumpToTxtFile(P: Pointer; Count: Cardinal; LogHandle: THandle);
var
  n16: integer;
  I, J: integer;
  LineAdr: Int64;
  nOffset: Int64;
  S: string;
  V: PByte;
begin

  n16:= Count div 16;
  if (Count mod 16) > 0 then Inc(n16);

  for I := 1 to n16 do begin

    LineAdr:= Int64(P) + (I-1)*16;

    S:= Format('%.10x   ', [LineAdr]);

    for J := 1 to 16 do begin
      if J = 9 then S:= S + ' ';
      nOffset:= (I-1)*16 + J - 1;
      V:= PByte(Int64(P) + nOffset);
      if nOffset < Count then S:= S + Format('%.2x ', [Integer(V^)]);
    end;

    AddLog(LogHandle, S);

  end;

end;

procedure DumpToTxtFile(P: Pointer; Count: Cardinal; FileName: string; Append: Boolean);
var
  LogHandle: THandle;
begin

  if (Not Append) and FileExists(FileName)
    then DeleteFile(FileName);

  LogHandle:= OpenLog(FileName);

  try
    InternalDumpToTxtFile(P, Count, LogHandle);
  finally
    CloseLog(LogHandle)
  end;

end;

{ TsanStackMemoryManager }

procedure TsanStackMemoryManager.AllocNewPage(NeedSize: Cardinal);
var
  AllocSize: Cardinal;
  pNextPage: PsanStackMemoryPage;
begin

  // Вначале я выделял память по SystemInfo.dwPageSize,
  // но оказалось, что выделить можно не более 31017 раз, далее
  // была ошибка в VirualAlloc (code 8)
  // Поэтому с каждым разом будем увеличивать блок памяти в два раза
  // пока не достигнем FMinPageSize * 512 далее будем выделять уже
  // такими блоками

  // Расчитаем размер следующей страницы
  // Не хочу использовать функцию со степенью

  case FPages.Count of
    0: AllocSize:= FMinPageSize;
    1: AllocSize:= FMinPageSize * 2;
    2: AllocSize:= FMinPageSize * 4;
    3: AllocSize:= FMinPageSize * 8;
    4: AllocSize:= FMinPageSize * 16;
    5: AllocSize:= FMinPageSize * 32;
    6: AllocSize:= FMinPageSize * 64;
    7: AllocSize:= FMinPageSize * 128;
    8: AllocSize:= FMinPageSize * 256;
    else begin
      AllocSize:= FMinPageSize * 512;
    end;
  end;

  if NeedSize > AllocSize then begin
    AllocSize:= ((NeedSize div FMinPageSize) + 1) * FMinPageSize;
  end;

  if (FActivePageIndex = FPages.Count - 1) then begin
    New(pNextPage);
    FPages.Add(pNextPage);
    AllocPageMemory(pNextPage, AllocSize);
  end else begin

    pNextPage:= PsanStackMemoryPage(FPages.Items[FActivePageIndex+1]);

    if pNextPage^.Size < NeedSize then begin
      FreePageMemory(pNextPage);
      AllocPageMemory(pNextPage, AllocSize);
    end;

  end;

  if Assigned(FActivePagePtr)
    then pNextPage^.PrevPageCursor:= FActivePageCursor
    else pNextPage^.PrevPageCursor:= 0;

  FActivePagePtr:= pNextPage;
  Inc(FActivePageIndex);
  FActivePageCursor:= 0;

end;

procedure TsanStackMemoryManager.AllocPageMemory(pPage: PsanStackMemoryPage;
  AllocSize: Cardinal);
begin

  pPage^.Address:= VirtualAlloc(nil, AllocSize, MEM_RESERVE or MEM_COMMIT, PAGE_READWRITE);
  pPage^.Size:= AllocSize;

  if Not Assigned(pPage^.Address) then RaiseLastOSError;

end;

procedure TsanStackMemoryManager.Clear;
var
  SaveCheckLeakMemory: Boolean;
begin
  SaveCheckLeakMemory:= FCheckLeakMemory;
  try
    FCheckLeakMemory:= False;
    FreePages;
  finally
    FCheckLeakMemory:= SaveCheckLeakMemory;
  end;
end;

constructor TsanStackMemoryManager.Create;
begin
  FActivePageIndex:= -1;
  FActivePagePtr:= nil;
  FActivePageCursor:= 0;
  FCheckLeakMemory:= False;
  FPages:= TList.Create;
  FMinPageSize:= GetMinPageSize;
end;

destructor TsanStackMemoryManager.Destroy;
begin
  FreePages;
  FPages.Free;
  inherited;
end;

procedure TsanStackMemoryManager.DumpToFile(FileName: string);
var
  LogHandle: THandle;
  I: integer;
  pPage: PsanStackMemoryPage;
begin

  if FileExists(FileName) then DeleteFile(FileName);

  LogHandle:= OpenLog(FileName);

  try

    AddLog(LogHandle, Format('Pages: %d', [FPages.Count]));
    AddLog(LogHandle, Format('ActivePageIndex: %d', [FActivePageIndex]));
    AddLog(LogHandle, Format('ActivePageCursor: %d', [FActivePageCursor]));
    AddLog(LogHandle, '');

    for I := 1 to FPages.Count do begin

      pPage:= PsanStackMemoryPage(FPages.Items[I-1]);

      AddLog(LogHandle, Format('Dump of page %d', [I-1]));
      AddLog(LogHandle, Format('Page size %d', [pPage^.Size]));
      AddLog(LogHandle, '');

      InternalDumpToTxtFile(pPage^.Address, pPage^.Size, LogHandle);

      AddLog(LogHandle, '');

    end;

  finally
    CloseLog(LogHandle);
  end;

end;

function TsanStackMemoryManager.FindPage(P: Pointer): integer;
var
  I: integer;
  pPage: PsanStackMemoryPage;
  n1, n2, n0: Int64;
begin

  Result:= -1;

  n0:= Int64(P);

  for I := 0 to FActivePageIndex do begin

    pPage:= PsanStackMemoryPage(FPages.Items[I]);

    n1:= Int64(pPage^.Address);
    n2:= n1 + pPage^.Size - 1;

    if (n1 <= n0) and (n0 <= n2) then begin
      Result:= I;
      break;
    end;

  end;

end;

procedure TsanStackMemoryManager.FreeMem(P: Pointer);
var
  PageIndex: integer;
  pPage: PsanStackMemoryPage;
begin

  PageIndex:= FindPage(P);

  if PageIndex = -1 then raise Exception.Create('Invalid pointer');

  pPage:= FPages[PageIndex];

  if (P = pPage^.Address) and (PageIndex > 0) then begin
    FActivePageCursor:= FActivePagePtr^.PrevPageCursor;
    FActivePageIndex:= PageIndex - 1;
    FActivePagePtr:= FPages[FActivePageIndex];
  end else begin
    FActivePageIndex:= PageIndex;
    FActivePagePtr:= FPages[FActivePageIndex];
    FActivePageCursor:= Int64(P) - Int64(FActivePagePtr^.Address);
  end;

end;

procedure TsanStackMemoryManager.FreePageMemory(pPage: PsanStackMemoryPage);
begin
  VirtualFree(pPage^.Address, 0, MEM_RELEASE);
end;

procedure TsanStackMemoryManager.FreePages;
var
  I: integer;
  P: PsanStackMemoryPage;
  IsLeakMemory: Boolean;
begin

  IsLeakMemory:= (FActivePageIndex > 0) or (FActivePageCursor > 0);

  for I:= 1 to FPages.Count do begin
    P:= PsanStackMemoryPage(FPages.Items[I-1]);
    FreePageMemory(P);
    Dispose(P);
  end;

  FPages.Clear;

  FActivePageIndex:= -1;
  FActivePagePtr:= nil;
  FActivePageCursor:= 0;

  if FCheckLeakMemory and IsLeakMemory
    then raise Exception.Create('TsanStackMemoryManager: Leak memory');

end;

function TsanStackMemoryManager.GetMem(Size: Cardinal): Pointer;
begin

  if GetActivePageFreeSpace < Size then begin
    AllocNewPage(Size);
  end;

  Result:= Pointer(Int64(FActivePagePtr^.Address) + FActivePageCursor);

  Inc(FActivePageCursor, Size);

end;

function TsanStackMemoryManager.GetActivePageFreeSpace: Cardinal;
begin

  if Not Assigned(FActivePagePtr) then begin
    Result:= 0;
  end else begin
    Result:= FActivePagePtr^.Size - FActivePageCursor;
  end;

end;

function TsanStackMemoryManager.GetMem(Value: string): PChar;
var
  Size: integer;
  SizeNullTerm: integer;
  Len: integer;
begin

  Len:= Length(Value);
  Size:= Len * SizeOf(Char);
  SizeNullTerm:= SizeOf(Char);

  Result:= GetMem(Size + SizeNullTerm);
  if Len > 0 then Move(Value[1], Result^, Size);
  Result[Len]:= Char(0);

end;

function TsanStackMemoryManager.GetMinPageSize: Cardinal;
var
  SystemInfo: TSystemInfo;
begin

  GetSystemInfo(SystemInfo);
  Result:= SystemInfo.dwPageSize;

end;

function TsanStackMemoryManager.GetPageCount: integer;
begin
  Result:= FPages.Count;
end;

function TsanStackMemoryManager.GetTotalMemory: integer;
var
  I: Integer;
begin
  Result:= 0;
  for I := 1 to FPages.Count do begin
    Result:= Result + Integer(PsanStackMemoryPage(FPages.Items[I-1])^.Size);
  end;
end;

end.
