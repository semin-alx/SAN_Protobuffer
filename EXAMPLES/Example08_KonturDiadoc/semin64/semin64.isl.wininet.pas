unit semin64.isl.wininet;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses System.SysUtils, Winapi.WinInet, Winapi.Windows, semin64.isl.provider;

type
  TsanIslHTTPWinInetProvider = class(TsanIslHTTPProvider)
  private
    FMsgTableModule: HMODULE;   // HMODULE wininet.dll for resolve error strings by numbers
    FInternetHandle: HINTERNET; // Handle of InternetOpen
    FConnectHandle:  HINTERNET; // Handle of InternetConnect
    procedure CloseWininetHandle(var Handle: HINTERNET);
    function CreateRequest(Verb: TsanIslHTTPVerbs; Command: string): HINTERNET;
    procedure PrepareRequestHeaders(hRequest: HINTERNET);
    procedure SendToServer(hRequest: HINTERNET);
    procedure ReceiveFromServer(hRequest: HINTERNET);
    procedure CheckHost;
    procedure CheckPort;
    function GetAnswerHeaders(hRequest: HINTERNET): integer;
    function GetAnswerHeader(hRequest: HINTERNET; dwInfoLevel: DWORD): string;
  protected
    procedure InternalConnect; override;
    procedure InternalDisconnect; override;
    function  InternalSendRequest(Verb: TsanIslHTTPVerbs; Command: string): integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WininetCheck(RetVal: BOOL);
  end;

implementation

{ TislHTTPWinInetProvider }

resourcestring
  ISL_ERR_INVALID_HOST = 'Invalid host name';
  ISL_ERR_INVALID_PORT = 'Invalid port number';
  ISL_ERR_WININET = '%s (Error number: %d)';

const
  RESPONSE_BUFFER_SIZE = 4096;

procedure TsanIslHTTPWinInetProvider.CheckHost;
begin
  if Trim(Host)  = '' then raise EsanIslConnectionError.Create(ISL_ERR_INVALID_HOST);
end;

procedure TsanIslHTTPWinInetProvider.CheckPort;
begin
  if Port <= 0 then raise EsanIslConnectionError.Create(ISL_ERR_INVALID_PORT);
end;

procedure TsanIslHTTPWinInetProvider.CloseWininetHandle(var Handle: HINTERNET);
begin
  if Assigned(Handle) then begin
    InternetCloseHandle(Handle);
    Handle:= nil;
  end;
end;

constructor TsanIslHTTPWinInetProvider.Create;
begin
  inherited;
  FMsgTableModule:= 0;
  FInternetHandle:= nil;
  FConnectHandle:= nil;
  FMsgTableModule:= LoadLibrary('wininet.dll');
end;

function TsanIslHTTPWinInetProvider.CreateRequest(Verb: TsanIslHTTPVerbs;
  Command: string): HINTERNET;
var
  Flags: DWORD;
  Version: string;
  Len: DWORD;
begin

  Flags:= INTERNET_FLAG_RELOAD or
          INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
          INTERNET_FLAG_NO_CACHE_WRITE or
          INTERNET_FLAG_PRAGMA_NOCACHE or
          INTERNET_FLAG_KEEP_CONNECTION;

  Version:= 'HTTP/' + GetHTTPVersion;

  if UseSSL then begin
    Flags:= Flags or INTERNET_FLAG_SECURE;
  end;

  Result:= HttpOpenRequest(FConnectHandle,
                           PChar(GetHTTPVerb(Verb)),
                           PChar(Command),
                           PChar(Version),
                           nil, nil, Flags, 0);

  Len:= Sizeof(Flags);
  InternetQueryOption(Result, INTERNET_OPTION_SECURITY_FLAGS, @Flags, Len);
  Flags:= Flags or SECURITY_FLAG_IGNORE_UNKNOWN_CA;

  InternetSetOption(Result, INTERNET_OPTION_SECURITY_FLAGS, @Flags, Len);

  WininetCheck(BOOL(Result));

end;

destructor TsanIslHTTPWinInetProvider.Destroy;
begin
  if FMsgTableModule <> 0 then FreeLibrary(FMsgTableModule);
  inherited;
end;

function TsanIslHTTPWinInetProvider.GetAnswerHeader(hRequest: HINTERNET;
  dwInfoLevel: DWORD): string;
var
  Buf: PByte;
  BufSize: DWORD;
  dwIndex: DWORD;
  Res: BOOL;
begin

  Result:= '';

  dwIndex:= 0;
  BufSize:= 0;

  Res:= HttpQueryInfo(hRequest, dwInfoLevel, nil, BufSize, dwIndex);
  if Res and (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then WininetCheck(Res);

  if BufSize > 0 then begin

    // BufSize already contains place for #0
    // BufSize:= BufSize + SizeOf(Char); - it not need
    GetMem(Buf, BufSize);

    try

      FillChar(Buf^, 0, BufSize);

      dwIndex:= 0;

      WininetCheck(HttpQueryInfo(hRequest,
                                 dwInfoLevel,
                                 Buf, BufSize, dwIndex));

       Result:= PChar(Buf);

    finally
      FreeMem(Buf);
    end;

  end;

end;

function TsanIslHTTPWinInetProvider.GetAnswerHeaders(hRequest: HINTERNET): integer;
begin
  Result:= StrToInt(GetAnswerHeader(hRequest, HTTP_QUERY_STATUS_CODE));
  AnswerHeaders.Text:= GetAnswerHeader(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF);
end;

procedure TsanIslHTTPWinInetProvider.InternalConnect;
begin

  CheckHost;
  CheckPort;

  if Not Assigned(FInternetHandle) then begin
    FInternetHandle:= InternetOpen(PChar(Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    WininetCheck(BOOL(FInternetHandle));
  end;

  if Not Assigned(FConnectHandle) then begin
    FConnectHandle:= InternetConnect(FInternetHandle, PChar(Host), Port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    WininetCheck(BOOL(FConnectHandle));
  end;

end;

procedure TsanIslHTTPWinInetProvider.InternalDisconnect;
begin
  CloseWininetHandle(FConnectHandle);
  CloseWininetHandle(FInternetHandle);
end;

function TsanIslHTTPWinInetProvider.InternalSendRequest(Verb: TsanIslHTTPVerbs;
  Command: string): integer;
var
  hRequest: HINTERNET;
begin

  hRequest:= CreateRequest(Verb, Command);

  try
    PrepareRequestHeaders(hRequest);
    SendToServer(hRequest);
    ReceiveFromServer(hRequest);
    Result:= GetAnswerHeaders(hRequest); // Result HTTP Status code
  finally
    CloseWininetHandle(hRequest);
  end;

end;

procedure TsanIslHTTPWinInetProvider.PrepareRequestHeaders(hRequest: HINTERNET);
var
  I: integer;
  S: string;
begin

  for I := 1 to RequestHeaders.Count do begin
    S:= RequestHeaders.Strings[I-1];
    WininetCheck(HttpAddRequestHeaders(hRequest, PChar(S), Length(S), HTTP_ADDREQ_FLAG_ADD));
  end;

end;

procedure TsanIslHTTPWinInetProvider.ReceiveFromServer(hRequest: HINTERNET);
var
  Buf: array[0..RESPONSE_BUFFER_SIZE-1] of Byte;
  Res: BOOL;
  dwRead: DWORD;
begin

  AnswerData.Clear;

  repeat

    dwRead:= 0;
    Res:= InternetReadFile(hRequest, @Buf[0], RESPONSE_BUFFER_SIZE, dwRead);

    if dwRead > 0 then begin
      WininetCheck(Res);
      AnswerData.Write(Buf, dwRead);
    end;

  until Res and (dwRead = 0);

end;

procedure TsanIslHTTPWinInetProvider.SendToServer(hRequest: HINTERNET);
begin
  WininetCheck(HttpSendRequest(hRequest, nil, 0, RequestData.Memory, RequestData.Size));
end;


procedure TsanIslHTTPWinInetProvider.WininetCheck(RetVal: BOOL);
var
  ErrNo:  DWORD;
  ErrMes: string;
  Buffer: pointer;
begin

  if Not RetVal then begin

    ErrNo:= GetLastError;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                  FORMAT_MESSAGE_FROM_HMODULE,
                  Pointer(FMsgTableModule),
                  ErrNo,
                  0,
                  @Buffer,
                  0,
                  nil);

    ErrMes:= PChar(Buffer);
    LocalFree(HLOCAL(Buffer));

    raise EsanIslConnectionError.Create(Format(ISL_ERR_WININET, [ErrMes, ErrNo]));

  end;

end;

end.
