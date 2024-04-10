unit semin64.isl.provider;

{---------------------------------------------------------------------------
    Class TislHTTPProvider is declare common interface for HTTP Providers
 ---------------------------------------------------------------------------}

interface

uses System.Classes, System.SysUtils;

type

  TsanIslHTTPVerbs = (hvGET, hvPOST, hvPUT);
  TsanIslHTTPVersion = (HTTP_1_0, HTTP_1_1);

  EsanIslConnectionError = class(Exception);

  TsanIslHTTPProvider = class(TObject)
  private
    FConnected: Boolean;
    FUseSSL: Boolean;
    FHost: string;
    FPort: integer;
    FHTTPVersion: TsanIslHTTPVersion;
    FAgent: string;
    FRequestData: TMemoryStream;
    FAnswerData: TMemoryStream;
    FRequestHeaders: TStrings;
    FAnswerHeaders: TStrings;
    procedure SetConnected(const Value: Boolean);
    procedure SetUseSSL(const Value: Boolean);
    procedure CheckNoActive;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    procedure SetAnswerHeaders(const Value: TStrings);
    procedure SetRequestHeaders(const Value: TStrings);
  protected
    function GetHTTPVerb(Verb: TsanIslHTTPVerbs): string;
    function GetHTTPVersion: string;
    procedure InternalConnect; virtual; abstract;
    procedure InternalDisconnect; virtual; abstract;
    function  InternalSendRequest(Verb: TsanIslHTTPVerbs; Command: string): integer; virtual; abstract;
  public

    constructor Create; virtual;
    destructor Destroy; override;

    // Before request you can set RequestHeaders and to prepare sending data
    // via TransferData. Result is HTTP response code
    // Server can return optional data over TransferData also.
    function SendRequest(Verb: TsanIslHTTPVerbs; Command: string): integer;

    // Open/Close connection
    property Connected: Boolean read FConnected write SetConnected;

    // Optional headers for the request
    property RequestHeaders: TStrings read FRequestHeaders write SetRequestHeaders;

    // All headers of answer
    property AnswerHeaders: TStrings read FAnswerHeaders write SetAnswerHeaders;

    // Send optional data when SendRequest call
    property RequestData: TMemoryStream read FRequestData;

    // Receive data when SendRequest call
    property AnswerData: TMemoryStream read FAnswerData;

    // Default: True
    property UseSSL: Boolean read FUseSSL write SetUseSSL;

    // Default HTTP 1.0 (HTTP_1_0)
    property HTTPVersion: TsanIslHTTPVersion read FHTTPVersion write FHTTPVersion;

    property Host: string read FHost write SetHost;

    // Default 443 (HTTPS)
    property Port: integer read FPort write SetPort;

    // HTTP Agent, Default: ISL_DEFAULT_AGENT
    property Agent: string read FAgent write FAgent;

  end;

implementation

{ TislProvider }

resourcestring

  ISL_DEFAULT_AGENT = 'ISLClient v1.0';

  ISL_ERR_CONNECTION_IS_OPEN = 'Error: Connection is open, changes is not accessible';
  ISL_ERR_CONNECTION_IS_NOT_OPEN = 'Error: Connection is not open';

procedure TsanIslHTTPProvider.CheckNoActive;
begin
  if FConnected then begin
    raise EsanIslConnectionError.Create(ISL_ERR_CONNECTION_IS_OPEN);
  end;
end;

constructor TsanIslHTTPProvider.Create;
begin
  FConnected:= False;
  FUseSSL:= True;
  FPort:= 443;
  FHost:= '';
  FAgent:= ISL_DEFAULT_AGENT;
  FHTTPVersion:= HTTP_1_0;
  FRequestHeaders:= TStringList.Create;
  FAnswerHeaders:= TStringList.Create;
  FRequestData:= TMemoryStream.Create;
  FAnswerData:= TMemoryStream.Create;
end;

destructor TsanIslHTTPProvider.Destroy;
begin
  Connected:= False;
  FRequestData.Free;
  FAnswerData.Free;
  FRequestHeaders.Free;
  FAnswerHeaders.Free;
  inherited;
end;

function TsanIslHTTPProvider.GetHTTPVerb(Verb: TsanIslHTTPVerbs): string;
begin
  case Verb of
    hvGET:  Result:= 'GET';
    hvPOST: Result:= 'POST';
    hvPUT:  Result:= 'PUT';
  end;
end;

function TsanIslHTTPProvider.GetHTTPVersion: string;
begin
  case FHTTPVersion of
    HTTP_1_0: Result:= '1.0';
    HTTP_1_1: Result:= '1.1';
  end;
end;

function TsanIslHTTPProvider.SendRequest(Verb: TsanIslHTTPVerbs;
  Command: string): integer;
begin

  if Not FConnected then begin
    raise EsanIslConnectionError.Create(ISL_ERR_CONNECTION_IS_NOT_OPEN);
  end else begin
    Result:= InternalSendRequest(Verb, Command);
  end;

end;

procedure TsanIslHTTPProvider.SetAnswerHeaders(const Value: TStrings);
begin
  FAnswerHeaders.Assign(Value);
end;

procedure TsanIslHTTPProvider.SetConnected(const Value: Boolean);
begin

  if FConnected <> Value then begin

    if Value then InternalConnect
             else InternalDisconnect;

    FConnected := Value;

  end;

end;

procedure TsanIslHTTPProvider.SetHost(const Value: string);
begin
  if FHost <> Value then begin
    CheckNoActive;
    FHost:= Value;
  end;
end;

procedure TsanIslHTTPProvider.SetPort(const Value: integer);
begin
  if FPort <> Value then begin
    CheckNoActive;
    FPort:= Value;
  end;
end;

procedure TsanIslHTTPProvider.SetRequestHeaders(const Value: TStrings);
begin
  FRequestHeaders.Assign(Value);
end;

procedure TsanIslHTTPProvider.SetUseSSL(const Value: Boolean);
begin
  if FUseSSL <> Value then begin
    CheckNoActive;
    FUseSSL:= Value;
  end;
end;

end.
