unit diadoc;

interface

uses System.Classes, System.SysUtils, semin64.isl.provider,
  semin64.isl.wininet;

type
  TDiadocApi = class(TObject)
  private
    FToken: string;
    FHost: string;
    FPort: integer;
    FUserName: string;
    FPassword: string;
    FApiKey: string;
    FHTTPProvider: TsanIslHTTPProvider;
    function RequestToken: string;
    procedure AddAPIKeyToHeader;
    function HTTPAnswerToString: string;
    procedure PrepareRequest;
  public

    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure Get(Url: string; Answer: TStream);

    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property ApiKey: string read FApiKey write FApiKey;

  end;

implementation

const
  HTTP_OK = 200;

{ TDiadocApi }

procedure TDiadocApi.AddAPIKeyToHeader;
begin
  FHTTPProvider.RequestHeaders.Add(Format('Authorization: DiadocAuth ddauth_api_client_id=%s', [ApiKey]));
end;

procedure TDiadocApi.Connect;
begin

  if FHTTPProvider.Connected then FHTTPProvider.Connected:= False;

  if FApiKey = '' then begin
    raise Exception.Create('API Key is not specified');
  end;
  if FUserName = '' then begin
    raise Exception.Create('UserName is not specified');
  end;
  if FPassword = '' then begin
    raise Exception.Create('Password is not specified');
  end;
  FHTTPProvider.HTTPVersion:= HTTP_1_1;
  FHTTPProvider.Host:= FHost;
  FHTTPProvider.Port:= FPort;
  FHTTPProvider.Connected:= True;
  FToken:= RequestToken;

end;

constructor TDiadocApi.Create;
begin
  FToken:= '';
  FHost:= '';
  FPort:= 443;
  FUserName:= '';
  FPassword:= '';
  FApiKey:= '';
  FHTTPProvider:= TsanIslHTTPWinInetProvider.Create;
end;

destructor TDiadocApi.Destroy;
begin
  FHTTPProvider.Free;
  inherited;
end;

procedure TDiadocApi.Disconnect;
begin
  FToken:= '';
  FHTTPProvider.Connected:= False;
end;

procedure TDiadocApi.Get(Url: string; Answer: TStream);
var
  Ret: integer;
begin
  PrepareRequest;
  Ret:= FHTTPProvider.SendRequest(hvGET, Url);
  if Ret = HTTP_OK then begin
    Answer.CopyFrom(FHTTPProvider.AnswerData);
  end else begin
    raise Exception.Create(Format('Error [%d]:#13%s', [Ret, HTTPAnswerToString]));
  end;
end;


function TDiadocApi.HTTPAnswerToString: string;
var
  SResult: Ansistring;
begin
  FHTTPProvider.AnswerData.Seek(0, soBeginning);
  SetLength(SResult, FHTTPProvider.AnswerData.Size);
  Move(FHTTPProvider.AnswerData.Memory^, (@SResult[1])^, FHTTPProvider.AnswerData.Size);
  Result:= UTF8ToWideString(SResult);
end;

procedure TDiadocApi.PrepareRequest;
begin

  if FToken = '' then begin
    raise Exception.Create('You should call the Connect method at first');
  end;

  FHTTPProvider.RequestHeaders.Clear;
  FHTTPProvider.RequestData.Clear;
  FHTTPProvider.RequestHeaders.Add(Format('Authorization: DiadocAuth ddauth_api_client_id=%s,ddauth_token=%s',
    [FApiKey, FToken]));

end;

function TDiadocApi.RequestToken: string;
var
  RequestData: string;
  Buf: TBytes;
  Ret: integer;
begin

  RequestData:= Format('{"login" : "%s", "password" : "%s"}', [FUserName, FPassword]);

  FHTTPProvider.RequestHeaders.Clear;
  FHTTPProvider.RequestData.Clear;
  AddAPIKeyToHeader;
  FHTTPProvider.RequestHeaders.Add('Content-Type: application/json');
  Buf:= TEncoding.UTF8.GetBytes(RequestData);
  FHTTPProvider.RequestData.Write(Pointer(Buf)^, Length(Buf));
  Ret:= FHTTPProvider.SendRequest(hvPOST, '/V3/Authenticate?type=password');
  if Ret = HTTP_OK then begin
    Result:= HTTPAnswerToString;
  end else begin
    raise Exception.Create(Format('Error [%d]:#13%s', [Ret, HTTPAnswerToString]));
  end;

end;

end.
