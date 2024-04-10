unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, semin64.protobuf, diadoc, proto;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtApiKey: TEdit;
    edtUserName: TEdit;
    edtPassword: TEdit;
    btnGetMyOrganizations: TButton;
    mmResult: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGetMyOrganizationsClick(Sender: TObject);
  private
    FDiadocAPI: TDiadocApi;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnGetMyOrganizationsClick(Sender: TObject);
var
  Buf: TMemoryStream;
  OrganizationList: TsanPBMessage;
begin

  Buf:= TMemoryStream.Create;
  OrganizationList:= CreateProtoInstance('Diadoc.Api.Proto.OrganizationList');

  try

    FDiadocAPI.ApiKey:= edtApiKey.Text;
    FDiadocAPI.UserName:= edtUserName.Text;
    FDiadocAPI.Password:= edtPassword.Text;
    FDiadocAPI.Host:= 'diadoc-api.kontur.ru';
    FDiadocAPI.Port:= 443;

    FDiadocAPI.Connect;

    FDiadocAPI.Get('/GetMyOrganizations?autoRegister=false', Buf);
    OrganizationList.LoadFromStream(Buf);
    OrganizationList.DbgMessageToStrings(mmResult.Lines);

    FDiadocAPI.Disconnect;

  finally
    Buf.Free;
    OrganizationList.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDiadocAPI:= TDiadocApi.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDiadocAPI.Free;
end;

end.
