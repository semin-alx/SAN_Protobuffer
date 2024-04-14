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
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTeamType: TsanPBMessageType;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  TeamMsg: TsanPBMessage;
  TeamId: integer;
  MembersField: TsanPBField;
  MemberName: string;
  I: integer;
begin

  // Создадим объект, который будет работать с данными
  // тип которых описан в FTeamType
  TeamMsg:= FTeamType.CreateInstance;

  try

    mmContents.Clear;

    // Загружаем данные из team.bin
    TeamMsg.LoadFromFile('team.bin');

    // Читаем поле team_id
    TeamId:= TeamMsg.FieldByName('team_id').GetValueAsInt32;
    mmContents.Lines.Add('team_id: ' + IntToStr(TeamId));

    // Читаем поле members
    // Поле определено как repeated - список значений
    MembersField:= TeamMsg.FieldByName('members');

    for I := 1 to MembersField.RecordCount do begin
      MemberName:= MembersField.GetValueAsString(I-1);
      mmContents.Lines.Add('member: ' + MemberName);
    end;

  finally
    TeamMsg.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  TeamMsg: TsanPBMessage;
begin

  // Создадим объект, который будет работать с данными
  // тип которых описан в FTeamType
  TeamMsg:= FTeamType.CreateInstance;

  try

    // Заполняем данными
    TeamMsg.FieldByName('team_id').AppendValueAsInt32(100);

    TeamMsg.FieldByName('members').AppendValueAsString('Ivan');
    TeamMsg.FieldByName('members').AppendValueAsString('Sergey');
    TeamMsg.FieldByName('members').AppendValueAsString('Oleg');

    // Сериализуем в бинарный файл team.bin
    TeamMsg.SaveToFile('team.bin');

    ShowMessage('OK');

  finally
    TeamMsg.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  // Описываем наш Protocol Buffer тип

  // message Team {
  //   optional int32 team_id = 1;
  //   repeated string members = 2;
  // }

  // Создаем объект, который будет содержать описание типа
  // Не забываем его потом освободить (см. OnDestroy)
  FTeamType:= TsanPBMessageType.Create(nil, 'Team');

  FTeamType.AddFieldDef(ftoOptional, ftInt32,  nil, 'team_id', 1);
  FTeamType.AddFieldDef(ftoRepeated, ftString, nil, 'members', 2);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTeamType.Free;
end;

end.
