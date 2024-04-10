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
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
  private
    FTournamentType: TsanPBMessageType;
    FTeamType: TsanPBMessageType;
    procedure PrintTeamInfo(TeamMsg: TsanPBMessage);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnLoadClick(Sender: TObject);
var
  TournamentMsg: TsanPBMessage;
  TeamMsg: TsanPBMessage;
  I: integer;
begin

  mmContents.Clear;

  TournamentMsg:= FTournamentType.CreateInstance;

  try

    TournamentMsg.LoadFromFile('tournament.bin');

    TeamMsg:= TournamentMsg.MessageByName('teams');

    for I := 1 to TeamMsg.RecordCount do begin
      TeamMsg.MoveTo(I-1);
      PrintTeamInfo(TeamMsg);
    end;

  finally
    TournamentMsg.Free;
  end;

end;

procedure TForm1.btnSaveClick(Sender: TObject);
var
  TournamentMsg: TsanPBMessage;
  TeamMsg: TsanPBMessage;
begin

  TournamentMsg:= FTournamentType.CreateInstance;

  try

    TeamMsg:= TournamentMsg.MessageByName('teams');

    TeamMsg.Append;
    TeamMsg.FieldByName('team_id').AppendValueAsInt32(1);
    TeamMsg.FieldByName('members').AppendValueAsString('Ivan');
    TeamMsg.FieldByName('members').AppendValueAsString('Sergey');
    TeamMsg.FieldByName('members').AppendValueAsString('Oleg');

    TeamMsg.Append;
    TeamMsg.FieldByName('team_id').AppendValueAsInt32(2);
    TeamMsg.FieldByName('members').AppendValueAsString('Dmitriy');
    TeamMsg.FieldByName('members').AppendValueAsString('Alexey');
    TeamMsg.FieldByName('members').AppendValueAsString('Andrey');

    TournamentMsg.SaveToFile('tournament.bin');

    ShowMessage('OK');

  finally
    TournamentMsg.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

//   message Team {
//     optional int32 team_id = 1;
//     repeated string members = 2;
//   }

//   message Tournament {
//	   repeated Team teams = 1;
//   }

  FTeamType:= TsanPBMessageType.Create(nil, 'Team');
  FTeamType.AddFieldDef(ftoOptional, ftInt32,  nil, 'team_id', 1);
  FTeamType.AddFieldDef(ftoRepeated, ftString, nil, 'members', 2);

  FTournamentType:= TsanPBMessageType.Create(nil, 'Tournament');
  FTournamentType.AddFieldDef(ftoRepeated, ftMessage, FTeamType, 'teams', 1);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTeamType.Free;
  FTournamentType.Free;
end;

procedure TForm1.PrintTeamInfo(TeamMsg: TsanPBMessage);
var
  TeamId: integer;
  Members: TsanPBField;
  MemberName: string;
  I: integer;
begin

  TeamId:= TeamMsg.FieldByName('team_id').GetValueAsInt32;
  mmContents.Lines.Add('team_id: ' + IntToStr(TeamId));

  Members:= TeamMsg.FieldByName('members');

  for I := 1 to Members.RecordCount do begin
    MemberName:= Members.GetValueAsString(I-1);
    mmContents.Lines.Add('member: ' + MemberName);
  end;

  mmContents.Lines.Add('');

end;

end.
