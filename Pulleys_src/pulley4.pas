{****************************************************************}
{* "Two vertical pulleys (case 2)" unit for Pulleys application *}
{****************************************************************}

unit pulley4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys4 }
  {************}
  TfPulleys4 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    laQuestion: TLabel;
    Label1, Label6, Label7, Label8, Label17, Label18, Label19, Label20: TLabel;
    Image1: TImage;
    laM1, laM2, laA1, laA2, laUA1, laUA2, laD: TLabel;
    edM1, edM2, edA1, edA2, edT, edS1, edS2: TEdit;
    imEval: TImage;
    btAnswer: TButton;
    btShow: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    rM1, rM2, rT, rA1, rA2, rS1, rS2: Real;
    sDCaption: string;
  public
    iQuestion, iQuestions: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUP_2 = #$C2#$B2;

var
  fPulleys4: TfPulleys4;

implementation

{$R *.lfm}

{************}
{ TfPulleys4 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys4.FormCreate(Sender: TObject);

begin
  laM1.Caption := StringReplace(laM1.Caption, '1', SUB_1, []);
  laM2.Caption := StringReplace(laM2.Caption, '2', SUB_2, []);
  laA1.Caption := StringReplace(laA1.Caption, '1', SUB_1, []);
  laA2.Caption := StringReplace(laA2.Caption, '2', SUB_2, []);
  laUA1.Caption := StringReplace(laUA1.Caption, '2', SUP_2, []);
  laUA2.Caption := StringReplace(laUA2.Caption, '2', SUP_2, []);
end;

{ Window show-up: Generate the exercise }

procedure TfPulleys4.FormActivate(Sender: TObject);

var
  MoveTime: Real;

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Two vertical pulleys (case 2).';
  edM1.Text := ''; edM2.Text := ''; edT.Text := ''; edA1.Text := ''; edA2.Text := ''; edS2.Text := ''; edS1.Text := '';
  imEval.Visible := False;
  repeat
    rM1 := Random(17) / 2 + 2; rM2 := Random(17) / 2 + 2;
    MoveTime := (Random(50) + 1) / 10;
    rA1 := (rG * (rM1 - 2 * rM2)) / (rM1 + 4 * rM2); rA2 := 2 * rA1;
    rT := (3 * rG * rM1 * rM2) / (rM1 + 4 * rM2);
    rS2 := 0.5 * rA2 * Sqr(MoveTime); rS1 := rS2 / 2;
  until (rM1 >= 2.5 * rM2) and (rM1 <= 5 * rM2) and (rS2 >= 0.5) and (rS2 <= 7.5);
  edM1.Text := FloatToStr(rM1); edM2.Text := FloatToStr(rM2);
  sDCaption := laD.Caption;
  laD.Caption := StringReplace(laD.Caption, '1 s', FloatToStr(MoveTime) + ' s', []);
  edA1.SetFocus;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys4.btAnswerClick(Sender: TObject);

var
  T, A1, A2, S1, S2: Real;

begin
  if edT.Text = '' then
    T := 0
  else
    T := StrToFloat(edT.Text);
  if edA1.Text = '' then
    A1 := 0
  else
    A1 := StrToFloat(edA1.Text);
  if edA2.Text = '' then
    A2 := 0
  else
    A2 := StrToFloat(edA2.Text);
  if edS1.Text = '' then
    S1 := 0
  else
    S1 := StrToFloat(edS1.Text);
  if edS2.Text = '' then
    S2 := 0
  else
    S2 := StrToFloat(edS2.Text);
  if (Abs(T - rT) < 0.001) and (Abs(A1 - rA1) < 0.001) and (Abs(A2 - rA2) < 0.001) and (Abs(S1 - rS1) < 0.001) and (Abs(S2 - rS2) < 0.001) then begin
    // Correct answer
    imEval.Picture.LoadFromFile('correct.png');
    bCorrect := True;
  end
  else begin
    // False answer
    imEval.Picture.LoadFromFile('false.png');
  end;
  imEval.Visible := True;
  btAnswer.Enabled := False;
  if not bCorrect then
    btShow.Enabled := True;                                                    // give user possibility to view answer
  btClose.SetFocus;
end;

{ Button "Show": Display exercise answer }

procedure TfPulleys4.btShowClick(Sender: TObject);

begin
  edA1.Text := FloatToStrF(rA1, ffFixed, 0, 3); edA2.Text:=FloatToStrF(rA2, ffFixed, 0, 3);
  edT.Text := FloatToStrF(rT, ffFixed, 0, 3);
  edS1.Text := FloatToStrF(rS1, ffFixed, 0, 3); edS2.Text:=FloatToStrF(rS2, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window }

procedure TfPulleys4.btCloseClick(Sender: TObject);

begin
  laD.Caption := sDCaption;
  Close;
end;

end.

