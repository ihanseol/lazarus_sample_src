{******************************************************************}
{* "Two horizontal pulleys (case 2)" unit for Pulleys application *}
{******************************************************************}

unit pulley9;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys9 }
  {************}
  TfPulleys9 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    laQuestion: TLabel;
    Label1, Label2, Label9, Label10, Label11, Label12: TLabel;
    Image1: TImage;
    laM1, laM2, laMu, laA1, laA2, laT1, laT2: TLabel;
    laUA1, laUA2: TLabel;
    edM1, edM2, edMu, edFK, edA1, edA2, edT1, edT2: TEdit;
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
    rM1, rM2, rMu, rFK, rT1, rT2, rA1, rA2: Real;
  public
    iQuestion, iQuestions: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUP_2 = #$C2#$B2;

var
  fPulleys9: TfPulleys9;

implementation

{$R *.lfm}

{************}
{ TfPulleys9 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys9.FormCreate(Sender: TObject);

begin
  laM1.Caption := StringReplace(laM1.Caption, '1', SUB_1, []);
  laM2.Caption := StringReplace(laM2.Caption, '2', SUB_2, []);
  laT1.Caption := StringReplace(laT1.Caption, '1', SUB_1, []);
  laT2.Caption := StringReplace(laT2.Caption, '2', SUB_2, []);
  laA1.Caption := StringReplace(laA1.Caption, '1', SUB_1, []);
  laA2.Caption := StringReplace(laA2.Caption, '2', SUB_2, []);
  laUA1.Caption := StringReplace(laUA1.Caption, '2', SUP_2, []);
  laUA2.Caption := StringReplace(laUA2.Caption, '2', SUP_2, []);
end;

{ Window show-up: Generate the exercise }

procedure TfPulleys9.FormActivate(Sender: TObject);

var
  FN: Real;

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Two horizontal pulleys (case 2).';
  edFK.Text := '';
  edT1.Text := ''; edT2.Text := '';
  edA1.Text := ''; edA2.Text := '';
  imEval.Visible := False;
  repeat
    rM1 := Random(17) / 2 + 2; rM2 := Random(29) / 2 + 2;
    rMu := (Random(48) + 3) / 100; edMu.Text := FloatToStr(rMu);
    FN := rM1 * rG; rFK := rMu * FN;
    rA1 := (2 * (rM2 * rG - 2 * rFK)) / (rM2 + 4 * rM1); rA2 := rA1 / 2;
  until (rM2 >= 1.5 * rM1) and (rA1 <= 7.5);
  edM1.Text := FloatToStr(rM1); edM2.Text := FloatToStr(rM2);
  rT1 := rM1 * rA1 + rFK; rT2 := 2 * rT1;
  edFK.SetFocus;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys9.btAnswerClick(Sender: TObject);

var
  FK, T1, T2, A1, A2: Real;

begin
  if edFK.Text = '' then
    FK := 0
  else
    FK := StrToFloat(edFK.Text);
  if edT1.Text = '' then
    T1 := 0
  else
    T1 := StrToFloat(edT1.Text);
  if edT2.Text = '' then
    T2 := 0
  else
    T2 := StrToFloat(edT2.Text);
  if edA1.Text = '' then
    A1 := 0
  else
    A1 := StrToFloat(edA1.Text);
  if edA2.Text = '' then
    A2 := 0
  else
    A2 := StrToFloat(edA2.Text);
  if (Abs(FK - rFK) < 0.001) and (Abs(T1 - rT1) < 0.001) and (Abs(T2 - rT2) < 0.001) and (Abs(A1 - rA1) < 0.001) and (Abs(A2 - rA2) < 0.001) then begin
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

procedure TfPulleys9.btShowClick(Sender: TObject);

begin
  edFK.Text := FloatToStrF(rFK, ffFixed, 0, 3);
  edA1.Text := FloatToStrF(rA1, ffFixed, 0, 3); edA2.Text:=FloatToStrF(rA2, ffFixed, 0, 3);
  edT1.Text := FloatToStrF(rT1, ffFixed, 0, 3); edT2.Text:=FloatToStrF(rT2, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window }

procedure TfPulleys9.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

