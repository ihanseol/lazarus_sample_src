{******************************************************}
{* "Three blocks pulley" unit for Pulleys application *}
{******************************************************}

unit pulley3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys3 }
  {************}
  TfPulleys3 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    laQuestion: TLabel;
    Label6, Label7, Label8, Label9, Label10, Label12, Label13, Label14, Label15: TLabel;
    Image1: TImage;
    laM1, laM2, laM3, laT1, laT2, laT3, laUA: TLabel;
    edM1, edM2, edM3, edA, edT1, edT2, edT3: TEdit;
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
    rM1, rM2, rM3, rT1, rT2, rT3, rA: Real;
  public
    iQuestion, iQuestions: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUB_3 = #$E2#$82#$83; SUP_2 = #$C2#$B2;

var
  fPulleys3: TfPulleys3;

implementation

{$R *.lfm}

{************}
{ TfPulleys3 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys3.FormCreate(Sender: TObject);

begin
  laM1.Caption := StringReplace(laM1.Caption, '1', SUB_1, []);
  laM2.Caption := StringReplace(laM2.Caption, '2', SUB_2, []);
  laM3.Caption := StringReplace(laM3.Caption, '3', SUB_3, []);
  laT1.Caption := StringReplace(laT1.Caption, '1', SUB_1, []);
  laT2.Caption := StringReplace(laT2.Caption, '2', SUB_2, []);
  laT3.Caption := StringReplace(laT3.Caption, '3', SUB_3, []);
  laUA.Caption := StringReplace(laUA.Caption, '2', SUP_2, []);
end;

{ Window show-up: Generate exercise }

procedure TfPulleys3.FormActivate(Sender: TObject);

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Three blocks pulley.';
  edM1.Text := ''; edM2.Text := ''; edM3.Text := ''; edT1.Text := ''; edT2.Text := ''; edT3.Text := ''; edA.Text := '';
  imEval.Visible := False;
  repeat
    rM1 := Random(17) / 2 + 2; rM2 := Random(17) / 2 + 2; rM3 := Random(17) / 2 + 2;
  until (rM1 < rM2) and (rM2 < rM3) and (rM2 + rM3 - rM1 >= 4);
  edM1.Text := FloatToStr(rM1); edM2.Text := FloatToStr(rM2); edM3.Text := FloatToStr(rM3);
  rA := (rG * (rM2 + rM3 - rM1)) / (rM1 + rM2 + rM3);
  rT1 := rM1 * rG + rM1 * rA;
  rT2 := rT1;
  rT3 := rM3 * rG - rM3 * rA;
  edA.SetFocus;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys3.btAnswerClick(Sender: TObject);

var
  T1, T2, T3, A: Real;

begin
  if edT1.Text = '' then
    T1 := 0
  else
    T1 := StrToFloat(edT1.Text);
  if edT2.Text = '' then
    T2 := 0
  else
    T2 := StrToFloat(edT2.Text);
  if edT3.Text = '' then
    T3 := 0
  else
    T3 := StrToFloat(edT3.Text);
  if edA.Text = '' then
    A := 0
  else
    A := StrToFloat(edA.Text);
  if (Abs(T1 - rT1) < 0.001) and (Abs(T2 - rT2) < 0.001) and (Abs(T3 - rT3) < 0.001) and (Abs(A - rA) < 0.001) then begin
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

procedure TfPulleys3.btShowClick(Sender: TObject);

begin
  edA.Text := FloatToStrF(rA, ffFixed, 0, 3);
  edT1.Text := FloatToStrF(rT1, ffFixed, 0, 3); edT2.Text:=FloatToStrF(rT2, ffFixed, 0, 3); edT3.Text:=FloatToStrF(rT3, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window }

procedure TfPulleys3.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

