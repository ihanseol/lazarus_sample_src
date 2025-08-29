{****************************************************************}
{* "Horizontal pulleys (3 masses)" unit for Pulleys application *}
{****************************************************************}

unit pulley7;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys7 }
  {************}
  TfPulleys7 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    laQuestion: TLabel;
    Label8, Label9, Label10, Label12, Label13: TLabel;
    Image1: TImage;
    laM1, laM2, laM3, laA1, laA2, laA3, laT1, laT3: TLabel;
    laUA1, laUA2, laUA3: TLabel;
    edM1, edM2, edM3, edA1, edA2, edA3, edT1, edT3: TEdit;
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
    rM1, rM2, rM3, rT1, rT3, rA1, rA2, rA3: Real;
  public
    iQuestion, iQuestions: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUB_3 = #$E2#$82#$83; SUP_2 = #$C2#$B2;

var
  fPulleys7: TfPulleys7;

implementation

{$R *.lfm}

{************}
{ TfPulleys7 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys7.FormCreate(Sender: TObject);

begin
  laM1.Caption := StringReplace(laM1.Caption, '1', SUB_1, []);
  laM2.Caption := StringReplace(laM2.Caption, '2', SUB_2, []);
  laM3.Caption := StringReplace(laM3.Caption, '3', SUB_3, []);
  laT1.Caption := StringReplace(laT1.Caption, '1', SUB_1, []);
  laT3.Caption := StringReplace(laT3.Caption, '3', SUB_3, []);
  laA1.Caption := StringReplace(laA1.Caption, '1', SUB_1, []);
  laA2.Caption := StringReplace(laA2.Caption, '2', SUB_2, []);
  laA3.Caption := StringReplace(laA3.Caption, '3', SUB_3, []);
  laUA1.Caption := StringReplace(laUA1.Caption, '2', SUP_2, []);
  laUA2.Caption := StringReplace(laUA2.Caption, '2', SUP_2, []);
  laUA3.Caption := StringReplace(laUA3.Caption, '2', SUP_2, []);
end;

{ Window show-up: Generate the exercise }

procedure TfPulleys7.FormActivate(Sender: TObject);

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Horizontal pulley (three blocks).';
  edM1.Text := ''; edM2.Text := ''; edM3.Text := ''; edT1.Text := ''; edT3.Text := ''; edA1.Text := ''; edA2.Text := ''; edA3.Text := '';
  imEval.Visible := False;
  repeat
    rM1 := Random(17) / 2 + 2; rM2 := Random(17) / 2 + 2; rM3 := Random(17) / 2 + 2;
  until (rM2 - rM1 >= 1) and (rM3 - (rM1 + rM2) >= 1);
  edM1.Text := FloatToStr(rM1); edM2.Text := FloatToStr(rM2); edM3.Text := FloatToStr(rM3);
  rA1 := rG * (rM3 - rM1) / (rM1 + rM2 + rM3); rA2 := rA1; rA3 := rA1;
  rT1 := rM1 * rA1 + rM1 * rG; rT3 := rM2 * rA2 + rT1;
  edA1.SetFocus;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys7.btAnswerClick(Sender: TObject);

var
  A1, A2, A3, T1, T3: Real;

begin
  if edT1.Text = '' then
    T1 := 0
  else
    T1 := StrToFloat(edT1.Text);
  if edT3.Text = '' then
    T3 := 0
  else
    T3 := StrToFloat(edT3.Text);
  if edA1.Text = '' then
    A1 := 0
  else
    A1 := StrToFloat(edA1.Text);
  if edA2.Text = '' then
    A2 := 0
  else
    A2 := StrToFloat(edA2.Text);
  if edA3.Text = '' then
    A3 := 0
  else
    A3 := StrToFloat(edA3.Text);
  if (Abs(T1 - rT1) < 0.001) and (Abs(T3 - rT3) < 0.001) and (Abs(A1 - rA1) < 0.001) and (Abs(A2 - rA2) < 0.001) and (Abs(A3 - rA3) < 0.001) then begin
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

procedure TfPulleys7.btShowClick(Sender: TObject);

begin
  edA1.Text := FloatToStrF(rA1, ffFixed, 0, 3); edA2.Text:=FloatToStrF(rA2, ffFixed, 0, 3); edA3.Text:=FloatToStrF(rA3, ffFixed, 0, 3);
  edT1.Text := FloatToStrF(rT1, ffFixed, 0, 3); edT3.Text:=FloatToStrF(rT3, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window }

procedure TfPulleys7.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

