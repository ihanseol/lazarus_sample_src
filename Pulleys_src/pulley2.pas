{***************************************************}
{* "Atwood's Machine" unit for Pulleys application *}
{***************************************************}

unit pulley2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys2 }
  {************}
  TfPulleys2 = class(TForm)
    StaticText1: TStaticText;
    laQuestion: TLabel;
    Label1, Label2, Label10, Label11: TLabel;
    Image1: TImage;
    laM1, laM2, laA1, laA2, laT1, laT2, laMp, laR: TLabel;
    laUA1, laUA2, laUMp, laUR: TLabel;
    edM1, edM2, edA1, edA2, edT1, edT2, edMp, edR: TEdit;
    stPulley: TStaticText;
    btAnswer: TButton;
    btShow: TButton;
    btClose: TButton;
    imEval: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    rM1, rM2, rMp, rT1, rT2, rA1, rA2: Real;
  public
    iQuestion, iQuestions, iCase: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUP_2 = #$C2#$B2;

var
  fPulleys2: TfPulleys2;

implementation

{$R *.lfm}

{************}
{ TfPulleys2 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys2.FormCreate(Sender: TObject);

begin
  laM1.Caption := StringReplace(laM1.Caption, '1', SUB_1, []);
  laT1.Caption := StringReplace(laT1.Caption, '1', SUB_1, []);
  laA1.Caption := StringReplace(laA1.Caption, '1', SUB_1, []);
  laM2.Caption := StringReplace(laM2.Caption, '2', SUB_2, []);
  laT2.Caption := StringReplace(laT2.Caption, '2', SUB_2, []);
  laA2.Caption := StringReplace(laA2.Caption, '2', SUB_2, []);
  laUA1.Caption := StringReplace(laUA1.Caption, '2', SUP_2, []);
  laUA2.Caption := StringReplace(laUA2.Caption, '2', SUP_2, []);
end;

{ Window show-up: Generate the exercise }

procedure TfPulleys2.FormActivate(Sender: TObject);

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Atwood''s Machine.';
  edM1.Text := ''; edM2.Text := ''; edT1.Text := ''; edT2.Text := ''; edA1.Text := ''; edA2.Text := '';
  edMp.Text := ''; edR.Text := '';
  laR.Visible := False; edR.Visible := False;
  laMp.Visible := False; edMp.Visible := False;
  laUMp.Visible := False; laUR.Visible := False;
  case iCase of
    // 3 kinds of exercise, corr. to 3 different selections in main window "Options" menu
    0: begin
         stPulley.Caption := 'Atwood''s Machine ideal case: Frictionless and neglecting pulley mass.';
       end;
    1: begin
         stPulley.Caption := 'Atwood''s  Machine: Frictionless, but considering pulley mass.';
         laMp.Visible := True; edMp.Visible := True; laUMp.Visible := True;
       end;
    2: begin
         stPulley.Caption := 'Atwood''s  Machine: Considering bearing  friction and pulley mass.';
         laMp.Visible := True; edMp.Visible := True; laUMp.Visible := True;
         laR.Visible := True; edR.Visible := True; laUR.Visible := True;
       end;
  end;
  imEval.Visible := False;
  repeat
    rM1 := Random(17) / 2 + 2; rM2 := Random(17) / 2 + 2;
  until rM1 - rM2 >= 2;
  edM1.Text := FloatToStr(rM1); edM2.Text := FloatToStr(rM2);
  case iCase of
    0: begin
         // Ideal Atwood machine
         rA1 := (rG * (rM1 - rM2)) / (rM1 + rM2); rA2 := rA1;
         rT1 := rM1 * rG - rM1 * rA1; rT2 := rT1;
       end;
    1: begin
         // Atwood machine with pulley mass considered
         rMp := (Random(17) / 2 + 2) / 5;
         edMp.Text := FloatToStr(rMp);
         rA1 := (rG * (rM1 - rM2)) / (rM1 + rM2 + rMp / 2); rA2 := rA1; rA2 := rA1;
         rT1 := rM1 * rG - rM1 * rA1;
         rT2 := rM2 * rG + rM2 * rA2;
       end;
    2: begin
         // Atwood machine with bearing friction and pulley mass considered
         // Not yet implemented...
       end;
  end;
  edA1.SetFocus;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys2.btAnswerClick(Sender: TObject);

var
  T1, T2, A1, A2: Real;

begin
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
  if (Abs(T1 - rT1) < 0.001) and (Abs(T2 - rT2) < 0.001) and (Abs(A1 - rA1) < 0.001) and (Abs(A2 - rA2) < 0.001) then begin
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

procedure TfPulleys2.btShowClick(Sender: TObject);

begin
  edA1.Text := FloatToStrF(rA1, ffFixed, 0, 3); edA2.Text:=FloatToStrF(rA2, ffFixed, 0, 3);
  edT1.Text := FloatToStrF(rT1, ffFixed, 0, 3); edT2.Text:=FloatToStrF(rT2, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close window }

procedure TfPulleys2.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

