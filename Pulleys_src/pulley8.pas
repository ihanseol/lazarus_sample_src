{**************************************************}
{* "Inclined pulley" unit for Pulleys application *}
{**************************************************}

unit pulley8;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys8 }
  {************}
  TfPulleys8 = class(TForm)
    StaticText1: TStaticText;
    laQuestion: TLabel;
    Label8, Label9, Label10, Label11, Label12, Label13: TLabel;
    Image1: TImage;
    laM1, laM2, laMu, laA1, laA2, laT1, laT2: TLabel;
    laUA1, laUA2: TLabel;
    edM1, edM2, edTheta, edMu: TEdit;
    edA1, edA2, edT1, edT2: TEdit;
    stPulley: TStaticText;
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
    rM1, rM2, rTheta, rMu, rT1, rT2, rA1, rA2: Real;
  public
    iQuestion, iQuestions, iCase: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUP_2 = #$C2#$B2;

var
  fPulleys8: TfPulleys8;

implementation

{$R *.lfm}

{************}
{ TfPulleys8 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys8.FormCreate(Sender: TObject);

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

procedure TfPulleys8.FormActivate(Sender: TObject);

var
  S: string;

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Inclined pulley.';
  S := 'Mass- and frictionless pulley.  ';
  // 2 kinds of exercises (selections in the main window's "Options" menu )
  if iCase = 0 then begin
    S := 'Mass- and frictionless pulley. ';
    S += 'The friction between block 1 and the surface is neglected.';
    laMu.Visible := False; edMu.Visible := False;
  end
  else begin
    S := 'Mass- and frictionless pulley. ';
    S += 'The friction between block 1 and the surface is NOT negligible.';
    laMu.Visible := True; edMu.Visible := True;
  end;
  stPulley.Caption := S;
  edM1.Text := ''; edM2.Text := ''; edTheta.Text := ''; edMu.Text := '';
  edT1.Text := ''; edT2.Text := ''; edA1.Text := ''; edA2.Text := '';
  imEval.Visible := False;
  repeat
    rM1 := Random(17) / 2 + 2; rM2 := Random(29) / 2 + 2;
  until (rM2 >= 1.5 * rM1) and (rM2 <= 4 * rM1);
  edM1.Text := FloatToStr(rM1); edM2.Text := FloatToStr(rM2);
  rTheta := Random(31) + 10; edTheta.Text := FloatToStr(rTheta);;
  if iCase = 0 then begin
    // Simple case
    rA2 := (rM2 * rG - rM1 * rG * Sin(2 * Pi * rTheta / 360)) / (rM1 + rM2); rA1 := rA2;
    rT2 := rM2 * rG - rM2 * rA2; rT1 := rT2;
  end
  else begin
    // Case with kinetic friction
    rMu := (Random(48) + 3) / 100; edMu.Text := FloatToStr(rMu);
    rA2 := rG * (rM2 - rM1 * Sin(2 * Pi * rTheta / 360) - rM1 * Cos(2 * Pi * rTheta / 360) * rMu) / (rM1 + rM2); rA1 := rA2;
    rT2 := rM2 * rG - rM2 * rA2; rT1 := rT2;
  end;
  edA1.SetFocus;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys8.btAnswerClick(Sender: TObject);

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

procedure TfPulleys8.btShowClick(Sender: TObject);

begin
  edA1.Text := FloatToStrF(rA1, ffFixed, 0, 3); edA2.Text:=FloatToStrF(rA2, ffFixed, 0, 3);
  edT1.Text := FloatToStrF(rT1, ffFixed, 0, 3); edT2.Text:=FloatToStrF(rT2, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window }

procedure TfPulleys8.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

