{*************************************************************}
{* "Block without acceleration" unit for Pulleys application *}
{*************************************************************}

unit pulley1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys1 }
  {************}
  TfPulleys1 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    laQuestion: TLabel;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    Image1: TImage;
    edM, edTa, edTb: TEdit;
    imEval: TImage;
    btAnswer: TButton;
    btShow: TButton;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    rM, rTa, rTb: Real;
  public
    iQuestion, iQuestions: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

var
  fPulleys1: TfPulleys1;

implementation

{$R *.lfm}

{************}
{ TfPulleys1 }
{************}

{ Window show-up: Generate the exercise }

procedure TfPulleys1.FormActivate(Sender: TObject);

var
  R: Integer;

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Block without acceleration.';
  edM.Text := ''; edTa.Text := ''; edTb.Text := '';
  edM.ReadOnly  := False; edM.TabStop  := True; edM.Color := clDefault;
  edTa.ReadOnly := False; edTa.TabStop := True; edTa.Color := clDefault;
  edTb.ReadOnly := False; edTb.TabStop := True; edTb.Color := clDefault;
  imEval.Visible := False;
  rM  := Random(19) / 2 + 1;
  rTa := (rM * rG) / 2;
  rTb := rM * rG;
  // 3 exercise types: M, Ta or Tb given
  R := Random(5);
  if R = 0 then begin
    edTa.Text := FloatToStr(rTa); edTa.ReadOnly := True; edTa.TabStop := False; edTa.Color := clCream; edM.SetFocus;
  end
  else if R = 1 then begin
    edTb.Text := FloatToStr(rTb); edTb.ReadOnly := True; edTb.TabStop := False; edTb.Color := clCream; edM.SetFocus;
  end
  else begin
    edM.Text := FloatToStr(rM); edM.ReadOnly := True; edM.TabStop := False; edM.Color := clCream; edTa.SetFocus;
  end;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys1.btAnswerClick(Sender: TObject);

var
  M, Ta, Tb: Real;

begin
  if edM.Text = '' then
    M := 0
  else
    M := StrToFloat(edM.Text);
  if edTa.Text = '' then
    Ta := 0
  else
    Ta := StrToFloat(edTa.Text);
  if edTb.Text = '' then
    Tb := 0
  else
    Tb := StrToFloat(edTb.Text);
  if (Abs(M - rM) < 0.001) and (Abs(Ta - rTa) < 0.001) and (Abs(Tb - rTb) < 0.001) then begin
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

procedure TfPulleys1.btShowClick(Sender: TObject);

begin
  edM.Text  := FloatToStrF(rM, ffFixed, 0, 3);
  edTa.Text := FloatToStrF(rTa, ffFixed, 0, 3);
  edTb.Text := FloatToStrF(rTb, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window (return to main window) }

procedure TfPulleys1.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

