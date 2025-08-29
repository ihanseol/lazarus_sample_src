{****************************************************************}
{* "Two vertical pulleys (case 1)" unit for Pulleys application *}
{****************************************************************}

unit pulley5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {************}
  { TfPulleys5 }
  {************}
  TfPulleys5 = class(TForm)
    StaticText1, StaticText2: TStaticText;
    laQuestion: TLabel;
    Label1, Label2, Label6, Label7, Label8, Label9, Label10, Label11, Label12: TLabel;
    Image1: TImage;
    laT1, laT2, laT3, laT4, laT5: TLabel;
    edM, edT1, edT2, edT3, edT4, edT5, edF: TEdit;
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
    rM, rT1, rT2, rT3, rT4, rT5, rF: Real;
  public
    iQuestion, iQuestions: Integer;
    rG: Real;
    bCorrect: Boolean;
  end;

const
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUB_3 = #$E2#$82#$83; SUB_4 = #$E2#$82#$84; SUB_5 = #$E2#$82#$85;

var
  fPulleys5: TfPulleys5;

implementation

{$R *.lfm}

{************}
{ TfPulleys5 }
{************}

{ Application start: Apply sub- and superscripts }

procedure TfPulleys5.FormCreate(Sender: TObject);

begin
  laT1.Caption := StringReplace(laT1.Caption, '1', SUB_1, []);
  laT2.Caption := StringReplace(laT2.Caption, '2', SUB_2, []);
  laT3.Caption := StringReplace(laT3.Caption, '3', SUB_3, []);
  laT4.Caption := StringReplace(laT4.Caption, '4', SUB_4, []);
  laT5.Caption := StringReplace(laT5.Caption, '5', SUB_5, []);
end;

{ Window-showup: Generate exercise }

procedure TfPulleys5.FormActivate(Sender: TObject);

var
  R: Integer;

begin
  laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions) + ': Two vertical pulleys (case 1).';
  edM.ReadOnly := False; edM.TabStop := True; edM.Color := clDefault;
  edT1.ReadOnly := False; edT1.TabStop := True; edT1.Color := clDefault;
  edT2.ReadOnly := False; edT2.TabStop := True; edT2.Color := clDefault;
  edT3.ReadOnly := False; edT3.TabStop := True; edT3.Color := clDefault;
  edM.Text := ''; edT1.Text := ''; edT2.Text := ''; edT3.Text := ''; edT4.Text := ''; edT5.Text := ''; edF.Text := '';
  imEval.Visible := False;
  // 4 types of exercise: M, T1, T2 or T3 given
  R := Random(6);
  if R in [3..5] then begin
    rM := Random(17) / 2 + 2; edM.Text := FloatToStr(rM);
    edM.ReadOnly := True; edM.TabStop := False; edM.Color := clCream;
    rT1 := rM * rG; rT2 := rT1 / 2; rT3 := rT2;
    edT1.SetFocus;
  end
  else begin
    rT1 := Random(161) / 2 + 20;
    rM := rT1 / rG; rT2 := rT1 / 2; rT3 := rT2;
    if R = 0 then begin
      edT1.Text := FloatToStr(rT1);
      edT1.ReadOnly := True; edT1.TabStop := False; edT1.Color := clCream;
    end
    else if R = 1 then begin
      edT2.Text := FloatToStr(rT2);
      edT2.ReadOnly := True; edT2.TabStop := False; edT2.Color := clCream;
    end
    else begin
      edT3.Text := FloatToStr(rT3);
      edT3.ReadOnly := True; edT3.TabStop := False; edT3.Color := clCream;
    end;
    edM.SetFocus;
  end;
  rT5 := rT2; rT4 := rT2 + rT3 + rT5; rF := rT5;
  bCorrect := False;
  btAnswer.Enabled := True; btShow.Enabled := False;
end;

{ Button "Answer": Check user answer }

procedure TfPulleys5.btAnswerClick(Sender: TObject);

var
  M, T1, T2, T3, T4, T5, F: Real;

begin
  if edM.Text = '' then
    M := 0
  else
    M := StrToFloat(edM.Text);
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
  if edT4.Text = '' then
    T4 := 0
  else
    T4 := StrToFloat(edT4.Text);
  if edT5.Text = '' then
    T5 := 0
  else
    T5 := StrToFloat(edT5.Text);
  if edF.Text = '' then
    F := 0
  else
    F := StrToFloat(edF.Text);
  if (Abs(M - rM) < 0.001) and (Abs(T1 - rT1) < 0.001) and (Abs(T2 - rT2) < 0.001) and (Abs(T3 - rT3) < 0.001) and
     (Abs(T4 - rT4) < 0.001) and (Abs(T5 - rT5) < 0.001) and (Abs(F - rF) < 0.001) then begin
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
    btShow.Enabled := True;                                                    // give user possibility to view correct answer
  btClose.SetFocus;
end;

{ Button "Show": Display exercise answer }

procedure TfPulleys5.btShowClick(Sender: TObject);

begin
  edM.Text  := FloatToStrF(rM, ffFixed, 0, 3);
  edT1.Text := FloatToStrF(rT1, ffFixed, 0, 3); edT2.Text:=FloatToStrF(rT2, ffFixed, 0, 3);
  edT3.Text := FloatToStrF(rT3, ffFixed, 0, 3); edT4.Text:=FloatToStrF(rT4, ffFixed, 0, 3);
  edT5.Text := FloatToStrF(rT5, ffFixed, 0, 3); edF.Text:=FloatToStrF(rF, ffFixed, 0, 3);
  btShow.Enabled := False;
  btClose.SetFocus;
end;

{ Button "Close": Close the window }

procedure TfPulleys5.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

