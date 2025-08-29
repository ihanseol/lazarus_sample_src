{****************************************}
{* Main unit for EinMalEins application *}
{****************************************}

unit einxeins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, PopupNotifier;

type
  {*********}
  {* Tf1x1 *}
  {*********}
  Tf1x1 = class(TForm)
    mMenu: TMainMenu;
    mFile, fFileNew, fFileExit: TMenuItem;
    mSettings, mSettings100, mSettingsNo0, mSettingsNo1: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    rbCalc: TRadioButton;
    rbEqu: TRadioButton;
    rbComp: TRadioButton;
    laQuestion: TLabel;
    edN1: TEdit;
    edMult: TEdit;
    edN2: TEdit;
    edOp: TEdit;
    edN3: TEdit;
    edOp2: TEdit;
    edN4: TEdit;
    edEval: TEdit;
    memoHelp: TMemo;
    edQuestions: TEdit;
    edCorrect: TEdit;
    edFalse: TEdit;
    edSuccess: TEdit;
    btQuestion: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure fFileNewClick(Sender: TObject);
    procedure fFileExitClick(Sender: TObject);
    procedure mSettings100Click(Sender: TObject);
    procedure mSettingsNo0Click(Sender: TObject);
    procedure mSettingsNo1Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbCalcChange(Sender: TObject);
    procedure rbEquChange(Sender: TObject);
    procedure rbCompChange(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure edQuestionsChange(Sender: TObject);
  private
    iQuestions, iSQuestions, iCorrect, iFalse, iN1, iN2, iN3, iN4: Integer;
    sExercise, sSExercise: string;
  end;

var
  f1x1: Tf1x1;

implementation

{$R *.lfm}

{ New 1 x 1 exercise preparations }

procedure NewExercise(SExercise: string; SQuestions: Integer; out Exercise: string; out Questions, QCorrect, QFalse: Integer);

var
  S: string;

begin
  Exercise := SExercise; Questions := SQuestions;
  f1x1.edEval.Text := ''; f1x1.laQuestion.Caption := 'Aufgab';
  f1x1.edN1.Text := ''; f1x1.edN4.Text := ''; f1x1.memoHelp.Clear;
  // Set fields content as appropriate for actual exercise type
  if Exercise = 'calc' then begin
    // Calculation exercise
    f1x1.edN2.Color := clForm; f1x1.edN2.TabStop := False; f1x1.edN2.ReadOnly := True; f1x1.edN2.Text := '';
    f1x1.edOp.Color := clForm; f1x1.edOp.TabStop := False; f1x1.edOp.ReadOnly := True; f1x1.edOp.Text := '=';
    f1x1.edN3.Color := clDefault; f1x1.edN3.TabStop := True; f1x1.edN3.ReadOnly := False; f1x1.edN3.Text := '';
    f1x1.edN4.Visible := False; f1x1.edOp2.Visible := False;
    S := 'Multiplizéier déi 2 Zuelen a gëff d''Resultat erann.';
  end
  else if Exercise = 'equ' then begin
    // Equation exercise
    f1x1.edN2.Color := clDefault; f1x1.edN2.TabStop := True; f1x1.edN2.ReadOnly := False; f1x1.edN2.Text := '';
    f1x1.edOp.Color := clForm; f1x1.edOp.TabStop := False; f1x1.edOp.ReadOnly := True; f1x1.edOp.Text := '=';
    f1x1.edN3.Color := clForm; f1x1.edN3.TabStop := False; f1x1.edN3.ReadOnly := True; f1x1.edN3.Text := '';
    f1x1.edN4.Visible := False; f1x1.edOp2.Visible := False;
    S := 'Gëff déi Zuel erann déi, wann ee se matt deer 1. Zuel multiplizéiert dat ugewisent Resultat ergëtt.'
  end
  else begin
    // Comparison exercise
    f1x1.edN2.Color := clForm; f1x1.edN2.TabStop := False; f1x1.edN2.ReadOnly := True; f1x1.edN2.Text := '';
    f1x1.edOp.Color := clDefault; f1x1.edOp.TabStop := True; f1x1.edOp.ReadOnly := False; f1x1.edOp.Text := '';
    f1x1.edN3.Color := clForm; f1x1.edN3.TabStop := False; f1x1.edN3.ReadOnly := True; f1x1.edN3.Text := '';
    f1x1.edN4.Visible := True; f1x1.edOp2.Visible := True;
    S := 'Multiplizéier déi 2 Zuelen op deen 2 Säite vun deem fräie Feld a gëff erann op dat éischt Resultat méi kleng (<) ';
    S += 'méi grouss (>) oder ''t selwecht (=) wéi dat zweet ass.';
  end;
  f1x1.memoHelp.Lines.AddText(S);
  f1x1.edCorrect.Text := ''; f1x1.edFalse.Text := ''; f1x1.edSuccess.Text := ''; f1x1.edSuccess.Color := clCream;
  QCorrect := 0; QFalse := 0;
  f1x1.btQuestion.Caption := 'Fro';
  f1x1.btQuestion.Enabled := True;
end;

{*********}
{* Tf1x1 *}
{*********}

{ Application start: Initialisation }

procedure Tf1x1.FormCreate(Sender: TObject);

begin
  iSQuestions := 10;
  sSExercise  := 'calc';
  NewExercise(sSExercise, iSQuestions, sExercise, iQuestions, iCorrect, iFalse);
  Randomize;
end;

{ Menu item "Fichier > Neien Test": Prepare for new exercise }

procedure Tf1x1.fFileNewClick(Sender: TObject);

begin
  NewExercise(sSExercise, iSQuestions, sExercise, iQuestions, iCorrect, iFalse);
end;

{ Menu item "Fichier > Verloossen": Exit application }

procedure Tf1x1.fFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Astellungen > Punkten op 100": Select maximum of 100 resp. 60 points }

procedure Tf1x1.mSettings100Click(Sender: TObject);

begin
  if mSettings100.Checked then
    mSettings100.Checked := False
  else
    mSettings100.Checked := True;
end;

{ Menu item "Astellungen > Keng 'x 0' Aufgaben": Select if '0' operand is disabled or not }

procedure Tf1x1.mSettingsNo0Click(Sender: TObject);

begin
  if mSettingsNo0.Checked then
    mSettingsNo0.Checked := False
  else
    mSettingsNo0.Checked := True;
end;

{ Menu item "Astellungen > Keng 'x 1' Aufgaben": Select if '1' operand is disabled or not }

procedure Tf1x1.mSettingsNo1Click(Sender: TObject);

begin
  if mSettingsNo1.Checked then
    mSettingsNo1.Checked := False
  else
    mSettingsNo1.Checked := True;
end;

{ Menu item "Hëllef > Iwwer"; Display program about}

procedure Tf1x1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'Klenge Rechentrainer-Programm matt deenen d''Primärschüler d''1x1 léieren oder verbessere kënnen.' + Chr(13) + Chr(13);
    S += 'Versioun 1.0, © allu, September 2018';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Fro/Äntwert": Generate new exercise resp. check user answer }

procedure Tf1x1.btQuestionClick(Sender: TObject);

const
  clOrange = $000080FF;

var
  Question, Max: Integer;
  Success: Real;
  Answer, UAnswer, SAnswer: string;
  OK: Boolean;

begin
  Question := iCorrect + iFalse + 1;
  // Generate new 1 x 1 exercise
  if btQuestion.Caption = 'Fro' then begin
    laQuestion.Caption := 'Aufgab ' + IntToStr(Question) + ' / ' + IntToStr(iQuestions);
    // Generate valid random operands
    //   - operands must not be 0 / 1, if settings tell so
    //   - for equation: exercise should not be 0 x ? = 0
    //   - for comparison: the 2 operations should be different
    //   - for comparison: the 2 results should not differ by more than 33%
    repeat
      OK := True; iN4 := -1;
      iN1 := Random(11); iN2 := Random(11);
      if mSettingsNo0.Checked and ((iN1 = 0) or (iN2 = 0)) then
        OK := False
      else if mSettingsNo1.Checked and ((iN1 = 1) or (iN2 = 1)) then
        OK := False;
      if OK then begin
        if (sExercise = 'calc') or (sExercise = 'equ') then
          iN3 := iN1 * iN2
        else begin
          iN3 := Random(11); iN4 := Random(11);
        end;
        if mSettingsNo0.Checked and ((iN3 = 0) or (iN4 = 0)) then
          OK := False
        else if mSettingsNo1.Checked and ((iN3 = 1) or (iN4 = 1)) then
          OK := False
        else if (iN1 = iN3) and (iN2 = iN4) then
          OK := False
        else if (sExercise = 'equ') and (iN1 = 0) and (iN3 = 0) then
          OK := False
        else if (sExercise = 'comp') and ((iN3 * iN4 < (2 / 3) * (iN1 * iN2)) or (iN3 * iN4 > (3 / 2) * (iN1 * iN2))) then
          OK := False;
      end;
    until OK;
    edN1.Text := IntToStr(iN1);
    // Fill values of actual exercise into form
    if sExercise = 'calc' then begin
      // Calculation exercise
      edN2.Text := IntToStr(iN2); edN2.TabStop := False; edN2.ReadOnly := True;
      edN3.Text := '';  edN3.TabStop := True; edN3.ReadOnly := False;
      edOp.TabStop := False;  edOp.ReadOnly := True;
      edN3.SetFocus;
    end
    else if sExercise = 'equ' then begin
      // Equation exercise
      edN2.Text := '';  edN2.TabStop := True; edN2.ReadOnly := False;
      edN3.Text := IntToStr(iN3);  edN3.TabStop := False; edN3.ReadOnly := True;
      edOp.TabStop := False;  edOp.ReadOnly := True;
      edN2.SetFocus;
    end
    else begin
      // Comparison exercsie
      edN2.Text := IntToStr(iN2); edN2.TabStop := False;  edN2.ReadOnly := True;
      edN3.Text := IntToStr(iN3); edN3.TabStop := False;  edN3.ReadOnly := True;
      edN4.Text := IntToStr(iN4);
      edOp.Text := ''; edOp.TabStop := True;  edOp.ReadOnly := False;
      edOp.SetFocus;
    end;
    btQuestion.Caption := 'Äntwert';
  end
  // Check user answer and do evaluation
  else begin
    // Calculate the exercise result (depending on type of exercise)
    if sExercise = 'calc' then begin
      // Calculation exercise
      Answer := IntToStr(iN3); SAnswer := Answer;
      UAnswer := edN3.Text;
    end
    else if sExercise = 'equ' then begin
      // Equation exercise
      Answer := IntToStr(iN2); SAnswer := Answer;
        UAnswer := edN2.Text;
    end
    else begin
      // Comparison exercise
      if iN1 * iN2 < iN3 * iN4 then begin
        Answer := '<';
        SAnswer := 'méi kleng (<)';
      end
      else if iN1 * iN2 > iN3 * iN4 then begin
        Answer := '>';
        SAnswer := 'méi grouss (>)';
      end
      else begin
        Answer := '=';
        SAnswer := 'gläich (=)';
      end;
      UAnswer := edOp.Text
    end;
    // Correct user answer
    if Answer = UAnswer then begin
      Inc(iCorrect);
      edEval.Text := 'Richteg!';
      edEval.Font.Color := clDefault;
    end
    // False user answer
    else begin
      Inc(iFalse);
      edEval.Text := 'Falsch! Richteg: ' + SAnswer;
      edEval.Font.Color := clRed;
    end;
    // Update evaluation table (with success as 'points from max = 60 (100)')
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iFalse);
    if mSettings100.Checked then
      Max := 100
    else
      Max := 60;
    Success := Max * (iCorrect / Question);
    edSuccess.Text := IntToStr(Round(Success));
    if Success >= 0.6 * Max then
      edSuccess.Color := clLime
    else if Success >= 0.5 * Max then
      edSuccess.Color := clYellow
    else if Success >= 0.4 * Max then
      edSuccess.Color := clOrange
    else
      edSuccess.Color := clRed;
    btQuestion.Caption := 'Fro';
    // If all questions have been done, display message that exercise is over
    if Question = iQuestions then begin
      MessageDlg('Enn vun dësem Exercice', 'Benotz w.e.g. de Menü "Fichier" fir een aneren ze maachen.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Exercise type changement: Calculation }

procedure Tf1x1.rbCalcChange(Sender: TObject);

begin
  if rbCalc.Checked then
    sSExercise := 'calc';
end;

{ Exercise type changement: Equation }

procedure Tf1x1.rbEquChange(Sender: TObject);

begin
  if rbEqu.Checked then
    sSExercise := 'equ';
end;

{ Exercise type changement: Comparison }

procedure Tf1x1.rbCompChange(Sender: TObject);

begin
  if rbComp.Checked then
    sSExercise := 'comp';
end;

{ Number of exercise questions changement }

procedure Tf1x1.edQuestionsChange(Sender: TObject);

begin
  if edQuestions.Text = '' then
    iSQuestions := 0
  else
    iSQuestions := StrToInt(edQuestions.Text);
end;

end.

