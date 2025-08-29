{**********************************************************}
{* List selection exercises unit for EngVerbs application *}
{**********************************************************}

unit vlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TTenses = record
    Infinitive, SimplePast, PastParticiple: string;
  end;
  TVerb = record
    Irregular: Boolean;
    VerForms: Integer;
    Tenses: array[0 .. 3] of TTenses;
    RegularForm, ConsonantDouble: Boolean;
  end;
  TVerbs = array of TVerb;
  {********}
  { TfList }
  {********}
  TfList = class(TForm)
    stTitle: TStaticText;
    lbVerbs: TListBox;
    lbAnswers: TListBox;
    rbReg: TRadioButton;
    rbIrreg: TRadioButton;
    laQuestion: TLabel;
    Label1: TLabel;
    laAnswers: TLabel;
    cbInclude: TCheckBox;
    btQuestion: TButton;
    btVerbAdd: TButton;
    btVerbRemove: TButton;
    btClose: TButton;
    imEval: TImage;
    procedure btQuestionClick(Sender: TObject);
    procedure btVerbAddClick(Sender: TObject);
    procedure btVerbRemoveClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure rbRegChange(Sender: TObject);
    procedure rbIrregChange(Sender: TObject);
  private
    iAnswers, iUserAnswers: Integer;
    aAnswers, aUserAnswers: array[1 .. 15] of string;
  public
    iVerbs, iQuestions, iList, iQuestion, iCorrect: Integer;
    bDoneQuestion, bDoneAll, bDoneTest: Boolean;
    aVerbs: TVerbs;
  end;

var
  fList: TfList;

implementation

{$R *.lfm}

{********}
{ TfList }
{********}

{ Button "Start/Question/Answer": Generate new verbs list resp. check user answer }

procedure TfList.btQuestionClick(Sender: TObject);

var
  V, I, J, IX: Integer;
  Correct, OK: Boolean;

begin
  // Button "Start/Question": Generate new verbs list
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Button "Start": Reset evaluation counters
    if btQuestion.Caption = 'Start' then begin
      iQuestion := 0; iCorrect := 0;
      rbReg.Enabled := False; rbIrreg.Enabled := False; cbInclude.Enabled := False;
    end;
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions);;
    for I := 1 to 15 do
      aAnswers[I] := '';
    lbVerbs.Clear; lbAnswers.Clear;
    imEval.Picture.Clear;
    iAnswers := 0;
    // Randomly choose list verbs
    for I := 1 to iList do begin
      repeat
        OK := True;
        V := Random(iVerbs);
        // Be sure verbs are used only once
        for J := 1 to I - 1 do begin
          if aVerbs[V].Tenses[1].Infinitive = lbVerbs.Items[J - 1] then
            OK := False;
        end;
        if OK then
          lbVerbs.Items.AddText(aVerbs[V].Tenses[1].Infinitive);
      until OK;
      // Selection = regular verbs
      if rbReg.Checked then begin
        // Inculde non-irregular verbs
        if not aVerbs[V].Irregular then begin
          Inc(iAnswers);
          aAnswers[iAnswers] := aVerbs[V].Tenses[1].Infinitive
        end
        else begin
          // Include irregular verbs with regular form if this is checked
          if cbInclude.Checked and aVerbs[V].RegularForm then begin
            Inc(iAnswers);
            aAnswers[iAnswers] := aVerbs[V].Tenses[1].Infinitive;
          end;
        end;
      end
      // Selection = irregular verbs
      else begin
        // Include irregular verbs
        if aVerbs[V].Irregular then begin
          // But include verbs with regular form only if this is checked
          if cbInclude.Checked or (not cbInclude.Checked and not aVerbs[V].RegularForm) then begin
            Inc(iAnswers);
            aAnswers[iAnswers] := aVerbs[V].Tenses[1].Infinitive;
          end;
        end;
      end;
    end;
    fList.btVerbAdd.Enabled := True; fList.btVerbRemove.Enabled := True;
    btQuestion.Caption := 'Answer';
  end
  // Button "Answer": Check user answer
  else begin
    iUserAnswers := lbAnswers.Items.Count;
    for I := 0 to iUserAnswers - 1 do
      aUserAnswers[I + 1] := lbAnswers.Items[I];
    Correct := True;
    // If the number of user selected verbs <> number of correct verbs, the answer is always false
    // Testing this makes sure that the answer is correct, if all verbs of the user list also exist in the answers list
    if iUserAnswers <> iAnswers then
      Correct := False
    // Check user selected verbs vs. correct answer verbs
    else begin
      OK := False;
      for I := 1 to iUserAnswers do begin
        for J := 1 to iAnswers do begin
          // If user verb is in answers list, the verb is correct
          if aUserAnswers[I] = aAnswers[J] then
            OK := True;
        end;
        // If one verb isn't correct, the answer is always false
        if not OK then
          Correct := False;
      end;
    end;
    // Correct answer
    if Correct then begin
      Inc(iCorrect);
      imEval.Picture.LoadFromFile('correct.jpg');
    end
    // False answer
    else begin
      imEval.Picture.LoadFromFile('false.jpg');
      // Display items not moved by the user (answer verbs being in the verbs box) in uppercase
      for I := 0 to lbVerbs.Items.Count - 1 do begin
        IX := -1;
        for J := 1 to iAnswers do begin
          if lbVerbs.Items[I] = aAnswers[J] then
            IX := I;
        end;
        if IX <> -1 then
          lbVerbs.Items[IX] := UpperCase(lbVerbs.Items[IX]);
      end;
      // Display items moved by the user (non-answer verbs being in the answers box) in uppercase
      for I := 0 to lbAnswers.Items.Count - 1 do begin
        IX := I;
        for J := 1 to iAnswers do begin
          if lbAnswers.Items[I] = aAnswers[J] then
            IX := -1;
        end;
        if IX <> -1 then
          lbAnswers.Items[IX] := UpperCase(lbAnswers.Items[IX]);
      end;
    end;
    fList.btVerbAdd.Enabled := False; fList.btVerbRemove.Enabled := False;
    // Setting the bDoneQuestion variable to True will update the evalation counters (timer routine on main form)
    bDoneQuestion := True;
    // All questions done
    if iQuestion = iQuestions then begin
      // Setting the bDoneTest variable to True will display the 'end of test' message (timer routine on main form)
      // Setting the bDoneAll variable to True will close this form (timer routine on main form)
      bDoneTest := True;
      bDoneAll := True;
    end
    // Question(s) remaining
    else begin
      btQuestion.Caption := 'Question';
      btQuestion.Enabled := True;
    end;
  end;
end;

{ Button "Close": Close the window }

procedure TfList.btCloseClick(Sender: TObject);

begin
  // Setting the bDoneAll variable to True will close this form (timer routine on main form)
  bDoneAll := True;
end;

{ Button "-->": Move selected verb to answers box }

procedure TfList.btVerbAddClick(Sender: TObject);

var
  IX: Integer;
  S: string;

begin
  IX := lbVerbs.ItemIndex;
  if IX <> -1 then begin
    // If there is a verb selected, move it
    S := lbVerbs.Items[IX];
    lbAnswers.Items.AddText(S);
    lbVerbs.Items.Delete(IX);
  end;
end;

{ Button "<--": Remove selected verb from answers box }

procedure TfList.btVerbRemoveClick(Sender: TObject);

var
  IX: Integer;
  S: string;

begin
  IX := lbAnswers.ItemIndex;
  if IX <> -1 then begin
    // If there is a verb selected, move it
    S := lbAnswers.Items[IX];
    lbVerbs.Items.AddText(S);
    lbAnswers.Items.Delete(IX);
  end;
end;

{ Change of "regular/irregular" radiobuttons status: Change label above answers box }

procedure TfList.rbRegChange(Sender: TObject);

begin
  if rbReg.Checked then
    laAnswers.Caption := 'Regular verb list:';
end;

procedure TfList.rbIrregChange(Sender: TObject);

begin
  if rbIrreg.Checked then
    laAnswers.Caption := 'Irregular verb list:';
end;

end.

