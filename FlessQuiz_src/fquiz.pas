{***************************************}
{* Main unit for FlessQuiz application *}
{***************************************}

unit fquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls;

const
    NQuizes    = 5;
    NCountries = 4;
    NPlaces    = 28;
    NRivers    = 17;
    NRiversLux = 15;

type
  TQuizes = array[1..NQuizes] of string;
  TCountries = array[1..NCountries] of string;
  TPlaces = array[1..NPlaces] of string;
  TRivers = array[1..NRivers] of string;
  TRiversDone = array[1..NRiversLux] of Boolean;
  TLuxRivers = array[1..NRivers] of record
    River, Source, SrcCountry, Destination, DestCountry, DestRiver: string;
  end;
  { TfFQuiz }
  TfFQuiz = class(TForm)
    mFQuiz: TMainMenu;
    mQuiz: TMenuItem;
    mQuizNew: TMenuItem;
    mQuizExit: TMenuItem;
    mQuestions: TMenuItem;
    mQuestions1: TMenuItem;
    mQuestions2: TMenuItem;
    mQuestions3: TMenuItem;
    mQuestions4: TMenuItem;
    mQuestions5: TMenuItem;
    mAnswers: TMenuItem;
    mAnswers1: TMenuItem;
    mAnswers2: TMenuItem;
    Title: TStaticText;
    Image1: TImage;
    Label1: TLabel;
    edRiver: TEdit;
    edQuiz: TLabel;
    cbAnswer: TComboBox;
    edEval: TEdit;
    Label2: TLabel;
    edQuestion: TEdit;
    Label3: TLabel;
    edCorrect: TEdit;
    Label4: TLabel;
    edSuccess: TEdit;
    bButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizNewClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mQuestions1Click(Sender: TObject);
    procedure mQuestions2Click(Sender: TObject);
    procedure mQuestions3Click(Sender: TObject);
    procedure mQuestions4Click(Sender: TObject);
    procedure mQuestions5Click(Sender: TObject);
    procedure mAnswers1Click(Sender: TObject);
    procedure mAnswers2Click(Sender: TObject);
    procedure bButtonClick(Sender: TObject);
  private
    iQuiz, iRiver, iQuestion, iCorrect: Integer;
    LuxRivers: TLuxRivers;
    aRiversDone: TRiversDone;
  end;

const
  QuizShort: TQuizes = (
  'Quell', 'Quell', 'Mündung', 'Mündung', 'Mündung');
  QuizLong: TQuizes = (
  'Quell (Land)', 'Quell (Uertschaft)', 'Mündung (Land)', 'Mündung (Uertschaft)', 'Mündung (Floss)');
  Countries: TCountries =  (
    'Lëtzebuerg', 'Belscht', 'Frankräich', 'Däitschland');
  Places: TPlaces = (
    'Audun-le-Tiche', 'Bastogne', 'Bazeilles', 'Bussang', 'Eichelsberg', 'Ettelbréck',
    'Giewelsmillen', 'Gréiwels', 'Grondhaff', 'Haute-Kontz','Héiweng',  'Helleng',
    'Huldang', 'Kautebaach', 'Koblenz', 'Kolmer-Bierg', 'Mäertert', 'Miersch',
    'Rammeldang','Reisduerf', 'Schëtzelbur', 'Sélange', 'Siren', 'Thiaumont',
    'Uewerkuer', 'Vaux-les-Rosières', 'Wallenduerf', 'Waasserbëlleg');
  Rivers: TRivers = (
    'Äisch', 'Atert', 'Gander', 'Klierf', 'Kuer', 'Mamer', 'Musel', 'Our', 'Sauer',
    'Schwaarz Ernz', 'Sir', 'Uelzecht', 'Waark', 'Wäiss Ernz', 'Wiltz', 'Meuse', 'Rhäin');

var
  fFQuiz: TfFQuiz;

implementation

{$R *.lfm}

{ Read rivers data from text file }

procedure ReadRivers(var LRivers: TLuxRivers);

var
  InFile: Text;
  I, J: Integer;
  S: string;

begin
  // To avoid problems with 2-byte UTF-characters, the different fields to fill the record are each on its own text line
  // Note that the order of the rivers in the file MUST be identical to the one in the TRivers const Rivers!
  Assign(InFile, 'fless.txt'); Reset(InFile); I := 0;
  while (not EoF(InFile)) do begin
    if I < NRiversLux then begin
      Readln(InFile, S);                                                       // read text line (may be blank or river name)
      if S <> '' then begin
        // If text line contains data, is considered to be a new river (name)
        Inc(I);
        // Read rest of data for this river and fill values into record structure
        with LRivers[I] do begin
          River := S;
          Readln(InFile, Source);
          Readln(InFile, S);
          // Get source country name from country code
          for J := 1 to NCountries do begin
            if S = Copy(Countries[J], 1, 1) then
              SrcCountry := Countries[J];
          end;
          Readln(InFile, Destination);
          Readln(InFile, S);
          // Get destination country name from country code
          for J := 1 to NCountries do begin
            if S = Copy(Countries[J], 1, 1) then
              DestCountry := Countries[J];
          end;
          Readln(InFile, DestRiver);
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Start a new quiz }

procedure NewQuiz(var Q, C: Integer; var Done: TRiversDone);

var
  I: Integer;

begin
  // Clear variables
  Q := 0; C := 0;
  for I := 1 to NRiversLux do
    Done[I] := False;
  // Clear form fields
  fFQuiz.edRiver.Text := '';    fFQuiz.cbAnswer.Text := '';  fFQuiz.edEval.Text := '';
  fFQuiz.edQuestion.Text := ''; fFQuiz.edCorrect.Text := ''; fFQuiz.edSuccess.Text := '';
  fFQuiz.mQuestions.Enabled := True; fFQuiz.mAnswers.Enabled := True;
  // Set button capture
  fFQuiz.bButton.Caption := 'Start';
end;

{ Fill the combobox with the values for the quiz actually selected }

procedure FillCombobox(Q: Integer);

var
  NItems, I: Integer;
  S: string;

begin
  // Number of items in the combobox
  case Q of
    1,3: NItems := NCountries;
    2,4: NItems := NPlaces;
      5: NItems := NRivers;
  end;
  // Fill the combobox
  fFQuiz.cbAnswer.Clear;
  if fFQuiz.mAnswers1.Checked then begin                                       // fill combobox only if "choose from list" is selected
    for I := 1 to NItems do begin
      case Q of
        1, 3: S := Countries[I];
        2, 4: S := Places[I];
           5: S := Rivers[I];
      end;
      fFQuiz.cbAnswer.Items.Append(S);
    end;
  end;
  // Adapt form field text to actually selected quiz
  fFQuiz.Title.Caption := 'D''Flëss zu Lëtzebuerg: ' + QuizLong[Q];
  fFQuiz.edQuiz.Caption := QuizShort[Q];
end;

{*********}
{ TfFQuiz }
{*********}

{ Initialisation at application start }

procedure TfFQuiz.FormCreate(Sender: TObject);

begin
  ReadRivers(LuxRivers);                                                       // read rivers data from file
  iQuiz := 5;                                                                  // set actual quiz
  FillCombobox(iQuiz);                                                         // fill the combobox
  NewQuiz(iQuestion, iCorrect, aRiversDone);                                   // start a new quiz (reset variables)
  Randomize;                                                                   // start random number generator
end;

{ Menu item "Quiz > Neie Quiz": Start new quiz }

procedure TfFQuiz.mQuizNewClick(Sender: TObject);

begin
  NewQuiz(iQuestion, iCorrect, aRiversDone);
end;

{ Menu item "Quiz > Zoumaachen": Exit application }

procedure TfFQuiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Froën > ...": Select the quiz questions to be used }

procedure TfFQuiz.mQuestions1Click(Sender: TObject);

begin
  if not mQuestions1.Checked then begin
    mQuestions1.Checked := True;   mQuestions2.Checked := False;  mQuestions3.Checked := False;
    mQuestions4.Checked := False;  mQuestions5.Checked := False;
    iQuiz := 1;
    FillCombobox(iQuiz);
  end;
end;

procedure TfFQuiz.mQuestions2Click(Sender: TObject);

begin
  if not mQuestions2.Checked then begin
    mQuestions2.Checked := True;   mQuestions1.Checked := False;  mQuestions3.Checked := False;
    mQuestions4.Checked := False;  mQuestions5.Checked := False;
    iQuiz := 2;
    FillCombobox(iQuiz);
  end;
end;

procedure TfFQuiz.mQuestions3Click(Sender: TObject);

begin
  if not mQuestions3.Checked then begin
    mQuestions3.Checked := True;   mQuestions1.Checked := False;  mQuestions2.Checked := False;
    mQuestions4.Checked := False;  mQuestions5.Checked := False;
    iQuiz := 3;
    FillCombobox(iQuiz);
  end;
end;

procedure TfFQuiz.mQuestions4Click(Sender: TObject);

begin
  if not mQuestions4.Checked then begin
    mQuestions4.Checked := True;   mQuestions1.Checked := False;  mQuestions2.Checked := False;
    mQuestions3.Checked := False;  mQuestions5.Checked := False;
    iQuiz := 4;
    FillCombobox(iQuiz);
  end;
end;

procedure TfFQuiz.mQuestions5Click(Sender: TObject);

begin
  if not mQuestions5.Checked then begin
    mQuestions5.Checked := True;   mQuestions1.Checked := False;  mQuestions2.Checked := False;
    mQuestions3.Checked := False;  mQuestions4.Checked := False;
    iQuiz := 5;
    FillCombobox(iQuiz);
  end;
end;

{ Menu items "Äntwerten > Auswielen": Answers given by selection from list }

procedure TfFQuiz.mAnswers1Click(Sender: TObject);

begin
  if not mAnswers1.Checked then begin
    mAnswers1.Checked := True;  mAnswers2.Checked := False;
    FillCombobox(iQuiz);                                                       // fill the combobox with values for actual quiz
  end;
end;

{ Menu items "Äntwerten > Aginn": Answers given by keyboard entry }

procedure TfFQuiz.mAnswers2Click(Sender: TObject);

begin
  if not mAnswers2.Checked then begin
    mAnswers2.Checked := True;
    mAnswers1.Checked := False;
    cbAnswer.Clear;                                                            // clear the combobox (no answer selections shown)
  end;
end;

{ Button click: Main quiz routine }

procedure TfFQuiz.bButtonClick(Sender: TObject);

var
  Answer, UAnswer: string;
  Percent: Real;

begin
  // Action = question
  if (bButton.Caption = 'Start') or (bButton.Caption = 'Nächst Fro') then begin
    if bButton.Caption = 'Start' then begin
      // Disable quiz settings
      mQuestions.Enabled := False;
      mAnswers.Enabled := False;
    end;
    Inc(iQuestion);
    // Proceed as long as there are undone rivers left
    if iQuestion <= NRiversLux then begin
      edQuestion.Text := IntToStr(iQuestion);
      // Get a random river (not yet done so far)
      repeat
        iRiver := Random(NRiversLux) + 1;
      until aRiversDone[iRiver] = False;
      aRiversDone[iRiver] := True;                                             // mark this river as "being done"
      edRiver.Text := Rivers[iRiver];
      // Set button caption for next action (= answer)
      bButton.Caption := 'Äntwert';
    end;
  end
  // Action = answer
  else begin
    // Proceed as long as there are undone rivers left
    if iQuestion <= NRiversLux then begin
      // Get user answer from combobox selection/entry
      UAnswer := cbAnswer.Text;
      // Get correct answer (depending on actual quiz)
      case iQuiz of
        1: Answer := LuxRivers[iRiver].SrcCountry;
        2: Answer := LuxRivers[iRiver].Source;
        3: Answer := LuxRivers[iRiver].DestCountry;
        4: Answer := LuxRivers[iRiver].Destination;
        5: Answer := LuxRivers[iRiver].DestRiver;
      end;
      // User answer is correct
      if (UAnswer = Answer) or ((iQuiz in [1, 3]) and (Length(UAnswer) = 1) and (UAnswer[1] = Answer[1])) then begin
        Inc(iCorrect);
        edCorrect.Text := IntToStr(iCorrect);
        edEval.Font.Color := clLime;
        edEval.Text := 'Richteg!';
      end
      // User answer is not correct
      else begin
        edEval.Font.Color := clRed;
        edEval.Text := 'Falsch! Richteg = ' + Answer;
      end;
      // Calculate success percentage
      Percent := 100 * iCorrect / iQuestion;
      Percent := Int(100 * Percent) / 100;
      if Percent < 50 then
        edSuccess.Font.Color := clRed
      else
        edSuccess.Font.Color := clDefault;
      edSuccess.Text := FloatToStr(Percent) + ' %';
      // If undone rivers left, set button caption for next action (= next question)
      if iQuestion < NRiversLux then
        bButton.Caption := 'Nächst Fro';
    end;
    // All rivers done: end of the quiz
    if iQuestion >= NRiversLux then begin
      if bButton.Caption = 'Äntwert' then begin
        MessageDlg('Flëss Quiz','De Quiz ass elo fäerdech', mtInformation, [mbOK], 0);
        bButton.Caption := 'Fäerdech';
      end;
    end;
  end;
end;

end.

