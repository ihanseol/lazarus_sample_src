{****************************************}
{* Main unit for Solubility application *}
{****************************************}

unit solubility_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, ExtCtrls;

type
  TIon = record
    Name, Formula: string;
    Charge: Integer;
  end;
  TIons = array of TIon;
  TSolubility = record
    Cation, Solubility: string;
    Exceptions: array of record
      Anion, Solubility: string;
    end;
  end;
  TSolubilities = array of TSolubility;
  TSpecial = array of record
    Salt: string;
    Special: Char;
  end;
  TDone = array of record
    Anion, Cation: string;
  end;
  {**************}
  { TfSolubility }
  {**************}
  TfSolubility = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsName, mOptionsReactifs: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, laQuestion: TLabel;
    edSalt, edSaltName: TEdit;
    rbSoluble, rbInsoluble, rbSlightly, rbReactif: TRadioButton;
    shSoluble, shInsoluble, shSlightly, shReactif: TShape;
    edDetails: TEdit;
    btStart: TButton;
    sgEval: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsNameClick(Sender: TObject);
    procedure mOptionsReactifsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure rbSolubleChange(Sender: TObject);
    procedure rbInsolubleChange(Sender: TObject);
    procedure rbSlightlyChange(Sender: TObject);
    procedure rbReactifChange(Sender: TObject);
  private
    iQuestions0, iQuestions, iQuestion, iCorrect: Integer;
    sAnswer, sDetails, sUserAnswer: string;
    bReactifs0, bReactifs, bName0, bName: Boolean;
    aRadioButtons: array[0..3] of TRadioButton;
    aRadioShapes: array[0..3] of TShape;
    aAnions, aCations: TIons;
    aSolubility: TSolubilities;
    aSpecial: TSpecial;
    aDone: TDone;
  end;

var
  fSolubility: TfSolubility;

implementation

{$R *.lfm}

{ Apply subscripts to salt formulas }

function ApplySubscripts(S: string): string;

const
  Subscripts: array[2..4] of string = (
    #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84
  );

var
  I: Integer;

begin
  for I := 2 to 4 do
    S := StringReplace(S, IntToStr(I), Subscripts[I], [rfReplaceAll]);
  Result := S;
end;

{ Format number for grid display (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN += ' '
  else
    SN += S;
  Result := SN;
end;

{ Read anions and cations from text files }

procedure ReadIons(out Anions, Cations: TIons);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Anions, 0); SetLength(Cations, 0);
  // Anions
  Assign(InFile, 'anions.txt'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Anions, N);
      Anions[N - 1].Name := Trim(LeftStr(Line, 15));
      Anions[N - 1].Formula := Trim(Copy(Line, 16, 10));
      Anions[N - 1].Charge := StrToInt(Copy(Line, 26, 2));
    end;
  end;
  Close(InFile);
  // Cations
  Assign(InFile, 'cations.txt'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Cations, N);
      Cations[N - 1].Name := Trim(LeftStr(Line, 15));
      Cations[N - 1].Formula := Trim(Copy(Line, 16, 10));
      Cations[N - 1].Charge := StrToInt(Copy(Line, 26, 2));
    end;
  end;
  Close(InFile);
end;

{ Read solubility rules from text file }

procedure ReadSolubility(out Solubility: TSolubilities);

const
  DSol: array[0..2] of string = ( 'soluble', 'insoluble', 'slightly soluble' );

var
  N, E, I, P: Integer;
  Sol, Line, S: string;
  InFile: Text;

begin
  N := 0; SetLength(Solubility, N);
  Assign(InFile, 'rules.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      S := LeftStr(Line, 15);
      if S <> '               ' then begin
        // Start of data (first line = general solubility rule) for a new cation
        S := Trim(S); Delete(S, Length(S), 1);                                 // remove the plural "s"
        Inc(N); SetLength(Solubility, N);                                      // new item of the TSolubilities array
        Solubility[N - 1].Cation := LowerCase(S);                              // cation name
        // Set general solubility rule for this cation
        for I := 0 to 2 do begin
          P := Pos(DSol[I], Line);
          if P > 0 then
            Solubility[N - 1].Solubility := DSol[I];
        end;
        // Set length of this cation's exceptions array to 0
        E := 0; SetLength(Solubility[N - 1].Exceptions, E);
      end
      else begin
        // Data continuation (second and possibly third line = solubility exceptions) for actual cation
        S := Trim(Copy(Line, 16, Length(Line)));
        if S <> 'Exceptions: -' then begin
          // Solubility for subsequent exceptions (the ones stated in this line)
          for I := 0 to 2 do begin
            P := Pos('(' + DSol[I] + ')', S);
            if P > 0 then
              Sol := DSol[I];
          end;
          // Parse line to find the exceptions (anions)
          P := Pos(':', S);
          S := Copy(S, P + 2, Length(S));
          while S <> '' do begin
            // Repeat until all exceptions (in this line) have been done
            if LeftStr(S, 13) = 'Alkali metals' then begin
              // Do not consider alkali metals (these will be hard-coded)
              S := '';                                                         // alkali metals must be last exception in the line!
            end
            else begin
              // Add new exception (anion) for actual cation
              Inc(E); SetLength(Solubility[N - 1].Exceptions, E);
              P := Pos(',', S);
              if P > 0 then begin
                Solubility[N - 1].Exceptions[E - 1].Anion := LowerCase(LeftStr(S, P - 1));
                Delete(S, 1, P + 1);
              end
              else begin
                Solubility[N - 1].Exceptions[E - 1].Anion := LowerCase(S);
                S := '';
              end;
              // Add solubility rule for this anion
              Solubility[N - 1].Exceptions[E - 1].Solubility := Sol;
            end;
          end;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Read non-applicable data from text file }

procedure ReadSpecial(out Special: TSpecial);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Special, 0);
  Assign(InFile, 'na.txt'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Special, N);
      Special[N - 1].Salt := Trim(LeftStr(Line, 15));
      Special[N - 1].Special := Copy(Line, 16, 1)[1];
    end;
  end;
  Close(InFile);
end;

{ Create salt formula from anion and cation }

function GetSalt(Anion, Cation: string; AnionCharge, CationCharge: Integer): string;

var
  A, C, Count, I: Integer;
  Salt: string;

begin
  CationCharge := -CationCharge;
  Salt := Anion;
  if AnionCharge = CationCharge then begin
    // Anion and cation have same absolute charges
    Salt += Cation;
  end
  else begin
    // Anion and cation have different absolute charges
    if AnionCharge = 1 then begin
      // Anion charge = +1
      if Anion = 'NH4' then
        Salt := '(NH4)';                                                       // the only anion that needs brackets in salt formula
      Salt += IntToStr(CationCharge) + Cation;
    end
    else if CationCharge = 1 then begin
      // Cation charge = -1
      Count := 0;
      for I := 1 to Length(Cation) do begin
        // Count number of different atoms
        if Cation[I] in ['A'..'Z', '0'..'9'] then
          Inc(Count);
      end;
      if Count = 1 then begin
        // No brackets needed for single-atom cation
        Salt += Cation + IntToStr(AnionCharge);
      end
      else begin
        // Need of brackets for multiple-atoms cation
        Salt += '(' + Cation + ')' + IntToStr(AnionCharge);
      end;
    end
    else begin
      // Last case: One of the ions has charge ±2, the other ±3
      if AnionCharge = 3 then begin
        // Anion charge = +3
        A := 2; C := 3;
      end
      else begin
        // Cation charge = -3
        A := 3; C := 2;
      end;
      // Salt formula will be Anion2Cation3 or Anion3Cation2 (with or without need of brackets; cf. above)
      if Anion = 'NH4' then
        Salt := '(NH4)';
      Salt += IntToStr(A);
      Count := 0;
      for I := 1 to Length(Cation) do begin
        if Cation[I] in ['A'..'Z', '0'..'9'] then
          Inc(Count);
      end;
      if Count = 1 then
        Salt += Cation + IntToStr(C)
      else
        Salt += '(' + Cation + ')' + IntToStr(C);
    end;
  end;
  Result := Salt;
end;

{ Check if an anion is an alkali metal }

function IsAlkaliMetal(Anion: string): Boolean;

const
  AlkaliMetals: array[0..5] of string = (
    'lithium', 'sodium', 'potassium', 'rubidium', 'cesium', 'francium'
  );

var
  I: Integer;
  ItIs: Boolean;

begin
  ItIs := False;
  for I := 0 to 5 do begin
    if Anion = AlkaliMetals[I] then
      ItIs := True;
  end;
  Result := ItIs;
end;

{**************}
{ TfSolubility }
{**************}

{ Application start: Initialization }

procedure TfSolubility.FormCreate(Sender: TObject);

begin
  aRadioButtons[0] := rbSoluble;  aRadioButtons[1] := rbInsoluble;
  aRadioButtons[2] := rbSlightly; aRadioButtons[3] := rbReactif;
  aRadioShapes[0] := shSoluble;  aRadioShapes[1] := shInsoluble;
  aRadioShapes[2] := shSlightly; aRadioShapes[3] := shReactif;
  ReadIons(aAnions, aCations);
  ReadSolubility(aSolubility);
  ReadSpecial(aSpecial);
  iQuestions0 := 20; bReactifs0 := False; bName0 := False;
  Randomize;
  mTestNew.Click;
end;

{ Menu item "Exercise > New": Start new exercise }

procedure TfSolubility.mTestNewClick(Sender: TObject);

var
  I: Integer;

begin
  // Make user settings active now
  iQuestions := iQuestions0; bReactifs := bReactifs0; bName := bName0;
  // Enable/disable "reacts with water" radiobutton
  if bReactifs then
    rbReactif.Enabled := True
  else
    rbReactif.Enabled := False;
  // Make either salt formula or name editi field visible
  if bName then begin
    edSalt.Visible := False; edSaltName.Visible := True;
  end
  else begin
    edSalt.Visible := True; edSaltName.Visible := False;
  end;
  // Reset form fields
  laQuestion.Caption := 'Question:';
  edSalt.Text := ''; edDetails.Text := '';
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  iQuestion := 0; iCorrect := 0;
  SetLength(aDone, 0);
  btStart.Enabled := True;
  btStart.Caption := 'Start';
end;

{ Menu item "Exercise > Exit": Exit application }

procedure TfSolubility.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Option > Number of questions...": User input of number of exercise questions }

procedure TfSolubility.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Solubility exercises', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 10 then
      iQuestions0 := 10                                                        // arbitrarly fixed minimum = 10
    else if iQuestions0 > 100 then
      iQuestions0 := 100;                                                      // arbitrarly fixed maximum = 100
  end;
end;

{ Menu item "Option > Include water reactifs": Toggle asking or not for water reactif salts }

procedure TfSolubility.mOptionsReactifsClick(Sender: TObject);

begin
  if mOptionsReactifs.Checked then
    mOptionsReactifs.Checked := False
  else
    mOptionsReactifs.Checked := True;
  bReactifs0 := mOptionsReactifs.Checked;
end;

{ Menu item "Option > Use salt name": Toggle display of salt formula or name }

procedure TfSolubility.mOptionsNameClick(Sender: TObject);

begin
  if mOptionsName.Checked then
    mOptionsName.Checked := False
  else
    mOptionsName.Checked := True;
  bName0 := mOptionsName.Checked;
end;

{ Menu item "Help > About": Display application about }

procedure TfSolubility.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry:' + LineEnding;
  S += 'Solubility of salts exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February-March 2023.';
  MessageDlg('About "Solubility"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate new question resp. check user answer }

procedure TfSolubility.btStartClick(Sender: TObject);

var
  AX, CX, I, J: Integer;
  Anion, Cation, Salt, SaltName, Answer: string;
  OK: Boolean;
  Colour: TColor;

begin
  // Button "Start/Question": Generate new question
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Question') then begin
    sAnswer := ''; sDetails := '';
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions);
    // Random anion and cation (among those available and valid)
    repeat
      OK := True;
      AX := Random(Length(aAnions)); CX := Random(Length(aCations));
      Anion := aAnions[AX].Name; Cation := aCations[CX].Name;
      for I := 0 to Length(aDone) - 1 do begin
        // Check if this anion-cation pair hasn't already been done
        if (aDone[I].Anion = Anion) and (aDone[I].Cation = Cation) then
          OK := False;
      end;
      if OK then begin
        // Get salt formula and name
        Salt := GetSalt(aAnions[AX].Formula, aCations[CX].Formula, aAnions[AX].Charge, aCations[CX].Charge);
        SaltName := aAnions[AX].Name + ' ' + aCations[CX].Name;
        // Check if this salt is "special" (unknown solubility, water reactif)
        for I := 0 to Length(aSpecial) - 1 do begin
          if Salt = aSpecial[I].Salt then begin
            if aSpecial[I].Special = 'R' then begin
              // Water reactif salts: Include only if user selected to do so
              if bReactifs then begin
                sAnswer := 'reacts with water';
                sDetails := 'Solubility does not apply to ' + Salt + ', as this salt reacts with water.';
              end
              else
                OK := False;
            end
            else begin
              // Unknown solubility salt
              OK := False;
            end;
          end;
        end;
      end;
    until OK;
    // Mark this anion-cation pair as done
    SetLength(aDone, Length(aDone) + 1);
    aDone[Length(aDone) - 1].Anion := Anion;
    aDone[Length(aDone) - 1].Cation := Cation;
    // Display salt formula or name
    if bName then
      edSaltName.Text := SaltName
    else
      edSalt.Text := ApplySubscripts(Salt);
    edDetails.Text := '';
    // Determine this salt's solubility (non water-reactif salts)
    if sAnswer = '' then begin
      // Salts of ammonium are all soluble
      if Anion = 'ammonium' then begin
        sAnswer := 'soluble';
        sdetails := 'All salts of ammonium are soluble';
      end
      // Salts of alkali metals are soluble, but there are exceptions (hard-coded here)
      else if IsAlkaliMetal(Anion) then begin
        sAnswer := 'soluble';
        sDetails := 'Salts of alkali metals are soluble';
        if (SaltName = 'rubidium oxalate') or (SaltName = 'rubidium phosphate') or (SaltName = 'cesium oxalate') then begin
          sAnswer := 'insoluble';
          sDetails += ', but ' + SaltName + ' is an exception and is insoluble'
        end
        else if (SaltName = 'lithium fluoride') or (SaltName = 'lithium carbonate') or (SaltName = 'lithium phosphate')
          or (SaltName = 'potassium perchlorate') or (SaltName = 'rubidium perchlorate') or (SaltName = 'cesium perchlorate') then begin
          sAnswer := 'slightly soluble';
          sDetails += ', but ' + SaltName + ' is only slightly soluble';
        end;
      end;
      // Solubility of non ammonium and alkali metals salts determined by considering the solubility rules for actual cation
      if sAnswer = '' then begin
        for I := 0 to Length(aSolubility) - 1 do begin
          if Cation = aSolubility[I].Cation then begin
            // General solubility rule for actual cation
            sAnswer := aSolubility[I].Solubility;
            if Length(aSolubility[I].Exceptions) = 0 then
              sDetails := 'All ' + Cation + 's are ' + sAnswer
            else begin
              sDetails := Cation + 's are ' + sAnswer;
              // Check if combination with actual anion is an exception
              for J := 0 to Length(aSolubility[I].Exceptions) - 1 do begin
                if LowerCase(Anion) = aSolubility[I].Exceptions[J].Anion then begin
                  Answer := sAnswer;
                  // Solubility of actual salt (with exceptions considered)
                  sAnswer := aSolubility[I].Exceptions[J].Solubility;
                  // Generate solubility details text
                  if Cation = 'fluoride' then
                    sDetails += ', but ' + Saltname + ' is ' + sAnswer
                  else if (sAnswer = 'soluble') or (sAnswer = 'insoluble') then
                    sDetails += ', but ' + Saltname + ' is an exception and is ' + sAnswer
                  else begin
                    if Answer = 'soluble' then
                      sDetails += ', but ' + Saltname + ' is only slightly soluble'
                    else
                      sDetails += ', but ' + Saltname + ' is slightly soluble';
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    // Solubility details for actual salt
    sDetails[1] := UpperCase(sDetails[1])[1];
    // Reset form fields (radiobuttons, evaluation shapes)
    rbSoluble.Checked := False; rbInsoluble.Checked := False;
    rbSlightly.Checked := False; rbReactif.Checked := False;
    sUserAnswer := '';
    shSoluble.Visible := False; shInsoluble.Visible := False;
    shSlightly.Visible := False; shReactif.Visible := False;
    // Next button push will be to check user answer
    btStart.Caption := 'Answer';
  end
  // Button "Answer": Check user answer (sUserAnswer variable is set when user checks a radiobutton)
  else begin
    if sUserAnswer = sAnswer then begin
      // Correct answer
      Inc(iCorrect);
      Colour := clLime;
    end
    else begin
      // False answer
      Colour := clRed;
    end;
    // Mark user answer (as correct or false)
    for I := 0 to 3 do begin
      if aRadioButtons[I].Checked then begin
        aRadioShapes[I].Visible := True;
        aRadioShapes[I].Brush.Color := Colour;
      end;
    end;
    // Display solubility details
    edDetails.Text := sDetails;
    // Fill-in evaluation grid
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * iCorrect / iQuestion), '%');
    // If there are still questions left...
    if iQuestion < iQuestions then begin
      // Next button push will be generation of another question
      btStart.Caption := 'Question';
    end
    // If all questions have been done...
    else begin
      // Display "end of exercise" message
      MessageDlg('Solubility exercises', 'All questions have been done. End of exercise.', mtInformation, [mbOK], 0);
      btStart.Caption := 'Start'; btStart.Enabled := False;
    end;
  end;
end;

{ User selection of radiobuttons: Save selection as actual user answer }

procedure TfSolubility.rbSolubleChange(Sender: TObject);

begin
  if rbSoluble.Checked then
    sUserAnswer := 'soluble';
end;

procedure TfSolubility.rbInsolubleChange(Sender: TObject);

begin
  if rbInsoluble.Checked then
    sUserAnswer := 'insoluble';
end;

procedure TfSolubility.rbSlightlyChange(Sender: TObject);

begin
  if rbSlightly.Checked then
    sUserAnswer := 'slightly soluble';
end;

procedure TfSolubility.rbReactifChange(Sender: TObject);

begin
  if rbReactif.Checked then
    sUserAnswer := 'reacts with water';
end;

end.

