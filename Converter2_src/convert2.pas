{****************************************}
{* Main unit for Converter2 application *}
{****************************************}

unit convert2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, help;

type
  {**************}
  { TfConverter2 }
  {**************}
  TfConverter2 = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew1, mTestNew2, mTestNew3A, mTestNew3B, mTestExit: TMenuItem;
    mSettings, mSettingsExercises, mSettingsNames: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Memo1: TMemo;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    cobMeasure: TComboBox;
    laExercise: TLabel;
    edValue1, edUnit1, edValue2, edUnit2, edEval: TEdit;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNew1Click(Sender: TObject);
    procedure mTestNew2Click(Sender: TObject);
    procedure mTestNew3AClick(Sender: TObject);
    procedure mTestNew3BClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsExercisesClick(Sender: TObject);
    procedure mSettingsNamesClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure cobMeasureChange(Sender: TObject);
  private
    iSQuestions, iSMeasure, iSOldMeasure, iQuestions, iMeasure, iQuestion, iCorrect, iFalse: Integer;
    rValue2, rRound2: Double;
    iTest: Integer;
  end;

const
  NMeasurements = 8;
  SUP_2 = #$C2#$B2; SUP_3 = #$C2#$B3;
  Measurements: array[1 .. NMeasurements] of string = (
    'Length', 'Area', 'Volume', 'Capacity (liquid)', 'Capacity (dry)', 'Weight', 'Capacity', 'Weight'
  );
  MeasurementsUS: array[1..NMeasurements] of Boolean = (
    True, True, True, True, True, True, False, False
  );
  MeasurementsUK: array[1..NMeasurements] of Boolean = (
    True, True, True, False, False, False, True, True
  );
  UnitCounts: array[1..NMeasurements] of Integer = (
    8, 6, 6, 7, 4, 7, 9, 7
  );
  UnitNames: array[1 .. NMeasurements, 1 .. 9] of string = (
    ( 'nautical mile', 'mile', 'rod', 'yard', 'foot', 'inch', 'furlong', 'fathom', '' ),
    ( 'square mile', 'square rod', 'square yard', 'square foot', 'square inch', 'acre', '', '', '' ),
    ( 'acre-foot', 'cubic yard', 'cubic foot', 'cubic inch', 'board foot', 'cord', '', '', '' ),
    ( 'gallon', 'quart', 'pint', 'gill', 'fluid ounce', 'fluid dram', 'minim', '', '' ),
    ( 'bushel', 'peck', 'quart', 'pint', '', '', '', '', '' ),
    ( 'short ton', 'short hundredweight', 'pound', 'ounce', 'dram', 'grain', 'stone', '', '' ),
    ( 'bushel', 'peck', 'gallon', 'quart', 'pint', 'gill', 'fluid ounce', 'fluid dram', 'minim' ),
    ( 'long ton', 'long hundredweight', 'pound', 'ounce', 'dram', 'grain', 'stone', '', '' )
  );
  UnitSymbols: array[1 .. NMeasurements, 1 .. 9] of string = (
    ( 'nmi', 'mi', 'rd', 'yd', 'ft', 'in', 'fur', 'fth', '' ),
    ( 'mi2', 'rd2', 'yd2', 'ft2', 'in2', 'acre', '', '', '' ),
    ( 'ac ft', 'yd3', 'ft3', 'in3', 'bd ft', 'cd', '', '', '' ),
    ( 'gal', 'qt', 'pt', 'gi', 'fl oz', 'fl dr', 'min', '', '' ),
    ( 'bu', 'pk', 'qt', 'pt', '', '', '', '', '' ),
    ( 'US ton', 'US cwt', 'lb', 'oz', 'dr', 'gr', 'st', '', '' ),
    ( 'bu', 'pk', 'gal', 'qt', 'pt', 'gi', 'fl oz', 'fl dr', 'min' ),
    ( 'UK ton', 'UK cwt', 'lb', 'oz', 'dr', 'gr', 'st', '', '' )
  );
  // 1 unit at position N = conversion-factor at position N+1 units at position N+1
  // Ex: 1 nmi = 1.151 mi; 1 mi = 320 rod, 1 rod = 5.50 yd...
  // Negative conversion-factors are used for "special units", to be converted to a given unit "out of sequence"
  // In the current version 1.0, these "special units" are NOT implemented!
  UnitConversion: array[1 .. NMeasurements, 1 .. 9] of Real = (
    ( 1, 1.151, 320, 5.50, 3, 12, -660, -6, 0 ),
    ( 1, 640, 30.25, 9, 144, -4840, 0, 0, 0 ),
    ( 1, 1613, 27, 1728, -144, -(1/12), 0, 0, 0 ),
    ( 1, 4, 2, 4, 4, 8, 60, 0, 0 ),
    ( 1, 4, 8, 2, 0, 0, 0, 0, 0 ),
    ( 1, 20, 100, 16, 16, 27.344, -14, 0, 0 ),
    ( 1, 4, 2, 4, 2, 4, 5, 8, 60 ),
    ( 1, 20, 112, 16, 16, 27.344, -14, 0, 0 )
  );
  // International unit symbols correspond 1-to-1 to a given American/British unit
  // Ex: nmi -> km, mi -> km, rd -> m...
  IntUnitConversionSymbols: array[1 .. NMeasurements, 1 ..9] of string = (
    ( 'km', 'km', 'm', 'm', 'cm', 'cm', 'm', 'm', '' ),
    ( 'km2', 'm2', 'm2', 'm2', 'cm2', 'ha', '', '', '' ),
    ( 'm3', 'm3', 'm3', 'cm3', 'l', 'm3', '', '', '' ),
    ( 'l', 'l', 'l', 'ml', 'ml', 'ml', 'ml', '', '' ),
    ( 'l', 'l', 'l', 'l', '', '', '', '', '' ),
    ( 't', 'kg', 'kg', 'g', 'g', 'g', 'kg', '', '' ),
    ( 'm3', 'm3', 'l', 'l', 'cm3', 'cm3', 'cm3', 'cm3', 'cm3' ),
    ( 't', 'kg', 'kg', 'g', 'g', 'g', 'kg', '', '' )
  );
  // International conversion-factors are given for 1 unit of the corr. given American/British unit
  // Ex: 1 nmi = 1.852 km, 1 mi = 1.609 km, 1 rd = 5.029 m...
  IntUnitConversion: array[1 .. NMeasurements, 1 .. 9] of Real = (
    ( 1.852, 1.609, 5.029, 0.9144, 30.48, 2.54, 201, 1.829, 0 ),
    ( 2.590, 25.293, 0.836, 0.093, 6.452, 0.405, 0, 0, 0 ),
    ( 1233, 0.765, 0.028, 16.387, 2.36, 3.62, 0, 0, 0 ),
    ( 3.785, 0.946, 0.473, 118.294, 29.573, 3.697, 0.06161, 0, 0 ),
    ( 35.239, 8.81, 1.101, 0.551, 0, 0, 0, 0, 0 ),
    ( 0.907, 45.359, 0.454, 28.350, 1.772, 0.0648, 6.35, 0, 0 ),
    ( 0.036, 0.0091, 4.546, 1.136, 568.26, 142.066, 28.412, 3.5516, 0.059194 ),
    ( 1.016, 50.802, 0.454, 28.350, 1.772, 0.0648, 6.35, 0, 0 )
  );

var
  fConverter2: TfConverter2;

implementation

{ Prepare for a new test }

procedure NewTest(Test, SQuestions, SMeasure: Integer; out Questions, OldSMeasure, Measure, Question, QCorrect, QFalse: Integer);

var
  I: Integer;
  OldMeasure: string;

begin
  fConverter2.cobMeasure.ItemIndex := 0;
  Questions := SQuestions;
  Question := 0; QCorrect := 0; QFalse := 0;
  fConverter2.laExercise.Caption := 'Question';
  fConverter2.edValue1.Text := ''; fConverter2.edValue2.Text := '';
  fConverter2.edUnit1.Text := '';  fConverter2.edUnit2.Text := '';
  fConverter2.edEval.Text := ''; fConverter2.edEval.Color := clCream;
  fConverter2.edQuestions.Text := ''; fConverter2.edCorrect.Text := ''; fConverter2.edFalse.Text := '';
  fConverter2.edSuccess.Text := ''; fConverter2.edSuccess.Color := clCream;
  OldMeasure := fConverter2.cobMeasure.Items[SMeasure];
  fConverter2.cobMeasure.Clear;
  if Test in [10, 31] then
    fConverter2.stTitle.Caption := 'American measurement units.'
  else
    fConverter2.stTitle.Caption := 'British measurement units.';
  for I := 1 to NMeasurements do begin
    // Add American OR British measurements to combobox
    if (Test in [10, 31]) and MeasurementsUS[I] then
      fConverter2.cobMeasure.Items.AddText(Measurements[I])
    else if (Test in [20, 32]) and MeasurementsUK[I] then
      fConverter2.cobMeasure.Items.AddText(Measurements[I]);
  end;
  // Determine index of (constant) measurements arrays (from item selected in combobox)
  Measure := -1;
  for I := 1 to NMeasurements do begin
    if (OldMeasure = Measurements[I]) and (((Test in [10, 31]) and MeasurementsUS[I]) or (Test in [20, 32]) and MeasurementsUK[I]) then
      Measure := I;
  end;
  if Measure = -1 then begin
    // No index found: This indicates, that this measurement doesn't exist as such for actual system;
    // e.g. "Capacity" for American system, where "Capacity (liquid)" and "Capacity (dry)" are used
    fConverter2.cobMeasure.ItemIndex := -1;
  end
  else begin
    if Test in [10, 31] then
      // American system: Combobox itemindex directly corresponds to array index
      fConverter2.cobMeasure.ItemIndex := Measure - 1
    else begin
      // British system: Combobox index has to be adapted (for last 2 (British only) measurements in array )
      if Measure <= fConverter2.cobMeasure.Items.Count then
        fConverter2.cobMeasure.ItemIndex := Measure - 1
      else
        fConverter2.cobMeasure.ItemIndex := Measure - 4;
    end;
    OldSMeasure := SMeasure;                                                   // to tell application, that a new test has been started (after change in combobox)
  end;
  fConverter2.btQuestion.Caption := 'Question'; fConverter2.btQuestion.Enabled := True;
end;

{ Conversion of international unit symbol to full name }

function IntUnitName(USymbol: string): string;

const
  NUnits = 14;
  USymbols: array[1..NUnits] of string = (
    'km', 'm', 'cm', 'km2', 'ha', 'm2', 'cm2', 'm3', 'cm3', 'l', 'ml', 't', 'kg', 'g'
  );
  UNames: array[1..NUnits] of string = (
  'kilometer', 'meter', 'centimeter', 'kilometer square', 'hectare', 'meter square',
  'centimeter square', 'meter cube', 'entimeter cube', 'liter', 'milliliter', 'metric ton', 'kilogram', 'gram'
  );

var
  I, IX: Integer;

begin
  for I := 1 to NUnits do begin
    if USymbol = USymbols[I] then
      IX := I;
  end;
  Result := UNames[IX];
end;

{ American/British measurement system unit conversion routine }

function MeasurementConversion(Test, Measure, X1, X2: Integer; Value1: Double): Double;

// Arguments: Test = actual conversion system; Measure = actual measurement;
//            X1, X2 = indexes for actual units (for actual conversion-factors); Value1: value to be converted

var
  I: Integer;
  Value2: Double;

begin
  Value2 := Value1;
  // American to American or British to British conversion
  if Test < 30 then begin
    if X1 < X2 then begin
      // Convert using conversion-factor "from start to end"
      for I := X1 + 1 to X2 do
        Value2 *= UnitConversion[Measure, I];
    end
    else begin
      // Convert using inverse of conversion-factor "from end to start"
      for I := X1 downto X2 + 1 do
        Value2 /= UnitConversion[Measure, I];
    end;
  end
  // American/British to International or International to American/British conversion
  else begin
    // International to American/British conversion
    if X1 = -1 then begin
      Value2 *= IntUnitConversion[Measure, X2];
    end
    // American/British to International conversion
    else if X2 = -1 then begin
      Value2 /= IntUnitConversion[Measure, X1];
    end;
  end;
  Result := Value2;
end;

{$R *.lfm}

{***************}
{* TfConverter2 *}
{***************}

{ Application start: Initialisation }

procedure TfConverter2.FormCreate(Sender: TObject);

begin
  iTest := 10; iSQuestions := 10; iSOldMeasure := -1; iSMeasure := 0;
  NewTest(iTest, iSQuestions, iSMeasure, iQuestions, iSOldMeasure, iMeasure, iQuestion, iCorrect, iFalse);
  Randomize;
end;

{ Menu item "Test > American system": Prepare for a new American measurements conversion test }

procedure TfConverter2.mTestNew1Click(Sender: TObject);

begin
  iTest := 10;
  NewTest(iTest, iSQuestions, iSMeasure, iQuestions, iSOldMeasure, iMeasure, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > British system": Prepare for a new British measurements conversion test }

procedure TfConverter2.mTestNew2Click(Sender: TObject);

begin
  iTest := 20;
  NewTest(iTest, iSQuestions, iSMeasure, iQuestions, iSOldMeasure, iMeasure, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > International/US": Prepare for a new International to American measurements conversion test }

procedure TfConverter2.mTestNew3AClick(Sender: TObject);

begin
  iTest := 31;
  NewTest(iTest, iSQuestions, iSMeasure, iQuestions, iSOldMeasure, iMeasure, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > International/UK": Prepare for a new International to British measurements conversion test }

procedure TfConverter2.mTestNew3BClick(Sender: TObject);

begin
  iTest := 32;
  NewTest(iTest, iSQuestions, iSMeasure, iQuestions, iSOldMeasure, iMeasure, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > Exit": Exit application }

procedure TfConverter2.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of questions": Choose number of exercises }

procedure TfConverter2.mSettingsExercisesClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Measurement units', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then
    iSQuestions := StrToInt(S);
end;

{ Menu item "Settings > Use full unit names": Toggle between unit names and symbols }

procedure TfConverter2.mSettingsNamesClick(Sender: TObject);

begin
  if mSettingsNames.Checked then
    mSettingsNames.Checked := False
  else
    mSettingsNames.Checked := True;
end;

{ Menu item "Help > Help": Display program help }

procedure TfConverter2.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfConverter2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Maths trainer for primary school pupils:' + LineEnding;
  S += 'US and UK measurement units conversion.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February-March 2020.';
  MessageDlg('About "Converter2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new exercise, resp. check pupil's answer }

procedure TfConverter2.btQuestionClick(Sender: TObject);

var
  UX1, UX2, V, Success: Integer;
  Value, Value1, UValue2, Round1, Round2: Double;
  Unit1, Unit2, Temp: string;
  OK: Boolean;

begin
  // Button "Question": Generate a new exercise
  // ------------------------------------------
  if btQuestion.Caption = 'Question' then begin
    if cobMeasure.ItemIndex = -1 then
      MessageDlg('User error', 'Please, select a measurement', mtError, [mbOK], 0)
    else if (iMeasure = -1) or (iSMeasure <> iSOldMeasure) then
      MessageDlg('User error', 'Please, select an item in the "Test" menu first!', mtError, [mbOK], 0)
    else begin
      Inc(iQuestion);
      laExercise.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
      // American to American resp. British to British conversion exercise
      if iTest < 30 then begin
        // Get 2 distinct random measurement units, with arbitrar condition
        // that the first is less than 1e+6 times bigger/smaller than the second
        repeat
          OK := True;
          UX1 := Random(UnitCounts[iMeasure]) + 1; UX2 := Random(UnitCounts[iMeasure]) + 1;
          if UX1 = UX2 then
            // Units must be different
            OK := False
          else if (UnitConversion[iMeasure, UX1] < 0) or (UnitConversion[iMeasure, UX2] < 0) then
            // "Special units" ignored in version 1.0
            OK := False
          else begin
            Value1 := 1; rValue2 := MeasurementConversion(iTest, iMeasure, UX1, UX2, Value1);
            if (rValue2 < 1E-6 * Value1) or (rValue2 > 1E+6 * Value1) then
              OK := False;
          end;
        until OK;
        if mSettingsNames.Checked then begin
          Unit1 := UnitNames[iMeasure, UX1]; Unit2 := UnitNames[iMeasure, UX2];
        end
        else begin
          Unit1 := UnitSymbols[iMeasure, UX1]; Unit2 := UnitSymbols[iMeasure, UX2];
        end;
      end
      // American/British to International (or vice-versa) conversion exercise
      else begin
        // Get random measurement unit
        UX1 := Random(UnitCounts[iMeasure]) + 1;
        // Exercises including international units limited to conversion to the one unit-pairs defined in the (constant) measurements arrays
        UX2 := UX1;
        if mSettingsNames.Checked then begin
          Unit1 := UnitNames[iMeasure, UX1]; Unit2 := IntUnitName(IntUnitConversionSymbols[iMeasure, UX2]);
        end
        else begin
          Unit1 := UnitSymbols[iMeasure, UX1]; Unit2 := IntUnitConversionSymbols[iMeasure, UX2];
        end;
        Value1 := 1;
        // Randomly choose between American/British to International or International to American/British conversion
        if Random(2) = 0 then begin
          rValue2 := MeasurementConversion(iTest, iMeasure, -1, UX2, Value1);
        end
        else begin
          Temp := Unit1; Unit1 := Unit2; Unit2 := Temp;
          rValue2 := MeasurementConversion(iTest, iMeasure, UX1, -1, Value1);
        end;
      end;
      // Get random value, but arbitrarily choose what digits may appear after the decimal point
      V := Random(6);
      case V of
        0: Value1 := (Random(9) + 1) / 1000;
        1: Value1 := Random(10) / 100 + (Random(2) + 1) * 5 / 1000;
        2: Value1 := Random(10) / 10 + (Random(4) + 1) * 2.5 / 100;
        3: Value1 := Random(10) + 1 + Random(10) / 10 + (Random(4) + 1) * 2.5 / 100;
        4: Value1 := Random(90) + 10 + Random(10) / 10 + (Random(4) + 1) * 2.5 / 100;
        5: Value1 := Random(900) + 100 + Random(10) / 10 + (Random(2) + 1) * 5 / 100;
      end;
      // Determine at which decimal position the value should be rounded
      if V = 5 then
        Round1 := 100
      else
        Round1 := 1000;
      // Calculate the 2nd (= the converted) value
      rValue2 *= Value1;
      // Determine at which decimal position the converted value should be rounded
      // This seems to work rather fine ???
      Round2 := Round1; Value := MeasurementConversion(iTest, iMeasure, UX1, UX2, 1);
      if Value >= 1 then begin
        repeat
          Value /= 10; Round2 /= 10;
        until Value < 1;
      end
      else begin
        repeat
          Value *= 10; Round2 *= 10;
        until Value > 1;
      end;
      if Round2 <= 1000 then
        Round2 := 1000;
      // If the converted value is bigger than 1e+4 or smaller than 1e-4,
      // arbitrarily use smaller resp. bigger values (to avoid "to small" or "to big" numbers )
      if rValue2 < 1E-4 then begin
        Value1 *= 1000; rValue2 *= 1000;
        Round1 := Round1 / 1000; Round2 := Round2 / 1000;                                // adapt the position where the values must be rounded
      end
      else if rValue2 > 1E+4 then begin
        Value1 /= 1000; rValue2 /= 1000;
        Round1 := Round1 * 1000; Round2 := Round2 * 1000;                                // adapt the position where the values must be rounded
      end;
      // Round the values; this is important to avoid erroneous digits after the
      // decimal point (and also to get a proper display)
      Value1 := Round(Round1 * Value1) / Round1;
      rValue2 := Round(Round2 * rValue2) / Round2;
      // Consider plural for full units names
      if mSettingsNames.Checked then begin
        if Value1 > 1 then begin
          Unit1 += 's';
          Unit1 := StringReplace(Unit1, 'foots', 'feet', []);
          Unit1 := StringReplace(Unit1, 'inchs', 'inches', []);
        end;
        Unit2 += '(s)';
        Unit2 := StringReplace(Unit2, 'foot(s)', 'foot/feet', []);
        Unit2 := StringReplace(Unit2, 'inch(s)', 'inch(es)', []);
      end;
      // Display the exercise question
      edValue1.Text := FloatToStr(Value1);
      edEval.Text := ''; edEval.Color := clCream;
      Unit1 := StringReplace(Unit1, '2', SUP_2, []); Unit2 := StringReplace(Unit2, '2', SUP_2, []);
      Unit1 := StringReplace(Unit1, '3', SUP_3, []); Unit2 := StringReplace(Unit2, '3', SUP_3, []);
      edUnit1.Text := Unit1; edUnit2.Text := Unit2;
      // Determine number of decimal digits to be considered when evaluating pupil's answer
      // This also seems to work rather fine ???
      rRound2 := 1;
      repeat
        Round2 /= 10;
        if Round2 >= 1 then
          rRound2 *= 10;
      until Round2 <= 1;
      edValue2.Text := '';
      edValue2.SetFocus;
      // Next button push will be to check pupil's answer
      btQuestion.Caption := 'Answer';
    end;
  end
  // Button "Answer": Check pupil's answer
  // -------------------------------------
  else begin
    edQuestions.Text := IntToStr(iQuestion);
    // Get pupil's answer from form
    if edValue2.Text = '' then
      UValue2 := -1
    else
      UValue2 := StrToFloat(edValue2.Text);
    // Pupil's answer is correct
    if Round(rRound2 * UValue2) = Round(rRound2 * rValue2) then begin
      Inc(iCorrect);
      edEval.Text := 'Correct!';
      edEval.Color := clCream;
    end
    // Pupil's answer is false
    else begin
      Inc(iFalse);
      edEval.Text := 'False! Correct answer = ' + FloatToStr(rValue2);
      edEval.Color := clRed;
    end;
    // Update evaluation table
    edCorrect.Text := IntToStr(iCorrect); edFalse.Text := IntToStr(iFalse);
    Success := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Success) + '%';
    if Success >= 60 then
      edSuccess.Color := clLime
    else if Success >= 50 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clRed;
    // Next button push is to generate a new question
    btQuestion.Caption := 'Question';
    // All questions done: Display message (and disable button, until a new test has been selected )
    if iQuestion = iQuestions then begin
      MessageDlg('End of test', 'All questions of this test have been done.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Selection of actual measurement (to do exercise about) }

procedure TfConverter2.cobMeasureChange(Sender: TObject);

begin
  iSMeasure := cobMeasure.ItemIndex;                                           // combobox itemindex will be transformed to measurements array index when a new test is started
end;

end.

