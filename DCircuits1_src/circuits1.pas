{****************************************}
{*    Electrical circuits: Ohm's Law    *}
{*--------------------------------------*}
{* Simple physics problems generator    *}
{* Main unit for DCircuits1 application *}
{****************************************}

unit circuits1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, PopupNotifier, help;

type
  { TDCircuits1Form }
  TDCircuits1Form = class(TForm)
    MenuDCircuits1: TMainMenu;
    MenuTest: TMenuItem;
    MenuTestNew: TMenuItem;
    MenuTestExit: TMenuItem;
    MenuCircuits: TMenuItem;
    MenuCircuitsSimple: TMenuItem;
    MenuCircuitsSeries: TMenuItem;
    MenuCircuitsParallel: TMenuItem;
    MenuOptions: TMenuItem;
    MenuOptionsView: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpPhysics: TMenuItem;
    MenuHelpProgram: TMenuItem;
    MenuHelpAbout: TMenuItem;
    Title: TStaticText;
    Image: TImage;
    Label1: TLabel;  EditU: TEdit;
    Label2: TLabel;  EditI: TEdit;
    Label3: TLabel;  EditR1: TEdit;
    Label4: TLabel;  EditR2: TEdit;
    Label5: TLabel;  EditR: TEdit;
    Label6: TLabel;  EditU1: TEdit;
    Label7: TLabel;  EditI1: TEdit;
    Label8: TLabel;  EditU2: TEdit;
    Label9: TLabel;  EditI2: TEdit;
    Label10: TLabel; AnswersCorrect: TEdit;
    Label11: TLabel; AnswersSuccess: TEdit;
    AnswersPic: TImage;
    Button: TButton;
    PopupAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure MenuTestNewClick(Sender: TObject);
    procedure MenuTestExitClick(Sender: TObject);
    procedure MenuCircuitsSimpleClick(Sender: TObject);
    procedure MenuCircuitsSeriesClick(Sender: TObject);
    procedure MenuCircuitsParallelClick(Sender: TObject);
    procedure MenuOptionsViewClick(Sender: TObject);
    procedure MenuHelpPhysicsClick(Sender: TObject);
    procedure MenuHelpProgramClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
  end;

type
  TInputFields   = array[1..9] of TEdit;
  TCircuitValues = array[1..9] of Real;
  TProgramValues = array[1..9] of Boolean;

const
  Circuits: array[1..3] of string = ('simple', 'series', 'parallel');

var
  DCircuits1Form: TDCircuits1Form;
  InputFields: TInputFields;
  CValues: TCircuitValues;
  PValues: TProgramValues;
  CircuitsUsed: array[1..3] of Boolean;
  Question, Correct: Integer;
  U, U1, U2, I, I1, I2, R, R1, R2, Success: Real;
  AppStart: Boolean;

implementation

{$R *.lfm}

{ Start a new test }

procedure NewTest;

begin
  // Clear evaluation values
  Question := 0; Correct := 0;
  DCircuits1Form.AnswersCorrect.Clear; DCircuits1Form.AnswersSuccess.Clear;
  DCircuits1Form.AnswersCorrect.Color := clDefault; DCircuits1Form.AnswersSuccess.Color := clDefault;
  // Execute a "click Start button" action
  DCircuits1Form.Button.Caption := 'Start';
  DCircuits1Form.Button.Click;
end;

{ Clear the form }

procedure ClearForm;

begin
  U := 0; U1 := 0; U2 := 0; I := 0; I1 := 0; I2 := 0; R := 0; R1 := 0; R2 := 0;
  DCircuits1Form.EditU.Clear; DCircuits1Form.EditU1.Clear; DCircuits1Form.EditU2.Clear;
  DCircuits1Form.EditI.Clear; DCircuits1Form.EditI1.Clear; DCircuits1Form.EditI2.Clear;
  DCircuits1Form.EditR.Clear; DCircuits1Form.EditR1.Clear; DCircuits1Form.EditR2.Clear;
  DCircuits1Form.EditU.Font.Color := clDefault; DCircuits1Form.EditU1.Font.Color := clDefault; DCircuits1Form.EditU2.Font.Color := clDefault;
  DCircuits1Form.EditI.Font.Color := clDefault; DCircuits1Form.EditI1.Font.Color := clDefault; DCircuits1Form.EditI2.Font.Color := clDefault;
  DCircuits1Form.EditR.Font.Color := clDefault; DCircuits1Form.EditR1.Font.Color := clDefault; DCircuits1Form.EditR2.Font.Color := clDefault;
  DCircuits1Form.AnswersPic.Picture.Clear;
end;

{ Form-fill-in the values for the actual exercise }

procedure FillForm;

var
  J: Integer;

begin
  // Do this for all exercise values
  for J := 1 to 9 do begin
    // Disabled values (U2, I2, R and R2 with simple circuits)
    if CValues[J] = 0 then begin
      InputFields[J].Color := clDefault;
      InputFields[J].Enabled := False;
    end
    // Enabled values (normal case)
    else begin
      InputFields[J].Enabled := True;
      // Value filled-in by program
      if PValues[J] then begin
        InputFields[J].Text := FloatToStr(CValues[J]);                                   // display the value
        InputFields[J].Color := clSilver;                                                // mark the value as program-filled-in
        InputFields[J].ReadOnly := True;                                                 // disable editing of this value
        InputFields[J].TabStop := False;
      end
      // Value to be filled in by user (or shown if "View results" is checked)
      else begin
        InputFields[J].Color := clDefault;
        // "View results" is checked
        if DCircuits1Form.MenuOptionsView.Checked then begin
          InputFields[J].Text := FloatToStr(CValues[J]);                                 // display the value
          InputFields[J].ReadOnly := False;                                              // disable editing
          InputFields[J].TabStop := True;
        end
        // "View results" is not checked (value has to be entered by user)
        else begin
          InputFields[J].ReadOnly := False;                                              // allow value entry
          InputFields[J].TabStop := True;
        end;
      end;
    end;
  end;
end;

{ Create an array indicating those values that have been filled in by the program}

procedure PValuesFill(PU, PI, PR1, PR2, PR, PU1, PI1, PU2, PI2: Boolean);

begin
  PValues[1] := PU;  PValues[2] := PI;  PValues[3] := PR1; PValues[4] := PR2; PValues[5] := PR;
  PValues[6] := PU1; PValues[7] := PI1; PValues[8] := PU2; PValues[9] := PI2;
end;

{ Random voltage value }

function Voltage: Real;

begin
  Voltage := Random(31) + 6;                                                             // U between 6 and 36 volts
end;

{ Random current value }

function Current(U: Real; Circuit: Integer): Real;

var
  Count: Integer;
  I: Real;
  OK: Boolean;

begin
  count := 0;
  repeat
    Inc(Count);
    I := (Random(291) + 10) / 100;                                                       // current between 0.10 and 3.00 amps
    if Circuit = 2 then begin                                                            // use "smaller" current for series circuits
      if I > 2 then
        I -= 1;
    end
    else if Circuit = 3 then begin                                                       // use "bigger" current for parallel circuits
      if I < 1 then
        I += 1;
    end;
    OK := True;
    if I = 1 then                                                                        // do not use I = 1 amp
      OK := False
    else begin
      if U <> 0 then begin
        // Current value depending on voltage already calculated
        if (U = I) or (U / I <> Int(U / I)) then                                         // do not use I = U; resistance should be an integer
          OK := False;
      end;
      if Count > 2500 then                                                               // to be sure to avaoid infinite loops
        I := 0;                                                                          // set I to "error" (sub will be called again)
    end;
  until OK or (Count > 2500);
  Current := I;
end;

{ Random resistance value }

function Resistance(U, I: Real; Circuit: Integer): Real;

var
  Count: Integer;
  R: Real;
  OK: Boolean;

begin
  Count := 0;
  repeat
    Inc(count);
    R := Random(239) + 2;                                                                // resistance between 2 and 240 ohms
    if Circuit = 2 then                                                                  // use "bigger" resistance for parallel circuit
      if R < 10 then
        R *= 10;
    OK := True;
    if (U <> 0) or (I <> 0) then begin
      // Resistance value without dependencies
      if (R > 100) and (R / 10 <> Int(R / 10)) then                                      // use "round" values
        OK := False
      else begin
        // Resistance depending on values already calculated
        if U <> 0 then begin
          // Resistance depending on voltage already calculated
          if (U = R) or (100 * U / R <> Int(100 * U / R)) then                           // do not use R = U; current should not have more than 2 decimal digits
            OK := False;
        end
        else if I <> 0 then begin
          // Resistance depending on current already calculated
          if (I * R <> Int(I * R)) or (I * R > 36) then                                  // voltage should be an integer and should not exceed 36V
            OK := False;
        end;
      end;
    end;
    if Count > 2500 then                                                                 // to be sure to avaoid infinite loops
      R := 0;                                                                            // set R to "error" (sub will be called again)
  until OK or (Count > 2500);
  Resistance := R;
end;

{ Individual resistances for series circuit }

procedure SeriesResistances(I, R: Real; var R1, R2: Real);

var
  Count: Integer;

begin
  Count := 0;
  repeat
    Inc(Count);
    R1 := Random(Trunc(R - 1)) + 1; R2 := R - R1;                                        // R1 between 1 and R - 1 (R2 by series circuit formula)
    if Count > 2500 then begin                                                           // to be sure to avaoid infinite loops
      R1 := 0; R2 := 0;                                                                  // set R to "error" (calculation will be done again)
    end;
  until (Count > 2500) or
        (100 * R1 * I = Int(100 * R1 * I)) and (100 * R2 * I = Int(100 * R2 * I));       // both voltages should not have more than 2 decimal digits
end;

{ Individual resistances for parallel circuit }

procedure ParallelResistances(U, R: Real; var R1, R2: Real);

var
  Count: Integer;

begin
  Count := 0;
  repeat
    repeat
      R1 := Int(Random(Round(8.9 * R)) + 1.1 * R);                                       // R1 between 1.1 times and 10 times the values of R
    until R1 > R;                                                                        // R1 must be greater than R!
    R2 := 1 / ((1 / R) - (1 / R1));                                                      // R2 as given by parallel circuit formula
    if Count > 2500 then begin
      R1 := 0; R2 := 0;                                                                  // set R to "error" (calculation will be done again)
    end;
  until (Count > 2500) or ((R1 = Int(R1)) and (R2 = Int(R2))                             // both resistances should be integer numbers
    and (100 * U / R1 = Int(100 * U / R1)) and (100 * U / R2 = Int(100 * U / R2)));      // both currents should not have more than 2 decimal digits
end;

{ =============== }
{ TDCircuits1Form }
{ =============== }


{ Application start: Initialisation }

procedure TDCircuits1Form.FormCreate(Sender: TObject);

var
  J: Integer;

begin
  // Use all 3 types of circuit
  for J := 1 to 3 do
    CircuitsUsed[J] := True;
  // Put form input fields into an array
  InputFields[1] := EditU;  InputFields[2] := EditI;  InputFields[3] := EditR1; InputFields[4] := EditR2; InputFields[5] := EditR;
  InputFields[6] := EditU1; InputFields[7] := EditI1; InputFields[8] := EditU2; InputFields[9] := EditI2;
  // Init and start test (with default settings)
  ClearForm;
  Randomize;
  AppStart := True;
  NewTest;
end;

{ Menu item "Test > New": Start a new test }

procedure TDCircuits1Form.MenuTestNewClick(Sender: TObject);

begin
  NewTest;
end;

{ Menu item "Test > Exit": Exit the application }

procedure TDCircuits1Form.MenuTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Circuits > Simple": (Un)select simple circuits for test questions }

procedure TDCircuits1Form.MenuCircuitsSimpleClick(Sender: TObject);

begin
  if MenuCircuitsSimple.Checked then
    MenuCircuitsSimple.Checked := False
  else
    MenuCircuitsSimple.Checked := True;
  CircuitsUsed[1] := MenuCircuitsSimple.Checked;
end;

{ Menu item "Circuits > Series": (Un)select series circuits for test questions }

procedure TDCircuits1Form.MenuCircuitsSeriesClick(Sender: TObject);

begin
  if MenuCircuitsSeries.Checked then
    MenuCircuitsSeries.Checked := False
  else
    MenuCircuitsSeries.Checked := True;
  CircuitsUsed[2] := MenuCircuitsSeries.Checked;
end;

{ Menu item "Circuits > Parallel": (Un)select parallel circuits for test questions }

procedure TDCircuits1Form.MenuCircuitsParallelClick(Sender: TObject);

begin
  if MenuCircuitsParallel.Checked then
    MenuCircuitsParallel.Checked := False
  else
    MenuCircuitsParallel.Checked := True;
  CircuitsUsed[3] := MenuCircuitsParallel.Checked;
end;

{ Menu item "Options > View results": Adapt form controls depending on test/view mode }

procedure TDCircuits1Form.MenuOptionsViewClick(Sender: TObject);

begin
  // Change to test mode
  if DCircuits1Form.MenuOptionsView.Checked then begin
    DCircuits1Form.MenuOptionsView.Checked := False;
    // Proceed with next exercise as test
    if Button.Caption = 'View' then
      Button.Caption := 'Next';
  end
  // View mode
  else begin
    DCircuits1Form.MenuOptionsView.Checked := True;
    // Clear evaluation values on the form (but keep actual values)
    DCircuits1Form.AnswersCorrect.Clear; DCircuits1Form.AnswersSuccess.Clear;
    DCircuits1Form.AnswersCorrect.Color := clDefault; DCircuits1Form.AnswersSuccess.Color := clDefault;
    // Setting the button to "View" will allow to view the results of the actual (unsolved) exercise
    if Button.Caption = 'Check' then
      Button.Caption := 'View';
  end;
end;

{ Menu item "Help > Physics": Display physics (Ohm's Law) help }

procedure TDCircuits1Form.MenuHelpPhysicsClick(Sender: TObject);

begin
  HelpForm.Caption := 'Ohm''s Law - Physics Help.';
  HelpForm.HelpText.Lines.LoadFromFile('help1.txt');                                     // load help text from file
  HelpForm.Show;                                                                         // display help (as a form)
end;

{ Menu item "Help > Program": Display program usage help }

procedure TDCircuits1Form.MenuHelpProgramClick(Sender: TObject);

begin
  HelpForm.Caption := 'Ohm''s Law - Program Help.';
  HelpForm.HelpText.Lines.LoadFromFile('help2.txt');                                     // load help text from file
  HelpForm.Show;                                                                         // display help (as a form)
end;

{ Menu item "Help > About": Display program info }

procedure TDCircuits1Form.MenuHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if PopupAbout.Visible then
    PopupAbout.Visible := False
  else begin
    S := 'Electrical circuits:  Ohm''s Law.' + Chr(13);                                  // Chr(13) is LF/CR
    S += 'Simple physics problems generator.' + Chr(13) + Chr(13);
    S += '© allu, January, 2018.';
    PopupAbout.Text := S;
    PopupAbout.Visible := True;
  end;
end;

{ Action button click: main program action, depending on actual situation }

procedure TDCircuits1Form.ButtonClick(Sender: TObject);

var
  CType, QType, J: Integer;
  UValue, P: Real;
  Filename, S: string;
  OK, ValuesOK: Boolean;

begin
  // At least one circuit type must be selected
  if CircuitsUsed[1] or CircuitsUsed[2] or CircuitsUsed[3] then begin
    // Generate a new exercise
    if (Button.Caption = 'Start') or (Button.Caption = 'Next') then begin
      // Circuit type (simple, series or parallel)
      repeat
        CType := Random(3) + 1;
      until CircuitsUsed[CType];                                                         // circuit type must have been selected to be used
      S := 'Ohm''s Law: ' + Circuits[CType] + ' circuit'; Title.Caption := S;
      ClearForm;                                                                         // clear form input values
      // Problem type
      if CType = 1 then                                                                  // simple circuit (3 question types)
        QType := Random(3)
      else                                                                               // series or parallel circuit (5 question types)
        QType := Random(5);
      // Circuit total values
      repeat
        ValuesOK := True;
        repeat
          case QType of
            0, 3:
              begin
                U := Voltage; I := Current(U, CType); R := U / I;                        // U and I as random values
              end;
            1:
              begin
                U := Voltage; R := Resistance(U, 0, CType); I := U / R;                  // U and R as random values
              end;
            2, 4:
              begin
                I := Current(0, CType); R := Resistance(0, I, CType); U := R * I;        // I and R as random values
              end;
          end;
        until (I <> 0) and (R <> 0);                                                     // these have been set to 0 if no suitable random value could be found
        // Simple circuit
        if CType = 1 then begin
          EditU2.Enabled := False; EditI2.Enabled := False; EditR.Enabled := False; EditR2.Enabled := False;
          // Calculation of individual values (0 indicates these values have to be disabled, as not used with simple circuits )
          R1 := R; R  := 0; R2 := 0;
          U1 := U; I1 := I;
          U2 := 0; I2 := 0;
          // Mark values to be displayed (= not to be entered by user)
          case QType of
            0: PValuesFill(True, True, False, False, False, False, False, False, False);
            1: PValuesFill(True, False, True, False, False, False, False, False, False);
            2: PValuesFill(False, True, True, False, False, False, False, False, False);
          end;
        end
        // Series or parallel circuit
        else begin
          EditU2.Enabled := True; EditI2.Enabled := True; EditR2.Enabled := True; EditR.Enabled  := True;
          // Calculate individual circuit values
          if CType = 2 then                                                              // series circuit
            SeriesResistances(I, R, R1, R2)
          else                                                                           // parallel circuit
            ParallelResistances(U, R, R1, R2);
          if (R1 > 0) and (R2 > 0) then begin                                            // these have been set to 0 if no suitable random value could be found
            if CType = 2 then begin                                                      // series circuit
              U1 := R1 * I; U2 := R2 * I;
              I1 := I; I2 := I;
            end
            else begin                                                                   // parallel circuit
              U1 := U; U2 := U;
              I1 := U1 / R1; I2 := U2 / R2;
            end;
            // Mark values to be displayed (= not to be entered by user)
            case QType of
              0: PValuesFill(True, True, True, False, False, False, False, False, False);
              1: PValuesFill(True, False, True, True, False, False, False, False, False);
              2: PValuesFill(False, True, True, True, False, False, False, False, False);
              3: if CType = 2 then                                                           // for series circuits, show total current and partial voltages
                   PValuesFill(False, True, False, False, False, True, False, True, False)
                 else                                                                        // for parallel circuits, show total voltage and partial currents
                   PValuesFill(True, False, False, False, False, False, True, False, True);
              4: if CType = 2 then                                                           // for series circuits, show R1 and partial voltages
                   PValuesFill(False, False, True, False, False, True, False, True, False)
                 else                                                                        // for parallel circuits, show R1 and partial currents
                   PValuesFill(False, False, True, False, False, False, True, False, True);
            end;
          end;
        end;
      until ValuesOK;
      // Draw the circuit (= load image file)
      Filename := Circuits[CType] + '.png';
      Image.Picture.LoadFromFile(Filename);
      // Fill the form (= display the values shown by the program)
      CValues[1] := U;  CValues[2] := I;  CValues[3] := R1; CValues[4] := R2; CValues[5] := R;
      CValues[6] := U1; CValues[7] := I1; CValues[8] := U2; CValues[9] := I2;
      FillForm;
      // Set button capture depending on "View" = on/off
      if MenuOptionsView.Checked then begin
        Button.SetFocus;
        Button.Caption := 'Next';
      end
      else begin
        if AppStart then                                                                 // to avoid "cant't focus" error at application start-up
          AppStart := False
        else begin
          for J := 9 downto 1 do begin
            if InputFields[J].Enabled and not PValues[J] then
              InputFields[J].SetFocus;
          end;
        end;
        Button.Caption := 'Check';
      end;
    end
    // Check user's answers or display exercise results (depending on "View" = on/off)
    else begin
      // Show exercise results
      if Button.Caption = 'View' then begin
        for J := 1 to 9 do begin
          if CValues[J] <> 0 then
            InputFields[J].Text  := FloatToStr(CValues[J]);
        end;
      end
      // Check user's answer
      else begin
        Inc(Question);
        OK := True;
        for J := 1 to 9 do begin
          if InputFields[J].Text = '' then                                               // to avoid error message when field is left blank
            UValue := 0
          else
            UValue := StrToFloat(InputFields[J].Text);
          if UValue <> CValues[J] then begin
            // False answer
            InputFields[J].Text := FloatToStr(CValues[J]);                               // display correct answer
            InputFields[J].Font.Color := clRed;                                          // use red font color to indicate there was a false answer
            OK := False;
          end;
        end;
        // Display global result (evaluation)
        if OK then begin
          // Answer = correct
          Inc(Correct);
          AnswersPic.Picture.LoadFromFile('correct.png');
        end
        else
          // Answer = false
          AnswersPic.Picture.LoadFromFile('false.png');
        S := IntToStr(Correct) + ' / ' + IntToStr(Question);
        AnswersCorrect.Text := S;
        // Success percentage
        P := Round(100 * (100 * Correct / Question)) / 100;
        AnswersSuccess.Text := FloatToStr(P) + '%';
        // Use different colors depending on success percenage
        if P < 50 then
          AnswersSuccess.Color := clRed
        else if P < 60 then
          AnswersSuccess.Color := clYellow
        else
          AnswersSuccess.Color := clLime;
      end;
      // Set button caption and focus (for next exercise)
      Button.Caption := 'Next';
      Button.SetFocus;
    end;
  end
  // No circuit type selected
  else
    MessageDlg('Invalid settings','No circuit type selected!', mtError, [mbOK], 0);
end;

end.

