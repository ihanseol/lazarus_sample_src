{************************************}
{* Main unit for Kidney application *}
{************************************}

unit kidney_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, kidney_help;

type
  TValues = array[0..2, 0..9] of Real;
  TEditFields = array[0..2, 0..9] of TEdit;
  {**********}
  { TfKidney }
  {**********}
  TfKidney = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileSample, mFileExit: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Memo1, Memo2: TMemo;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18, Label19, Label20: TLabel;
    Label21, Label22, Label23, Label24, Label25, Label26, Label27, Label28, Label29, Label30: TLabel;
    Label31, Label32, Label33, Label34, Label35, Label36, Label37, Label38, Label39, Label40: TLabel;
    Label41, Label42, Label43, Label44, Label45, Label46, Label47: TLabel;
    edPA1, edPA2, edPA3, edPV1, edPV2, edPV3: TEdit;
    edU1, edU2, edU3, edV1, edV2, edV3: TEdit;
    edC1, edC2, edC3, edE1, edE2, edE3: TEdit;
    edGFR, edFF, edHct, edRPF, edRBF: TEdit;
    edFiltered1, edFiltered2, edFiltered3, edSecreted1, edSecreted2, edSecreted3: TEdit;
    edReasorbed1, edReasorbed2, edReasorbed3, edUrine1, edUrine2, edUrine3: TEdit;
    edClearance, edExtraction, edUrine: TMemo;
    btCompute: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileSampleClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btComputeClick(Sender: TObject);
  private
    rHct, rGFR, rRPF, rRBF, rFF: Real;
    aValues: TValues;
    edValues: TEditFields;
  end;

var
  fKidney: TfKidney;

implementation

{$R *.lfm}

{ Format real number with given number of decimal digits }

function RFormat(R: Real; F: Integer): string;

var
  L: Cardinal;
  SR: string;

begin
  if R = 0 then
    SR := '0'
  else begin
    if F = -1 then begin
      // Use default number of decimal digits (normally 3)
      if R < 0.001 then
        SR := FloatToStrF(R, ffFixed, 0, 4)
      else
        SR := FloatToStrF(R, ffFixed, 0, 3);
    end
    else begin
      // Use number of decimal digits specified
      SR := FloatToStrF(R, ffFixed, 0, F);
    end;
    // Remove non-significant zeros
    repeat
      L := Length(SR);
      if SR[L] = '0' then
        Delete(SR, Length(SR), 1);
    until SR[Length(SR)] <> '0';
  end;
  // Remove decimal point (if nothing left at the right of it)
  if (RightStr(SR, 1) = '.') or (RightStr(SR, 1) = ',') then
    Delete(SR, Length(SR), 1);
  Result := SR;
end;

{ Clear form controls }

procedure ClearAll(var Values: TValues; var ValuesFields: TEditFields; All: Boolean);

var
  I, J: Cardinal;

begin
  for I := 0 to 2 do begin
    for J := 0 to 9 do begin
      Values[I, J] := 0;
      if (All) or ((not All) and (J > 3)) then
        // Clear user input fields only if All = True
        ValuesFields[I, J].Text := '';
    end;
  end;
  fKidney.edGFR.Text := ''; fKidney.edClearance.Lines.Clear;
  if All then
    // Clear Hct (user input field) only if All = True
    fKidney.edHct.Text := '';
  fKidney.edRPF.Text := ''; fKidney.edRBF.Text := '';
  fKidney.edFF.Text := '';  fKidney.edFF.Color := clDefault; fKidney.edExtraction.Lines.Clear;
end;

{ Read user values (measurements) from form }

procedure ReadValues(out Values: TValues; var ValuesFields: TEditFields; out Hct: Real; out Mess: string);

// There is no validation checking for values being a biological reality!

const
  SValues: array[0..3] of string = (
    'PA', 'PV', 'U', 'V'
  );

var
  Blanks, I, J: Cardinal;

begin
  Mess := '';
  for I := 0 to 2 do begin
    Blanks := 0;
    for J := 0 to 3 do begin
      if ValuesFields[I, J].Text = '' then
        Values[I, J] := 0
      else
        Values[I, J] := StrToFloat(ValuesFields[I, J].Text);
      if Values[I, J] <= 0 then begin
        if I = 0 then begin
          // For solute 1: All values must be filled in
          if Mess = '' then begin
            Mess := SValues[J] + ' must be greater than zero';
            ValuesFields[I, J].SetFocus;
          end;
        end
        else begin
          // For other solutes: Zero values accepted (if is the case for all of them; cf. further down)
          if Values[I, J] = 0 then
            Inc(Blanks)
          else begin
            if Mess = '' then begin
              Mess := SValues[J] + ' must be greater than zero';
              ValuesFields[I, J].SetFocus;
            end;
          end;
        end;
      end;
    end;
    if (ValuesFields[I, 0].Text <> '') and (ValuesFields[I, 1].Text <> '') and (Values[I, 1] >= Values[I, 0]) then begin
      // Concentration of solute in renal vein has to be smaller than the one in artery
      if Mess = '' then begin
        Mess := SValues[1] + ' must be less than ' + SValues[0];
        ValuesFields[I, 1].SetFocus;
      end;
    end;
    // For solutes 2 and 3: All values filled in or all fields blank (values = 0)
    if (Blanks <> 0) and (Blanks <> 4) then begin
      if Mess = '' then begin
        Mess := 'Missing measurment values for solute ' + IntToStr(I + 1);
        ValuesFields[I, 0].SetFocus;
      end;
    end;
  end;
  if fKidney.edHct.Text = '' then begin
    // Accept blank field for hematocrit
    MessageDlg('Incomplete data', 'Can''t calculate RBF without knowing hematocrit!', mtWarning, [mbOK], 0);
    Hct := 0;
  end
  else begin
    Hct := StrToFloat(fKidney.edHct.Text);
    if (Mess = '') and (Hct <= 0) then begin
      Mess := 'Hematocrit must be greater than zero';
      fKidney.edHct.SetFocus;
    end;
  end;
  // Display message if user data is invalid
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Display calculated values }

procedure DisplayValues(var Values: TValues; var ValuesFields: TEditFields; First, Last: Cardinal);

// Values displayed here are elements First...Last of two-dimensional array (other values are displayed in calculation method )

var
  I, J: Cardinal;
  LetBlank: Boolean;

begin
  for I := 0 to 2 do begin
    for J := First to Last do begin
      LetBlank := False;
      if J in [4..5] then begin
        // Clearance and extraction ratio: Do not display zero values (they mean, that this solute has not been used...)
        if Values[I, J] = 0 then
          LetBlank := True;
      end
      else if J in [6..9] then begin
        // Do not display filtered amount zero value (it means, that this solute has not been used...)
        if Values[I, 6] = 0 then
          LetBlank := True;
      end;
      if not LetBlank then
        // Display value, unless field has to be left blank
        ValuesFields[I, J].Text := RFormat(Values[I, J], -1);
    end;
  end;
end;

{**********}
{ TfKidney }
{**********}

{ Application start: Initialisation }

procedure TfKidney.FormCreate(Sender: TObject);

begin
  // Create 2-dimensioanl array with edit fields: first index for solute, 2nd index for value
  edValues[0, 0] := edPA1; edValues[0, 1] := edPV1; edValues[0, 2] := edU1;
  edValues[0, 3] := edV1; edValues[0, 4] := edC1;  edValues[0, 5] := edE1;
  edValues[0, 6] := edFiltered1;  edValues[0, 7] := edUrine1;
  edValues[0, 8] := edReasorbed1; edValues[0, 9] := edSecreted1;
  edValues[1, 0] := edPA2; edValues[1, 1] := edPV2; edValues[1, 2] := edU2;
  edValues[1, 3] := edV2; edValues[1, 4] := edC2;  edValues[1, 5] := edE2;
  edValues[1, 6] := edFiltered2;  edValues[1, 7] := edUrine2;
  edValues[1, 8] := edReasorbed2; edValues[1, 9] := edSecreted2;
  edValues[2, 0] := edPA3; edValues[2, 1] := edPV3; edValues[2, 2] := edU3;
  edValues[2, 3] := edV3; edValues[2, 4] := edC3;  edValues[2, 5] := edE3;
  edValues[2, 6] := edFiltered3;  edValues[2, 7] := edUrine3;
  edValues[2, 8] := edReasorbed3; edValues[2, 9] := edSecreted3;
  mFileNew.Click;
end;

{ Menu item "File > New": New calculation (clear all form fields) }

procedure TfKidney.mFileNewClick(Sender: TObject);

begin
  ClearAll(aValues, edValues, True);
end;

{ Menu item "File > Sample": Fill user entry fields with sample data }

procedure TfKidney.mFileSampleClick(Sender: TObject);

const
  Values: array[0..2, 0..3] of Real = (
   // PA, PV (in mg/100ml), U (in mg/ml), V (in ml/min)
   ( 1.0, 0.8, 0.45, 2.5 ),
   ( 1.0, 0.6, 0.55, 2.5 ),
   ( 1.0, 0.9, 0.3, 2.5 )
  );
  Hct = 0.50;

var
  I, J: Cardinal;

begin
  ClearAll(aValues, edValues, True);
  for I := 0 to 2 do begin
    for J := 0 to 3 do begin
      if J <= 1 then
        edValues[I, J].Text := RFormat(Values[I, J] / 100, -1)
      else
        edValues[I, J].Text := RFormat(Values[I, J], -1);
    end;
  end;
  edHct.Text := RFormat(Hct, -1);
  btCompute.SetFocus;
end;

{ Menu item "File > Exit": Exit application }

procedure TfKidney.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display application help }

procedure TfKidney.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfKidney.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Renal physiology:' + LineEnding;
  S += 'Calculation of glomerular filtration rate, renal blood flow, filtration rate and other values.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, December 2020.';
  MessageDlg('About "Kidney"', S, mtInformation, [mbOK], 0);
end;

{ Button "Compute": Compute renal physiology values }

procedure TfKidney.btComputeClick(Sender: TObject);

var
  I: Cardinal;
  PA, PV, U, V: Real;
  Mess, S: string;

begin
  ClearAll(aValues, edValues, False);
  ReadValues(aValues, edValues, rHct, Mess);                                   // read user values from form
  if Mess = '' then begin
    // Proceed only if user values are valid
    for I := 0 to 2 do begin
      PA := aValues[I, 0]; PV := aValues[I, 1];
      U  := aValues[I, 2]; V := aValues[I, 3];
      if PA > 0 then begin
        // Do calculation only, if this solute is actually used
        aValues[I, 4] := U * V / PA;                                           // clearance
        if I = 0 then
          rGFR := aValues[I, 4];                                               // glomerular filtration rate (1st solute supposed only filtered)
        aValues[I, 5] := (PA - PV) / PA;                                       // extraction ratio
      end;
    end;
    DisplayValues(aValues, edValues, 4, 5);                                    // display clearances and extraction ratios
    edGFR.Text := RFormat(rGFR, -1);                                           // display GFR
    edClearance.Lines.Clear;
    for I := 1 to 2 do begin
      // For solute 2 and 3: Determine if secretion/reabsorbtion based on clearance value
      if aValues[I, 4] <> 0 then begin
        S := 'For solute ' + IntToStr(I + 1) + ', ';
        if aValues[I, 4] < rGFR then
          S += 'C < GFR: solute is filtered and reabsorbed'
        else if aValues[I, 4] > rGFR then
          S += 'C > GFR: solute is filtered and secreted'
        else
          S += 'C = GFR: solute is only filtered';
        edClearance.Lines.AddText(S);
      end;
    end;
    rFF := aValues[0, 5]; edFF.Text := RFormat(rFF, -1);                        // filtration fraction (= extraction ratio of solute 1)
    if (rFF >= 0.15) and (rFF <= 0.2) then                                      // normal filtration fraction values for humans
      edFF.Color := clLime
    else if (rFF >= 0.1) and (rFF <= 0.25) then                                 // FF outside of normal range
      edFF.Color := clYellow
    else                                                                        // unrealistic FF values (?)
      edFF.Color := clRed;
    rRPF := rGFR / rFF; edRPF.Text := RFormat(rRPF, 0);                         // renal plasma flow
    if rHct <> 0 then begin
      // Calculate RBF only if user has specified a value for Hct
      rRBF := rRPF * (1 / (1 - rHct)); edRBF.Text := RFormat(rRBF, 0);         // renal blood flow
    end;
    edExtraction.Lines.Clear;
    for I := 1 to 2 do begin
      // For solute 2 and 3: Determine if secretion/reabsorbtion based on extraction ratio value
      if aValues[I, 5] <> 0 then begin
        S := 'For solute ' + IntToStr(I + 1) + ', ';
        if aValues[I, 5] < rFF then
          S += 'E < FF: solute is filtered and reabsorbed'
        else if aValues[I, 5] > rFF then
          S += 'E > FF: solute is filtered and secreted'
        else
          S += 'E = FF: solute is only filtered';
        edExtraction.Lines.AddText(S);
      end;
    end;
    // calculate amounts per min filtered, reabsorbed, secreted and appearing in urine
    edUrine.Lines.Clear;
    for I := 0 to 2 do begin
      PA := aValues[I, 0]; PV := aValues[I, 1];
      U  := aValues[I, 2]; V := aValues[I, 3];
      if PA > 0 then begin
        aValues[I, 6] := PA * rGFR;                                            // amount of a solute filtered / min
        aValues[I, 7] := U * V;                                                // amount of solute appearing in urine / min
        aValues[I, 8] := (PA * rGFR) - (U  *  V);                              // amount of solute reabsorbed / min
        if aValues[I, 8] < 0 then
          aValues[I, 8] := 0;
        aValues[I, 9] := (U * V) - (PA * rGFR);                                // amount of solute secreted / min
        if aValues[I, 9] < 0 then
          aValues[I, 9] := 0;
        DisplayValues(aValues, edValues, 6, 9);                                // display amount per min values
        // Textual display of what the amount of solute in urine is made of
        S := 'For solute ' + IntToStr(I + 1) + ', solute in urine is ';
        if U * V < rGFR * PA then
          S += 'solute filtered - solute reabsorbed'
        else if U * V > rGFR * PA then
          S += 'solute filtered + solute secreted'
        else
          S += 'solute filtered';
        edUrine.Lines.AddText(S);
      end;
    end;
  end;
end;

end.

