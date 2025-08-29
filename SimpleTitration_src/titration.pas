{*********************************************}
{* Main unit for SimpleTitration application *}
{*********************************************}

unit titration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, LCLIntf, Math;

type
  TTitrationValue = record
    Volume, PH: Real;
  end;
  TTitrationValues = array of TTitrationValue;
  TVolShapes = array[0..300] of record
    Left, Width: Integer;
  end;
  {*************}
  { TfTitration }
  {*************}
  TfTitration = class(TForm)
    mMenu: TMainMenu;
    mCalculation, mCalculationPH, mCalculationCurve, mCalculationSimul, mCalculationExit: TMenuItem;
    mOptions, mOptionsTitration, mOptionsTitrationAB, mOptionsTitrationBA: TMenuItem;
    mOptionsSimulation, mOptionsSimulationUConc, mOptionsSimulationUVol, mOptionsSimulationRandom: TMenuItem;
    mOptionsFlask, mOptionsFlask100, mOptionsFlask750, mOptionsFlask500, mOptionsFlask250, mOptionsMMol: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    imTitration, imCurve: TImage;
    shDrop: TShape;
    Label1, laAcid, laBase: TLabel;
    laConc1, laConc2, laVol1, laVol2: TLabel;
    laUConc1, laUConc2, laUVol1, laUVol2: TLabel;
    cobAcids, cobBases: TComboBox;
    edConc1, edConc2, edVol1, edVol2: TEdit;
    laTitration, laTitVolAcid, laTitVolBase, laTitVolTotal: TLabel;
    laTitQtyAcid, laTitQtyBase, laTitQtyH, laTitQtyOH: TLabel;
    laTitConcH, laTitConcOH, laPH, laPOH, laEndPoint: TLabel;
    laTitUVolAcid, laTitUVolBase, laTitUVolTotal: TLabel;
    laTitUQtyAcid, laTitUQtyBase, laTitUQtyH, laTitUQtyOH: TLabel;
    laTitUConcH, laTitUConcOH: TLabel;
    edTitVolAcid, edTitVolBase, edTitVolTotal: TEdit;
    edTitQtyAcid, edTitQtyBase, edTitQtyH, edTitQtyOH: TEdit;
    edTitConcH, edTitConcOH, edPH, edPOH: TEdit;
    edDetails: TMemo;
    btAction: TButton;
    tiTitration: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mCalculationPHClick(Sender: TObject);
    procedure mCalculationCurveClick(Sender: TObject);
    procedure mCalculationSimulClick(Sender: TObject);
    procedure mCalculationExitClick(Sender: TObject);
    procedure mOptionsTitrationABClick(Sender: TObject);
    procedure mOptionsTitrationBAClick(Sender: TObject);
    procedure mOptionsSimulationUConcClick(Sender: TObject);
    procedure mOptionsSimulationUVolClick(Sender: TObject);
    procedure mOptionsSimulationRandomClick(Sender: TObject);
    procedure mOptionsFlask100Click(Sender: TObject);
    procedure mOptionsFlask250Click(Sender: TObject);
    procedure mOptionsFlask500Click(Sender: TObject);
    procedure mOptionsFlask750Click(Sender: TObject);
    procedure mOptionsMMolClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure tiTitrationTimer(Sender: TObject);
    procedure cobAcidsChange(Sender: TObject);
    procedure cobBasesChange(Sender: TObject);
    procedure edConc1EditingDone(Sender: TObject);
    procedure edConc2EditingDone(Sender: TObject);
    procedure edVol1EditingDone(Sender: TObject);
    procedure edVol2EditingDone(Sender: TObject);
  private
    iVolShapeTotal, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, iStep: Integer;
    clVolShape: TColor;
    rVolFlask, rVolFlaskTotal, rAcidVol, rAcidConc, rHQty, rOHQty, rBaseVol, rBaseConc, rPH, rVolTotal, rOldVol, rOldPH: Real;
    sAction, sTitration, sAcid, sBase: string;
    bFlaskFull: Boolean;
    aAcids, aBases: array of string;
    aTitrationValues: TTitrationValues;
    bmCurve: TBitmap;
    aVolShapes: TVolShapes;
    shVolShapes: array[0..300] of TShape;
  end;

const
  SUP_PLUS = #$E2#$81#$BA; SUP_MINUS = #$E2#$81#$BB;
  SUP_Digits: array[0..9] of string = (
    #$E2#$81#$B0, #$C2#$B9, #$C2#$B2, #$C2#$B3, #$E2#$81#$B4, #$E2#$81#$B5, #$E2#$81#$B6, #$E2#$81#$B7, #$E2#$81#$B8, #$E2#$81#$B9
  );
  SUB_Digits: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84, #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  fTitration: TfTitration;

implementation

{$R *.lfm}

{ Format interger (right-align for given length) }

function IFormat(N, P: Integer): string;

var
  SN: string;

begin
  SN := IntToStr(N);
  while Length(SN) < P do
    SN := ' ' + SN;
  Result := SN;
end;

{ Format real (remove unsignificant zeros for given number of decimal digits) }

function RFormat(R: Real; D: Integer): string;

var
  I: Integer;
  SR0, SR: string;

begin
  if D = -1 then                                                               // default number of decimal digits
    D := 5;
  SR0 := FloatToStrF(R, ffFixed, 0, D); SR := SR0;                             // round to given number of decimal digits
  // Remove unsignificant digits
  I := Length(SR0);
  while SR0[I] = '0' do begin
    Delete(SR, Length(SR), 1);
    Dec(I);
  end;
  if (RightStr(SR, 1) = '.') or (RightStr(SR, 1) = ',') then                   // remove potential decimal separator at end of number string
    Delete(SR, Length(SR), 1);
  Result := SR;
end;

{ Transform numbers to subscripts }

function ApplySubscripts(S0: string): string;

var
  I: Integer;
  S: string;

begin
  S := '';
  for I := 1 to Length(S0) do begin
    if S0[I] in ['0'..'9'] then
      S += SUB_DIGITS[StrToInt(S0[I])]
    else
      S += S0[I];
  end;
  Result := S;
end;

{ Read flask content data }

procedure ReadVolShapeData(out VolShape: TVolShapes);

// Filling the flask is implemented by making visible a given number of shapes of height = 1 pixel
// (total number of shapes = 300). These shapes are defines in the data file read here: for each shape,
// being displayed at a given flask position, the file contains its horizontal position ("Left" property)
// and size ("Width" property). The vertical position ("Top" property) is not needed; with the position
// of the flask known, it can be calculated during the creation of the shape objects.

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'volsh.dat'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      VolShape[N - 1].Left  := StrToInt(Trim(LeftStr(Line, 3)));
      VolShape[N - 1].Width := StrToInt(Trim(Copy(Line, 5, 3)));
    end;
  end;
  Close(InFile);
end;

{ Empty the flask (making all shapes invisible) }

procedure EmptyFlask(out FlaskVol: Real);

var
  I: Integer;

begin
  for I := 0 to 300 do
    fTitration.shVolShapes[I].Visible := False;
  FlaskVol := 0;
end;

{ Fill the flask with given volume (by making the corresponding number of shapes visible) }

procedure FillFlask(FlaskTotalVol, FlaskVol: Real; var FlaskFull: Boolean;
  var VolShapes: TVolShapes; VolShapeTotal: Integer; VolShapeColor: TColor);

var
  VolShape, VShape, I: Integer;

begin
  // If fill-volume > flask content, display message
  if FlaskVol > FlaskTotalVol then begin
    if not FlaskFull then begin
      MessageDlg('SimpleTitration', 'Flask to small for actual solution volume', mtWarning, [mbOK], 0);
      FlaskFull := True;                                                       // this variable is used to avoid continuous (endless!) display of the message!
    end;
    EmptyFlask(FlaskVol);
  end
  // Fill the flask with given volume of actual solution
  else begin
    if FlaskVol > 0 then begin
      for I := 0 to 300 do
        fTitration.shVolShapes[I].Visible := False;
      VolShape := Round((FlaskVol / FlaskTotalVol) * VolShapeTotal);           // number of pixles, corresponding to actual flask content
      // Fill the flask with given volume by making corr. number of shapes visible
      // The loop calculates the sum of the number of pixles of the shapes; if
      // this number exceeds the number of pixles corr. to the actual flask
      // content, filling is complete
      VShape := 0; I := 0;
      repeat
        VShape += VolShapes[I].Width;
        if VShape <= VolShape then begin
          fTitration.shVolShapes[I].Visible := True;
          fTitration.shVolShapes[I].Pen.Color := VolShapeColor;                // solution color (unknown pH, acidic, basic, neutral)
          fTitration.shVolShapes[I].Brush.Color := VolShapeColor;
          Inc(I);
        end;
      until VShape >= VolShape;
    end;
  end;
end;

{ Clear the titration graph drawing surface }

procedure ClearCurve(Surface: TImage);

begin
  // Drawing surface as white rectancle (with black border)
  Surface.Picture.Bitmap.Canvas.Clear;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack; Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
end;

{ Draw the titration graph axes }

procedure DrawAxis(Surface: TImage; XL, XR, YT, YB: Integer; Title, LabelX, LabelY: string);

var
  YAX, YLX: Integer;

begin
  ClearCurve(Surface);
  fTitration.laTitration.Caption := Title;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  Surface.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 40, YAX);              // draw X-axis
  Surface.Picture.Bitmap.Canvas.Line(XL, YT - 20, XL, YB + 20);                // draw Y-axis
  Surface.Picture.Bitmap.Canvas.TextOut(XR - 35, YLX, LabelX);                 // display X-axis legend
  Surface.Picture.Bitmap.Canvas.TextOut(6, 6, LabelY);                         // display Y-axis legend
end;

{ Clear all: Clear edit fields and graph; empty flask }

procedure ClearAll(Action, Titration: string; var FlaskVol: Real; out Colour: TColor; out FlaskFull: Boolean; ClearParams: Boolean);

begin
  if (Action <> 'Start') or ((fTitration.btAction.Caption <> 'Pause') and (fTitration.btAction.Caption <> 'Resume')) then begin
    // Do not do the clearing, while the simulation is running!
    if ClearParams then begin
      // User input parameters are cleared only if the ClearParams variable is set to True
      fTitration.edVol1.Text := ''; fTitration.edVol2.Text := '';
      fTitration.edConc1.Text := ''; fTitration.edConc2.Text := '';
      EmptyFlask(FlaskVol); FlaskFull := False;
    end;
    if (Action = 'Start') and (not fTitration.mOptionsSimulationUConc.Checked) then begin
      // Simulation: Solution with unknown concentration
      fTitration.edConc1.Text := '?';
      fTitration.edConc1.Alignment := taCenter;
    end;
    // Clear all calculated values edit fields
    fTitration.edTitVolAcid.Text := ''; fTitration.edTitVolBase.Text := ''; fTitration.edTitVolTotal.Text := '';
    fTitration.edTitQtyAcid.Text := ''; fTitration.edTitQtyBase.Text := '';
    fTitration.edTitQtyH.Text := ''; fTitration.edTitQtyOH.Text := '';
    fTitration.edTitConcH.Text := ''; fTitration.edTitConcOH.Text := '';
    fTitration.edPH.Text := ''; fTitration.edPOH.Text := '';
    fTitration.edDetails.Lines.Clear;
    fTitration.laEndPoint.Visible := False;
    // Set the color of the flask solution (unknown, acidic, basic)
    if Action = 'pH' then
      Colour := clGreen
    else begin
      if Titration = 'ab' then
        Colour := clRed
      else
        Colour := clBlue;
    end;
    // Clear the titration graph
    ClearCurve(fTitration.imCurve);
  end;
end;

{ Prepare for a new calculation (pH, curve, simulation) }

procedure NewCalculation(Action, Titration: string; var FlaskVol: Real; out Colour: TColor; out FlaskFull: Boolean);

var
  Filename: string;

begin
  // Display title (depending on calculation actually done)
  fTitration.tiTitration.Enabled := False;
  if Action = 'pH' then
    fTitration.stTitle.Caption := 'pH calculation: Strong acid-base solution.'
  else if Action = 'Curve' then
    fTitration.stTitle.Caption := 'Titration curve: '
  else
    fTitration.stTitle.Caption := 'Titration simulation: ';
  // Clear all
  ClearAll(Action, Titration, FlaskVol, Colour, FlaskFull, True);
  // Hide pOH edit field (only displayed for a basic solution)
  fTitration.laPOH.Visible := False; fTitration.edPOH.Visible := False;
  // Hide the titrant drop
  fTitration.shDrop.Visible := False;
  // Adapt the position of the acid and base controls (the solution to be titrated always at
  // left, the titrant always at the right)
  if Titration = 'ab' then begin
    fTitration.laAcid.Left := 392; fTitration.laBase.Left := 731;
    fTitration.cobAcids.Left := 536; fTitration.cobBases.Left := 884;
    if Action <> 'pH' then
      fTitration.stTitle.Caption := fTitration.stTitle.Caption + 'Strong acid by strong base.';
  end
  else begin
    fTitration.laAcid.Left := 731; fTitration.laBase.Left := 392;
    fTitration.cobAcids.Left := 884; fTitration.cobBases.Left := 536;
    if Action <> 'pH' then
      fTitration.stTitle.Caption := fTitration.stTitle.Caption + 'Strong base by strong acid.';
  end;
  // Normally, the solution's concentration has to be entered by the user
  fTitration.edConc1.ReadOnly := False; fTitration.edConc1.TabStop := True;
  fTitration.edConc1.Alignment := taRightJustify; fTitration.edConc1.Font.Style := [];
  // Load titration image, depending on calculation done; adapt access to edit fields, filled-in by user
  if Action = 'pH' then begin
    fTitration.edVol2.Enabled := True;
    Filename := 'titration.jpg';
  end
  else begin
    if Action = 'Curve' then begin
      fTitration.edVol2.Enabled := False;
    end
    else begin
      // Adapt access to edit fields, filled-in by user, depending on simulation mode
      fTitration.edConc1.Text := '?';
      fTitration.edConc1.Alignment := taCenter; fTitration.edConc1.Font.Style := [fsBold];
      fTitration.edConc1.ReadOnly := True; fTitration.edConc1.TabStop := False;
      if fTitration.mOptionsSimulationUVol.Checked then begin
        fTitration.edVol2.Enabled := True;
      end
      else begin
        fTitration.edVol2.Enabled := False;
        if fTitration.mOptionsSimulationUConc.Checked then begin
          fTitration.edConc1.ReadOnly := False; fTitration.edConc1.TabStop := True;
          fTitration.edConc1.Alignment := taRightJustify; fTitration.edConc1.Font.Style := [];
          fTitration.edConc1.Text := '';
        end;
      end;
    end;
    // Image with basic (blue) resp. acidic (red) titrant
    if Titration = 'ab' then
      Filename := 'titration_ab.jpg'
    else
      Filename := 'titration_ba.jpg';
  end;
  // Load image for actual calculation type
  fTitration.laTitration.Caption := 'Titration';
  fTitration.imTitration.Picture.LoadFromFile(Filename);
  // Titrant volume to be entered during simulation = equivalence point volume
  if (Action = 'Start') and fTitration.mOptionsSimulationUVol.Checked then
    fTitration.laVol2.Caption := 'Equ. pt. volome'
  else
    fTitration.laVol2.Caption := 'Volume';
  // Set button caption (depending on calculation done)
  fTitration.btAction.Caption := Action;
end;

{ Read user input data from form }

procedure GetData(CType, TType: string; Acids, Bases: array of string; out Acid, Base: string;
  out AcidVol, AcidConc, BaseVol, BaseConc: Real; out Mess: string);

var
  Temp: Real;

begin
  Mess := '';
  Acid := Acids[fTitration.cobAcids.ItemIndex]; Base := Bases[fTitration.cobBases.ItemIndex];
  AcidVol := 0; AcidConc := 0; BaseVol := 0; BaseConc := 0;
  if fTitration.edVol1.Text <> '' then
    AcidVol := StrToFloat(fTitration.edVol1.Text);
  if (fTitration.edConc1.Text <> '') and (fTitration.edConc1.Text <> '?') then
    AcidConc := StrToFloat(fTitration.edConc1.Text);
  if fTitration.edVol2.Enabled and (fTitration.edVol2.Text <> '') then
    BaseVol := StrToFloat(fTitration.edVol2.Text);
  if fTitration.edConc2.Text <> '' then
    BaseConc := StrToFloat(fTitration.edConc2.Text);
  if TType = 'ba' then begin
    // Solution to be titrated always at the left: Edit field content depends on titration type
    Temp := AcidVol; AcidVol := BaseVol; BaseVol := Temp;
    Temp := AcidConc; AcidConc := BaseConc; BaseConc := Temp;
  end;
  // Validity check of user input (checking if all data is there, if positive values, ...)
  if AcidVol < 0 then begin
    Mess := 'Acid volume cannot be negative';
    if TType = 'ab' then
      fTitration.edVol1.SetFocus
    else
      fTitration.edVol2.SetFocus;
  end
  else if BaseVol < 0 then begin
    Mess := 'Base volume cannot be negative';
    if TType = 'ba' then
      fTitration.edVol1.SetFocus
    else
      fTitration.edVol2.SetFocus;
  end
  else if (TType = 'ab') and (AcidVol = 0) then begin
    Mess := 'No acid in the flask';
    fTitration.edVol1.SetFocus
  end
  else if (TType = 'ba') and (BaseVol = 0) then begin
    Mess := 'No base in the flask';
    fTitration.edVol1.SetFocus
  end
  else if (AcidConc < 0) then begin
    Mess := 'Acid concentration cannot be negative';
    if TType = 'ab' then
      fTitration.edConc1.SetFocus
    else
      fTitration.edConc2.SetFocus;
  end
  else if BaseConc < 0 then begin
    Mess := 'Base concentration cannot be negative';
    if TType = 'ba' then
      fTitration.edConc1.SetFocus
    else
      fTitration.edConc2.SetFocus;
  end
  else begin
    if (CType = 'pH') or (CType = 'Curve') then begin
      if AcidConc = 0 then begin
        Mess := 'Missing acid concentration';
        if TType = 'ab' then
          fTitration.edConc1.SetFocus
        else
          fTitration.edConc2.SetFocus;
      end
      else if BaseConc = 0 then begin
        Mess := 'Missing base concentration';
        if TType = 'ba' then
          fTitration.edConc1.SetFocus
        else
          fTitration.edConc2.SetFocus;
      end;
    end
    else begin
      if (TType = 'ab') and (BaseConc = 0) then begin
        Mess := 'Missing base concentration';
        fTitration.edConc2.SetFocus;
      end
      else if (TType = 'ba') and (AcidConc = 0) then begin
        Mess := 'Missing acid concentration';
        fTitration.edConc2.SetFocus;
      end
      else if fTitration.mOptionsSimulationUVol.Checked then begin
        if (TType = 'ab') and (BaseVol = 0) then
          Mess := 'Missing base equivalence point volume'
        else if (TType = 'ba') and (AcidVol = 0) then
          Mess := 'Missing acid equivalence point volume';
        fTitration.edVol2.SetFocus
      end
      else if fTitration.mOptionsSimulationUConc.Checked then begin
        if (TType = 'ab') and (AcidConc = 0) then
          Mess := 'Missing acid concentration'
        else if (TType = 'ba') and (BaseConc = 0) then
          Mess := 'Missing base concentration';
        fTitration.edConc1.SetFocus
      end;
    end;
  end;
  if Mess <> '' then
    // Error message if some user value is invalid
    MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
end;

{ Adapt acid/base amount and concentration display, depending if unit = Mol or mMol}

procedure AdaptQuantities(QtyUnit1, QtyUnit2: string);

begin
  if (QtyUnit1 = 'Mol') and (QtyUnit2 = 'mMol') then begin
    // Transform Mol to mMol
    if fTitration.edTitQtyAcid.Text <> '' then
      fTitration.edTitQtyAcid.Text := RFormat(StrToFloat(fTitration.edTitQtyAcid.Text) * 1000, -1);
    if fTitration.edTitQtyBase.Text <> '' then
      fTitration.edTitQtyBase.Text := RFormat(StrToFloat(fTitration.edTitQtyBase.Text) * 1000, -1);
    if fTitration.edTitQtyH.Text <> '' then
      fTitration.edTitQtyH.Text := RFormat(StrToFloat(fTitration.edTitQtyH.Text) * 1000, -1);
    if fTitration.edTitQtyOH.Text <> '' then
      fTitration.edTitQtyOH.Text := RFormat(StrToFloat(fTitration.edTitQtyOH.Text) * 1000, -1);
    if fTitration.edTitConcH.Text <> '' then
      fTitration.edTitConcH.Text := RFormat(StrToFloat(fTitration.edTitConcH.Text) * 1000, -1);
    if fTitration.edTitConcOH.Text <> '' then
      fTitration.edTitConcOH.Text := RFormat(StrToFloat(fTitration.edTitConcOH.Text) * 1000, -1);
  end
  else if (QtyUnit1 = 'mMol') and (QtyUnit2 = 'Mol') then begin
    // Transform mMol to Mol
    if fTitration.edTitQtyAcid.Text <> '' then
      fTitration.edTitQtyAcid.Text := RFormat(StrToFloat(fTitration.edTitQtyAcid.Text) / 1000, -1);
    if fTitration.edTitQtyBase.Text <> '' then
      fTitration.edTitQtyBase.Text := RFormat(StrToFloat(fTitration.edTitQtyBase.Text) / 1000, -1);
    if fTitration.edTitQtyH.Text <> '' then
      fTitration.edTitQtyH.Text := RFormat(StrToFloat(fTitration.edTitQtyH.Text) / 1000, -1);
    if fTitration.edTitQtyOH.Text <> '' then
      fTitration.edTitQtyOH.Text := RFormat(StrToFloat(fTitration.edTitQtyOH.Text) / 1000, -1);
    if fTitration.edTitConcH.Text <> '' then
      fTitration.edTitConcH.Text := RFormat(StrToFloat(fTitration.edTitConcH.Text) / 1000, -1);
    if fTitration.edTitConcOH.Text <> '' then
      fTitration.edTitConcOH.Text := RFormat(StrToFloat(fTitration.edTitConcOH.Text) / 1000, -1);
  end;
  // Change unit labels accordingly
  fTitration.laTitUQtyAcid.Caption := QtyUnit2; fTitration.laTitUQtyBase.Caption := QtyUnit2;
  fTitration.laTitUQtyH.Caption := QtyUnit2; fTitration.laTitUQtyOH.Caption := QtyUnit2;
  fTitration.laTitUConcH.Caption := QtyUnit2 + '/L'; fTitration.laTitUConcOH.Caption := QtyUnit2 + '/L';
end;

{ Main pH calculation routine }

procedure DoCalculation(CType, Acid, Base: string; AcidVol, BaseVol, AcidConc, BaseConc: Real; out HQty, OHQty, PH: Real);

var
  TotalVol, AcidQty, HConc, BaseQty, OHConc, POH: Real;
  S: string;

begin
  // Solution total volume
  TotalVol := AcidVol + BaseVol;
  // Acid and base amount (transform from ml to L!)
  AcidQty := (AcidVol / 1000) * AcidConc; BaseQty := (BaseVol / 1000) * BaseConc;
  // H+ and OH- amount (different from acid and base qty for polyprotic acids or bases with more than one OH)
  HQty := AcidQty;
  if LeftStr(Acid, 2) = 'H2' then
    HQty *= 2;
  OHQty := BaseQty;
  if RightStr(Base, 5) = '(OH)2' then
    OHQty *= 2;
  // Calculation of H+ concentration (for those H+ that did not react with OH- )
  if Abs(HQty - OHQty) < 1E-6 then begin
    // H+ qty = OH- qty: pH = 7
    HConc := 0; OHConc := 0; PH := 7;
  end
  else if HQty > OHQty then begin
    // H+ qty > OH- qty: solution is acidic; pH = -log[H+]
    HConc := 1000 * (HQty - OHQty) / TotalVol; PH := -Log10(HConc); OHConc := 0;
  end
  else begin
    // H+ qty < OH- qty: solution is basic; pOH = -log[OH-]
    OHConc := 1000 * (OHQty - HQty) / TotalVol; HConc := 0;
    POH := -Log10(OHConc); PH := 14 - POH;
  end;
  // Fill in calculated values edit fields
  if CType = 'pH' then begin
    // The procedure is called for all 3 types of calculations. In the case of the titration curve CType is 'curve'
    // during curve drawing (no fill-in) and set to 'pH' for the calculation (and display = values fill-in) of the
    // equivalent point situation. In the case of a simulation, the procedure is called with CType = 'pH', so the
    // calculated values fill-in is done for each simulation step
    fTitration.edTitVolAcid.Text := RFormat(AcidVol, 2); fTitration.edTitVolBase.Text := RFormat(BaseVol, 2);
    fTitration.edTitVolTotal.Text := RFormat(TotalVol, 2);
    fTitration.edTitQtyAcid.Text := RFormat(AcidQty, -1); fTitration.edTitQtyBase.Text := RFormat(BaseQty, -1);
    fTitration.edTitQtyH.Text := RFormat(HQty, -1); fTitration.edTitQtyOH.Text := RFormat(OHQty, -1);
    fTitration.edDetails.Lines.Clear;
    fTitration.edTitConcH.Text := RFormat(HConc, -1); fTitration.edTitConcOH.Text := RFormat(OHConc, -1);
    fTitration.edPH.Text := RFormat(PH, 2);
    if fTitration.laTitUQtyAcid.Caption = 'mMol' then
      AdaptQuantities('Mol', 'mMol');
    // Display pHO only for basic solutions
    if PH > 7 then begin
      fTitration.edPOH.Text := RFormat(POH, 2); fTitration.laPOH.Visible := True; fTitration.edPOH.Visible := True;
    end
    else begin
      fTitration.laPOH.Visible := False; fTitration.edPOH.Visible := False;
    end;
    // Display detailed information concerning the flask solution and its pH
    if HQty <= OHQty then
      S := 'All H+ ions combine with OH- ions to give H2O.' + LineEnding
    else
      S := 'All OH- ions combine with H+ ions to give H2O.' + LineEnding;
    if Abs(HQty - OHQty) < 1E-6 then
      S += 'There are neither H+, nor OH- ions left. The solution is neutral.'
    else if HQty > OHQty then
      S += 'There are ' + RFormat(HQty - OHQty, -1) + ' Mol of H+ ions left. The solution is acidic.'
    else
      S += 'There are ' + RFormat(OHQty - HQty, -1) + ' Mol of OH- ions left. The solution is basic.';
    S := StringReplace(S, '+', SUP_Plus, [rfReplaceAll]);
    S := StringReplace(S, '-', SUP_Minus, [rfReplaceAll]);
    S := StringReplace(S, 'H2O', 'H' + SUB_Digits[2] + 'O', [rfReplaceAll]);
    fTitration.edDetails.Lines.Clear; fTitration.edDetails.Lines.AddText(S);
  end;
end;

{ Draw the titration curve }

procedure DrawCurve(TType: string; Surface: TImage; var TitrationValues: TTitrationValues; XL, XR, YT, YB: Integer);

const
  TVolumes: array[0..6] of Integer = (
    1000, 500, 200, 100, 50, 20, 10
  );

var
  TVol, X, Y, N, I: Integer;
  PHMax: Real;
  Title: string;

begin
  N := Length(TitrationValues);
  // Graph title and drawing of axes
  Title := 'Titration of ' + fTitration.edVol1.Text + ' ml of ';
  if TType = 'ab' then begin
    Title += fTitration.cobAcids.Text + ' ' + fTitration.edConc1.Text + 'M';
    Title += ' by ' + fTitration.cobBases.Text + ' ' + fTitration.edConc2.Text + 'M';
    DrawAxis(Surface, XL, XR, YT, YB, Title, fTitration.cobBases.Text + ' [ml]', 'pH');
  end
  else begin
    Title += fTitration.cobBases.Text + ' ' + fTitration.edConc1.Text + 'M';
    Title += ' by ' + fTitration.cobAcids.Text + ' ' + fTitration.edConc2.Text + 'M';
    DrawAxis(Surface, XL, XR, YT, YB, Title, fTitration.cobAcids.Text + ' [ml]', 'pH')
  end;
  PHMax := 14;
  Surface.Picture.Bitmap.Canvas.Pen.Width := 2;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clGreen;
  // Draw the curve
  for I := 0 to N - 1 do begin
    // Calculate x-value (volume) on graph
    X := Round((TitrationValues[I].Volume / TitrationValues[N - 1].Volume) * (XR - XL)) + XL;
    // Calculate y-value (pH) on graph
    Y := YB - Round((TitrationValues[I].PH / PHMax) * (YB - YT));
    // Draw curve line
    if I = 0 then                                                              // first point of the curve:
      Surface.Picture.Bitmap.Canvas.MoveTo(X, Y)                               // position the pen
    else                                                                       // other points of the curve:
      Surface.Picture.Bitmap.Canvas.LineTo(X, Y);                              // draw line from previous point to here
  end;
  // Draw horizontal and vertical lines at the equivalence point
  X := XL; Y := YB - Round((7 / PHMax) * (YB - YT));
  while (X <= XR) and (Surface.Picture.Bitmap.Canvas.Pixels[X, Y] <> clGreen) do begin
    // Drawing of the horizontal line is done, starting from the y-axis, until the curve is reached (colored pixel at this position)
    if X mod 3 = 1 then
      // Use the mod function to draw a dotted (instead of full) line
      Surface.Picture.Bitmap.Canvas.Pixels[X, Y] := clBlack;
    Inc(X);
  end;
  // Drawing of the vertical line is done, starting from the point determined before to the x-axis
  for Y := YB - Round((7 / PHMax) * (YB - YT)) to YB do begin
    if Y mod 3 = 1 then
      // Use the mod function to draw a dotted (instead of full) line
      Surface.Picture.Bitmap.Canvas.Pixels[X, Y] := clBlack;
  end;
  // Draw ticks and write values of x-axis
  Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  // Choose one of the predefined x-axis ticks sets
  TVol := -1;
  for I := 0 to 6 do begin
    if TitrationValues[N - 1].Volume <= TVolumes[I] then
      TVol := TVolumes[I] div 10;
  end;
  if TVol <> -1 then begin
    // If none of the predefined ticks sets is appropriate (to small or to large volumes), don't draw any x-axis ticks
    for I := 1 to 10 do begin
      // Tick position on the x-axis
      X := Round((I * TVol / TitrationValues[N - 1].Volume) * (XR - XL)) + XL;
      if X <= XR - 60 then begin
        // Avoid last tick(s) overriding the x-axis label
        Surface.Picture.Bitmap.Canvas.Line(X, YB - 5, X, YB + 5);
        // Adapt position of tick labels (depending on their length) in order to center then below the tick
        if I * TVol < 10 then
          X += 5
        else if I * TVol >= 100 then
          X -= 4;
        Surface.Picture.Bitmap.Canvas.TextOut(X - 8, YB + 7, IntToStr((I * TVol)));
      end;
    end;
  end;
  // Draw ticks and write values of y-axis
  Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  for I := 1 to 14 do begin
    // pH on the graph considered to be between 0 and 14
    Y := YB - Round((I / PHMax) * (YB - YT));
    Surface.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
    Surface.Picture.Bitmap.Canvas.TextOut(12, Y - 8, IFormat(I, 2));
  end;
end;

{*************}
{ TfTitration }
{*************}

{ Application start: Initialisation }

procedure TfTitration.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Read flask content data from file
  ReadVolShapeData(aVolShapes);
  // Create bitmap for curve drawing
  bmCurve := TBitmap.Create;
  bmCurve.Width  := imCurve.Width;
  bmCurve.Height := imCurve.Height;
  imCurve.Picture.Graphic := bmCurve;
  iGraphLeft := 40; iGraphRight  := imCurve.Width - 60;
  iGraphTop  := 40; iGraphBottom := imCurve.Height - 40;
  // Create the 1px-height shapes, used to fill the flask
  // "Left" and "Width" properties of these shapes are defined in the file read before
  iVolShapeTotal := 0;
  for I := 0 to 300 do begin
    shVolShapes[I] := TShape.Create(shVolShapes[I]);
    shVolShapes[I].Parent := Self;
    shVolShapes[I].Shape := stRectangle;
    shVolShapes[I].Left := aVolShapes[I].Left;
    shVolShapes[I].Top := 718 - I;                                             // shape "Top" property can be calculated
    shVolShapes[I].Width := aVolShapes[I].Width; iVolShapeTotal += shVolShapes[I].Width;
    shVolShapes[I].Height := 1;
    shVolShapes[I].Brush.Color := clVolShape;
    shVolShapes[I].Pen.Color := clVolShape;
    shVolShapes[I].Visible := False;
  end;
  // Apply ions superscripts
  laTitQtyH.Caption := StringReplace(laTitQtyH.Caption, '+', SUP_Plus, []);
  laTitQtyOH.Caption := StringReplace(laTitQtyOH.Caption, '-', SUP_Minus, []);
  laTitConcH.Caption := StringReplace(laTitConcH.Caption, '+', SUP_Plus, []);
  laTitConcOH.Caption := StringReplace(laTitConcOH.Caption, '-', SUP_Minus, []);
  // Create array with acids (and apply subscripts to acids combobox items)
  SetLength(aAcids, cobAcids.Items.Count);
  for I := 0 to cobAcids.Items.Count - 1 do begin
    aAcids[I] := cobAcids.Items[I];
    cobAcids.Items[I] := ApplySubscripts(cobAcids.Items[I]);
  end;
  // Create array with acids (and apply subscripts to acids combobox items)
  SetLength(aBases, cobBases.Items.Count);
  for I := 0 to cobBases.Items.Count - 1 do begin
    aBases[I] := cobBases.Items[I];
    cobBases.Items[I] := ApplySubscripts(cobBases.Items[I]);
  end;
  // Set start-up parameters
  sTitration := 'ab'; rVolFlaskTotal := 250; clVolShape := clGreen;
  Randomize;
  // Start with a solution pH calculation
  mCalculationPH.Click;
end;

{ Menu item "Calculation > pH calculation": Prepare for a new pH calculation }

procedure TfTitration.mCalculationPHClick(Sender: TObject);

begin
  sAction := 'pH'; rVolFlask := 0;
  NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

{ Menu item "Calculation > Titration curve": Prepare for a new drawing of a titration curve }

procedure TfTitration.mCalculationCurveClick(Sender: TObject);

begin
  sAction := 'Curve'; rVolFlask := 0;
  NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

{ Menu item "Calculation > Titration simulation": Prepare for a new titration simulation }

procedure TfTitration.mCalculationSimulClick(Sender: TObject);

begin
  sAction := 'Start'; rVolFlask := 0;
  NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

{ Menu item "Calculation > Exit": Exit application }

procedure TfTitration.mCalculationExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Titration > ...": Toggle between acid-base and base-acid titration }

procedure TfTitration.mOptionsTitrationABClick(Sender: TObject);

begin
  mOptionsTitrationAB.Checked := True; mOptionsTitrationBA.Checked := False;
  sTitration := 'ab'; rVolFlask := 0;
  NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

procedure TfTitration.mOptionsTitrationBAClick(Sender: TObject);

begin
  mOptionsTitrationAB.Checked := False; mOptionsTitrationBA.Checked := True;
  sTitration := 'ba'; rVolFlask := 0;
  NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

{ Menu items "Options > Simulation > ...": Choose titration simulation mode }

procedure TfTitration.mOptionsSimulationUConcClick(Sender: TObject);

begin
  mOptionsSimulationUConc.Checked := True; mOptionsSimulationUVol.Checked := False; mOptionsSimulationRandom.Checked := False;
  if sAction = 'Start' then
    NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

procedure TfTitration.mOptionsSimulationUVolClick(Sender: TObject);

begin
  mOptionsSimulationUConc.Checked := False; mOptionsSimulationUVol.Checked := True; mOptionsSimulationRandom.Checked := False;
  if sAction = 'Start' then
    NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

procedure TfTitration.mOptionsSimulationRandomClick(Sender: TObject);

begin
  mOptionsSimulationUConc.Checked := False; mOptionsSimulationUVol.Checked := False; mOptionsSimulationRandom.Checked := True;
  if sAction = 'Start' then
    NewCalculation(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull);
end;

{ Menu items "Options > Flask content > ...": Choose content (maximum volume) of flask }

procedure TfTitration.mOptionsFlask100Click(Sender: TObject);

begin
  mOptionsFlask100.Checked := True; mOptionsFlask250.Checked := False; mOptionsFlask500.Checked := False; mOptionsFlask750.Checked := False;
  rVolFlaskTotal := 100;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

procedure TfTitration.mOptionsFlask250Click(Sender: TObject);

begin
  mOptionsFlask100.Checked := False; mOptionsFlask250.Checked := True; mOptionsFlask500.Checked := False; mOptionsFlask750.Checked := False;
  rVolFlaskTotal := 250;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

procedure TfTitration.mOptionsFlask500Click(Sender: TObject);

begin
  mOptionsFlask100.Checked := False; mOptionsFlask250.Checked := False; mOptionsFlask500.Checked := True; mOptionsFlask750.Checked := False;
  rVolFlaskTotal := 500;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

procedure TfTitration.mOptionsFlask750Click(Sender: TObject);

begin
  mOptionsFlask100.Checked := False; mOptionsFlask250.Checked := False; mOptionsFlask500.Checked := False; mOptionsFlask750.Checked := True;
  rVolFlaskTotal := 750;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

{ Menu item "Options > Amounts in mMol": Toggle between amounts in Mol and mMol }

procedure TfTitration.mOptionsMMolClick(Sender: TObject);

begin
  if mOptionsMMol.Checked then begin
    mOptionsMMol.Checked := False;
    AdaptQuantities('mMol', 'Mol');
  end
  else begin
    mOptionsMMol.Checked := True;
    AdaptQuantities('Mol', 'mMol');
  end;
end;

{ Menu item "Help > Help": Open application help text in default webbrowser }

procedure TfTitration.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Help > About": Display application about }

procedure TfTitration.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry: Titration of a strong acid by a strong base ';
  S += 'and titration of a strong base by a strong acid.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March-April 2021.';
  MessageDlg('About "SimpleTitration"', S, mtInformation, [mbOK], 0);
end;

{ Button "pH/Curve/Start/Pause/Resume" pushed: Perform corresponding action }

procedure TfTitration.btActionClick(Sender: TObject);

var
  Count, CountMax, Loop, I: Integer;
  Vol, EqVol: Real;
  Mess: string;
  OK, StopCalc: Boolean;

begin
  // Button "pH" pushed: Do pH calculation for actual solution
  if btAction.Caption = 'pH' then begin
    ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
    // Get solution data from form (user input)
    GetData(sAction, sTitration, aAcids, aBases, sAcid, sBase, rAcidVol, rAcidConc, rBaseVol, rBaseConc, Mess);
    if Mess = '' then begin
      // Calculate the pH
      DoCalculation(btAction.Caption, sAcid, sBase, rAcidVol, rBaseVol, rAcidConc, rBaseConc, rHQty, rOHQty, rPH);
      // Solution color: neutral, acidic or basic
      if rPH = 7 then
        clVolShape := clFuchsia
      else if rPH < 7 then
        clVolShape := clRed
      else
        clVolShape := clBlue;
      // Fill the flask (new solution color)
      FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
      edVol1.SetFocus;
    end;
  end
  // Button "Curve": Draw titration curve for actual solution and titrant
  else if btAction.Caption = 'Curve' then begin
    ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
    // Get solution and titrant data from form (user input)
    GetData(sAction, sTitration, aAcids, aBases, sAcid, sBase, rAcidVol, rAcidConc, rBaseVol, rBaseConc, Mess);
    if Mess = '' then begin
      I := 0; StopCalc := False; Count := -1;
      // Draw titration curve until solution total volume exceeds the volume at equivalence point by 1/3
      // Thus, drawing the curve beyond the equivalence point (and getting the equ. pt. at an "acceptable" position on the graph)
      while not StopCalc do begin
        // Calculate pH for actual solution (= initial solution + drop by drop addition of titrant)
        DoCalculation(btAction.Caption, sAcid, sBase, rAcidVol, rBaseVol, rAcidConc, rBaseConc, rHQty, rOHQty, rPH);
        // Ignore calculation if pH outside [0..14] limits
        if (rPH >= 0) and (rPH <= 14) then begin
          Inc(I); SetLength(aTitrationValues, I);
          if sTitration = 'ab' then
            aTitrationValues[I - 1].Volume := rBaseVol                         // volume value for acid-base titration
          else
            aTitrationValues[I - 1].Volume := rAcidVol;                        // volume value for base-acid titration
          aTitrationValues[I - 1].PH := rPH;                                   // corr. pH value
        end;
        // Add 1ml drop of titrant
        if sTitration = 'ab' then
          rBaseVol += 1
        else
          rAcidVol += 1;
        // Use a counter to determine when to stop the calculations (cf. above)
        if ((sTitration = 'ab') and (rPH > 7)) or ((sTitration = 'ba') and (rPH < 7)) then begin
          if Count = -1 then
            Count := 0; CountMax := Length(aTitrationValues) div 3;
          end;
        if Count >= 0 then begin
          Inc(Count);
          if Count > CountMax then
            StopCalc := True;
        end;
      end;
      // With the actual titration volume and pH values in an array, draw the titration curve
      DrawCurve(sTitration, imCurve, aTitrationValues, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom);
      // Calculate pH (and display all related values) for equivalence point
      if sTitration = 'ab' then begin
        rBaseVol := rAcidVol * (rAcidConc / rBaseConc);
        if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
          rBaseVol *= 2
        else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
          rBaseVol /= 2;
      end
      else begin
        rAcidVol := rBaseVol * (rBaseConc / rAcidConc);
        if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
          rAcidVol *= 2
        else if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
          rAcidVol /= 2;
      end;
      DoCalculation('pH', sAcid, sBase, rAcidVol, rBaseVol, rAcidConc, rBaseConc, rHQty, rOHQty, rPH);
      laEndPoint.Visible := True;
      rVolFlask := rAcidVol + rBaseVol;
      clVolShape := clFuchsia;                                                 // neutral solution at equivalence point
      FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
      edVol1.SetFocus;
    end;
  end
  // Button "Start" pushed: Start a titration simulation
  // The code of the simulation itself is contained in the timer routine (addition of 1ml titrant per second)
  else if btAction.Caption = 'Start' then begin
    ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
    // Get solution and titrant data from form (user input)
    GetData(sAction, sTitration, aAcids, aBases, sAcid, sBase, rAcidVol, rAcidConc, rBaseVol, rBaseConc, Mess);
    if Mess = '' then begin
      // Initialize simulation: To run the simulation, the (unknown) concentration of the solution must be known
      // by the application. If the user did not enter it (simulation mode = concentration), calculate it from
      // user entered volume (simulation mode = equivalence point volume) or set it to a random value
      if mOptionsSimulationUConc.Checked then begin
        // Simulation mode = concentration: Calculate equ. pt. volume of titrant
        // This is needed to determine the equ. pt. solution total volume in
        // order to be able to do a proper scaling of the titration graph x-axis
        if sTitration = 'ab' then begin
          rBaseVol := rAcidVol * (rAcidConc / rBaseConc);
          if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
            rBaseVol *= 2
          else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
            rBaseVol /= 2;
        end
        else begin
          rAcidVol := rBaseVol * (rBaseConc / rAcidConc);
          if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
            rAcidVol /= 2
          else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
            rAcidVol *= 2;
        end;
        rVolTotal := rAcidVol + rBaseVol;
      end
      else if mOptionsSimulationUVol.Checked then begin
        // Simulation mode = equ. pt. volume: Calculate solution concentration
        rVolTotal := rAcidVol + rBaseVol;
        if sTitration = 'ab' then begin
          rAcidConc := rBaseConc * (rBaseVol / rAcidVol);
          if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
            rAcidConc /= 2
          else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
            rAcidConc *= 2;
        end
        else begin
          rBaseConc := rAcidConc * (rAcidVol / rBaseVol);
          if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
            rBaseConc *= 2
          else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
            rBaseConc /= 2;
        end;
      end
      else begin
        // Simulation mode = random concentration: Determine an "acceptable" random solution concentration
        if sTitration = 'ab' then
          Vol := rAcidVol
        else
          Vol := rBaseVol;
        Loop := 1;                                                             // variable used to avoid an infinite loop if no "acceptable" value found
        // Get random equ. pt. volume until "acceptable" value found (or value set, if cannot find any)
        repeat
          OK := True;
          if Loop = 200 then
            // If no "acceptable" value could be found, just set the equ. pt. volume of the titrant equal to the solution volume
            EqVol := Vol
          else
            // Get random titrant equ. pt. volume. Choose it in order to be at least 10ml and the total volume fitting in the flask
            EqVol := Random(Round(rVolFlaskTotal - Vol - 11)) + 10;
          if sTitration = 'ab' then
            rBaseVol := EqVol
          else
            rAcidVol := EqVol;
          rVolTotal := rAcidVol + rBaseVol;
          // Now possible to calculate the solution concentration
          if sTitration = 'ab' then begin
            rAcidConc := rBaseConc * (rBaseVol / rAcidVol);
            if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
              rAcidConc /= 2
            else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
              rAcidConc *= 2;
          end
          else begin
            rBaseConc := rAcidConc * (rAcidVol / rBaseVol);
            if (LeftStr(sAcid, 2) = 'H2') and (RightStr(sBase, 5) <> '(OH)2') then
              rBaseConc *= 2
            else if (LeftStr(sAcid, 2) <> 'H2') and (RightStr(sBase, 5) = '(OH)2') then
              rBaseConc /= 2;
          end;
          // Arbitrarily choosen limits of the concentration 0.05M - 2M (ignore this if no random equ. pt. volume has been found)
          if Loop < 200 then begin
            if (sTitration = 'ab') and ((rAcidConc < 0.05) or (rAcidConc > 2)) then
              OK := False
            else if (sTitration = 'ba') and ((rBaseConc < 0.05) or (rBaseConc > 2)) then
              OK := False;
          end;
          Inc(Loop);
        until OK or (Loop > 200);
      end;
      // Initialize simulation
      if sTitration = 'ab' then begin
        shDrop.Brush.Color := clBlue;                                          // titrant is a base
        rBaseVol := 0;                                                         // initial solution contains no base
      end
      else begin
        shDrop.Brush.Color := clRed;                                           // titrant is an acid
        rAcidVol := 0;                                                         // initial solution contains no acid
      end;
      // Display the titrant drop
      shDrop.Pen.Color := shDrop.Brush.Color;
      shDrop.Top := 336;
      shDrop.Visible := True;
      iStep := 1;                                                              // counter (cf. timer routine)
      rOldVol := -1; rOldPH := -1;                                             // values of previous step (used for titration curve line drawing)
      btAction.Caption := 'Pause';
      btAction.SetFocus;
      tiTitration.Enabled := True;                                             // start the simulation timer
    end;
  end
  // Button "Pause" pushed: Pause the simulation (stop the timer)
  else if btAction.Caption = 'Pause' then begin
    tiTitration.Enabled := False;
    btAction.Caption := 'Resume';
    btAction.SetFocus;
  end
  // Button "Resume" pushed: Resume the simulation (restart the timer)
  else begin
    shDrop.Top := 336;
    tiTitration.Enabled := True;
    btAction.Caption := 'Pause';
    btAction.SetFocus;
  end;
end;

{ Titration simulation (timer routine) }

procedure TfTitration.tiTitrationTimer(Sender: TObject);

var
  X, Y, OldX, OldY: Integer;
  Vol, PHMax, VolMax: Real;
  Title: string;

begin
  if iStep = 1 then begin
    // New titrant drop
    shDrop.Top := 336; shDrop.Visible := True;
  end
  else begin
    // Drop falling down into the flask
    shDrop.Top := 336 + iStep * 46;
  end;
  // For each 0.1s, add 0.1ml of titrant (not really correct, as titrant drops are 1ml)
  if sTitration = 'ab' then begin
    rBaseVol += 0.1; Vol := rBaseVol;
  end
  else begin
    rAcidVol += 0.1; Vol := rAcidVol;
  end;
  if iStep = 10 then begin
    // Once a second, do pH calculation with display of titration values
    DoCalculation('pH', sAcid, sBase, rAcidVol, rBaseVol, rAcidConc, rBaseConc, rHQty, rOHQty, rPH);
  end
  else begin
    // Other times, just do the calculations
    DoCalculation(btAction.Caption, sAcid, sBase, rAcidVol, rBaseVol, rAcidConc, rBaseConc, rHQty, rOHQty, rPH);
  end;
  // Determine actual volume of solution in the flask. If flask full, terminate the simulation
  rVolFlask := rAcidVol + rBaseVol;
  if rVolFlask > rVolFlaskTotal then begin
    tiTitration.Enabled := False;
    MessageDlg('SimpleTitration', 'Flask full - Simulation terminated!', mtError, [mbOK], 0);
    shDrop.Visible := False;
    btAction.Caption := 'Start';
    edVol1.SetFocus;
  end
  // Fill the flask (with actual total solution volume and solution color depending on neutral, acidic, basic)
  else begin
    if Abs(rpH - 7) < 1E-6 then
      clVolShape := clFuchsia
    else if rpH < 7 then
      clVolShape := clRed
    else
      clVolShape := clBlue;
    FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
    // Real-time update of the titration curve
    imCurve.Picture.Bitmap.Canvas.Pen.Width := 2;
    imCurve.Picture.Bitmap.Canvas.Pen.Color := clGreen;
    VolMax := rVolTotal; PHMax := 14;
    OldX := Round((rOldVol / VolMax) * (iGraphRight - iGraphLeft)) + iGraphLeft;
    X := Round((Vol / VolMax) * (iGraphRight - iGraphLeft)) + iGraphLeft;      // x-value (volume) on graph
    OldY := iGraphBottom - Round((rOldPH / PHMax) * (iGraphBottom - iGraphTop));
    Y := iGraphBottom - Round((rPH / PHMax) * (iGraphBottom - iGraphTop));     // y-value (pH) on graph
    imCurve.Picture.Bitmap.Canvas.Pen.Color := clGreen;
    imCurve.Picture.Bitmap.Canvas.Pen.Width := 1;
    if rOldVol = -1 then begin
      // At the start of the simulation, display title and draw axes
      // I put the code for this here (and not in the btActionClick method), to avoid the "disappearance" of the
      // axes, due to a call to ClearAll when the EditingDone method is called (case, where the simulation mode
      // requires user input)
      Title := 'Titration of ' + edVol1.Text + ' ml of ';
      if sTitration = 'ab' then begin
        Title += cobAcids.Text + ' by ' + cobBases.Text + ' ' + edConc2.Text + ' M';
        DrawAxis(imCurve, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, Title, sBase + ' [ml]', 'pH');
      end
      else begin
        Title += cobBases.Text + ' by ' + cobAcids.Text + ' ' + edConc2.Text + ' M';
        DrawAxis(imCurve, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, Title, sAcid + ' [ml]', 'pH')
      end;
      if (Y <= iGraphBottom) and (Y >= iGraphTop) then
        imCurve.Picture.Bitmap.Canvas.MoveTo(X, Y);                            // start curve drawing (if pH in [0..14] limits)
    end
    else begin
      // Draw the curve by drawing lines from the previous to the actual point
      if (Y <= iGraphBottom) and (Y >= iGraphTop) then
        imCurve.Picture.Bitmap.Canvas.Line(OldX, OldY, X, Y);                  // continue curve (if pH in [0..14] limits)
    end;
    rOldVol := Vol; rOldPH := rPH;
    // If the equivalence point has been reached, stop the simulation
    if (iStep = 10) and (Abs(rPH - 7) < 1E-6) then begin
      tiTitration.Enabled := False;
      shDrop.Visible := False;
      if mOptionsSimulationUVol.Checked or mOptionsSimulationRandom.Checked then begin
        // Display the (originally unknown and now determined) value of the original solution's concentration
        if sTitration = 'ab' then begin
          edConc1.Text := RFormat(rAcidConc, -1)
        end
        else
          edConc1.Text := RFormat(rBaseConc, -1);
        edConc1.Alignment := taRightJustify;
      end;
      btAction.Caption := 'Start';
      edVol1.SetFocus;
    end;
    // Increnment step counter (value between 1 and 10 = 10 steps for a 1ml drop of titrant added)
    Inc(iStep);
    if iStep > 10 then begin
      iStep := 1;
      shDrop.Visible := False;
    end;
  end;
end;

{ Changes in acid/base selection (combobox) }

procedure TfTitration.cobAcidsChange(Sender: TObject);

begin
  ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

procedure TfTitration.cobBasesChange(Sender: TObject);

begin
  ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

{ Changes in acid/base volume (edit field) }

procedure TfTitration.edVol1EditingDone(Sender: TObject);

var
  Vol1, Vol2: Real;

begin
  ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
  if edVol1.Text = '' then
    Vol1 := 0
  else
    Vol1 := StrToFloat(edVol1.Text);
  if sAction = 'pH' then begin
    if edVol2.Text = '' then
      Vol2 := 0
    else
      Vol2 := StrToFloat(edVol2.Text);
    clVolShape := clGreen;
  end
  else
    Vol2 := 0;
  rVolFlask := Vol1 + Vol2;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

procedure TfTitration.edVol2EditingDone(Sender: TObject);

var
  Vol1, Vol2: Real;

begin
  ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
  if edVol1.Text = '' then
    Vol1 := 0
  else
    Vol1 := StrToFloat(edVol1.Text);
  if sAction = 'pH' then begin
    if edVol2.Text = '' then
      Vol2 := 0
    else
      Vol2 := StrToFloat(edVol2.Text);
    clVolShape := clGreen;
  end
  else
    Vol2 := 0;
  rVolFlask := Vol1 + Vol2;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

{ Changes in acid/base concentration (edit field) }

procedure TfTitration.edConc1EditingDone(Sender: TObject);

var
  Vol1, Vol2: Real;

begin
  ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
  if edVol1.Text = '' then
    Vol1 := 0
  else
    Vol1 := StrToFloat(edVol1.Text);
  if sAction = 'pH' then begin
    if edVol2.Text = '' then
      Vol2 := 0
    else
      Vol2 := StrToFloat(edVol2.Text);
    clVolShape := clGreen;
  end
  else
    Vol2 := 0;
  rVolFlask := Vol1 + Vol2;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

procedure TfTitration.edConc2EditingDone(Sender: TObject);

var
  Vol1, Vol2: Real;

begin
  ClearAll(sAction, sTitration, rVolFlask, clVolShape, bFlaskFull, False);
  if edVol1.Text = '' then
    Vol1 := 0
  else
    Vol1 := StrToFloat(edVol1.Text);
  if sAction = 'pH' then begin
    if edVol2.Text = '' then
      Vol2 := 0
    else
      Vol2 := StrToFloat(edVol2.Text);
    clVolShape := clGreen;
  end
  else
    Vol2 := 0;
  rVolFlask := Vol1 + Vol2;
  FillFlask(rVolFlaskTotal, rVolFlask, bFlaskFull, aVolShapes, iVolShapeTotal, clVolShape);
end;

end.

