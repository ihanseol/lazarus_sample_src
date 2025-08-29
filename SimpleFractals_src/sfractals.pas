{********************************************}
{* Main unit for SimpleFractals application *}
{********************************************}

unit sfractals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, LCLIntf;

type
  TRule = record
    Rule, VarName, VarReplace: string;
  end;
  TRules = array of TRule;
  {*************}
  { TfSFractals }
  {*************}
  TfSFractals = class(TForm)
    mMenu: TMainMenu;
    mFractal, mFractalNew, mFractalOpen, mFractalSaveAs, mFractalExit: TMenuItem;
    mOptions, mOptionsAlphabet, mOptionsAlphabetSwapTurn, mOptionsAlphabetChangeMove, mOptionsAlphabetColors: TMenuItem;
    mOptionsDrawing, mOptionsDrawing1, mOptionsDrawing2, mOptionsDrawing3, mOptionsDrawing4: TMenuItem;
    mHelp, mHelpLSystems, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imDraw: TImage;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edX0, edY0, edD: TEdit;
    edFractal: TMemo;
    btDraw: TButton;
    dlgOpenFractals: TOpenDialog;
    dlgSaveFractals: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mFractalNewClick(Sender: TObject);
    procedure mFractalOpenClick(Sender: TObject);
    procedure mFractalSaveAsClick(Sender: TObject);
    procedure mFractalExitClick(Sender: TObject);
    procedure mOptionsAlphabetSwapTurnClick(Sender: TObject);
    procedure mOptionsAlphabetChangeMoveClick(Sender: TObject);
    procedure mOptionsAlphabetColorsClick(Sender: TObject);
    procedure mOptionsDrawing1Click(Sender: TObject);
    procedure mOptionsDrawing2Click(Sender: TObject);
    procedure mOptionsDrawing3Click(Sender: TObject);
    procedure mOptionsDrawing4Click(Sender: TObject);
    procedure mHelpLSystemsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure edDEditingDone(Sender: TObject);
    procedure edX0EditingDone(Sender: TObject);
    procedure edY0EditingDone(Sender: TObject);
  private
    sDir, sFilename: string;
    bOpenFile: Boolean;
    clFractal, clBackground: TColor;
    Bitmap: TBitmap;
  end;

var
  fSFractals: TfSFractals;

implementation

{$R *.lfm}

{ Clear the drawing surface by drawing a white or black rectangle }

procedure SurfaceClear(var Draw: TImage; Background: TColor);

begin
  Draw.Picture.Bitmap.Canvas.Pen.Width := 1;
  Draw.Picture.Bitmap.Canvas.Pen.Color := clSilver;
  Draw.Picture.Bitmap.Canvas.Brush.Color := Background;
  Draw.Picture.Bitmap.Canvas.Rectangle(0, 0, Draw.Width, Draw.Height);
end;

{ Clear the form: Reset input fields and clear drawing surface}

procedure FormClear(var X0, Y0, D: TEdit; var Fractal: TMemo; var Draw: TImage; Background: TColor);

begin
  X0.Text := ''; Y0.Text := ''; D.Text := '';
  Fractal.Lines.Clear;
  SurfaceClear(Draw, Background);
end;

{ Get local settings decimal separator }

function DecimalSeparator: Char;

var
  R: string;

begin
  R := FloatToStr(1.1);
  Result := Copy(R, 2, 1)[1];
end;

{ Get degree angle in radians }

function DegToRad(A: real): Real;

begin
  Result := (A / 360) * 2 * Pi;
end;

{ Check if a string is a positive numeral }

function IsPositiveNumber(S, F: string): Boolean;

var
  SepCount, I: Integer;
  ItIs: Boolean;

begin
  ItIs := True;
  SepCount := 0;
  for I := 1 to Length(S) do begin
    if not (S[I] in ['0'..'9', DecimalSeparator]) then
      ItIs := False
    else if S[I] = DecimalSeparator then
      Inc(SepCount);
  end;
  if ((F = 'real') and (SepCount > 1)) or ((F = 'integer') and (SepCount > 0)) then
    ItIs := False;
  Result := ItIs;
end;

{ Check L-System alphabet validity }

function CheckValidAlphabet(S: string): string;

var
  I: Integer;
  Mess: string;

begin
  Mess := '';
  for I := 1 to Length(S) do begin
    if Mess = '' then begin
      if not (Copy(S, I, 1)[1] in ['F', 'G', '+', '-', 'V', 'W', 'X', 'Y', 'Z', '0'..'9']) then
        Mess := 'Invalid axiom alphabet: Unknown symbol'
      else begin
        if (not fSFractals.mOptionsAlphabetColors.Checked) and (Copy(S, I, 1)[1] in ['0'..'9']) then
          // Allow color codes 0..9 only if they are enabled in the "Options" menu
          Mess := 'Invalid axiom alphabet: Color codes not enabled';
      end;
    end;
  end;
  Result := Mess;
end;

{ Check L-System rules validity }

function CheckValidRule(var Rule: TRule): string;

var
  Mess: string;

begin
  Mess := '';
  if (LeftStr(Rule.Rule, 1) <> '"') or (Copy(Rule.Rule, 3, 2) <> '":') then
    Mess := 'Invalid rules format'
  else begin
    if not (Copy(Rule.Rule, 2, 1)[1] in ['F', 'G', '+', '-', 'V', 'W', 'X', 'Y', 'Z']) then
      Mess := 'Invalid rules alphabet: Invalid variable'
    else begin
      Rule.VarName := Copy(Rule.Rule, 2, 1);
      Delete(Rule.Rule, 1, 4);
      if (LeftStr(Rule.Rule, 1) <> '"') or (RightStr(Rule.Rule, 1) <> '"') then
        Mess := 'Invalid rules transformation string'
      else begin
        Rule.VarReplace := Copy(Rule.Rule, 2, Length(Rule.Rule) - 2);
        Mess := CheckValidAlphabet(Rule.VarReplace);
        if Mess <> '' then
          Mess := StringReplace(Mess, 'axiom', 'rules', []);
      end;
    end;
  end;
  Result := Mess;
end;

{ Remove alphabet color codes from L-System definition }

procedure RemoveColorCodes(var Memo: TMemo);

var
  I, J: Integer;
  Line: string;
  IsAxiom, IsRule, DoRemove: Boolean;

begin
  IsAxiom := False; IsRule := False; I := 0;
  while I < Memo.Lines.Count do begin
    Line := Memo.Lines[I];
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      DoRemove := False;
      if LeftStr(Memo.Lines[I], 5) = 'axiom' then
        IsAxiom := True                                                        // this is the axiom line
      else if LeftStr(Memo.Lines[I], 5) = 'rules' then
        IsRule := True;                                                        // this is the first rule line
      if IsAxiom then begin
        // If the line is the axiom line, remove the color code
        DoRemove := True;
        IsAxiom := False;
      end;
      if IsRule then begin
        // If the line is a rule line, remove the color code
        DoRemove := True;
        if RightStr(Memo.Lines[I], 1) = '}' then
          // If the line does not contain the end-of-rules-definition symbol '}',
          // the following line will be another rule line (with color code removal)
          IsRule := False;
      end;
      if DoRemove then begin
        // If line is axiom or rule line, do remove all color codes
        for J := 0 to 9 do
          Memo.Lines[I] := StringReplace(Memo.Lines[I], IntToStr(J), '', [rfReplaceAll]);
      end;
    end;
    Inc(I);
  end;
end;

{ Move the turtle as defined by the turtle command string }

procedure MoveTurtle(var Draw: TImage; TurtleCommands: string; L, X1, Y1: Integer; Angle: Real; ColFractal, ColBg: TColor);

const
  clPink = $B469FF; clOrange = $00A5FF; clPurple = $DB7093;
  Colours: array[0..9] of TColor = (
    clRed, clOrange, clYellow, clLime, clAqua, clBlue, clPurple, clFuchsia, clPink, clWhite
  );

var
  X2, Y2, I: Integer;
  Angle1, Angle2: Real;
  Command: Char;
  Colour: TColor;

begin
  SurfaceClear(Draw, ColBg);
  if colFractal = ColBg then
    Colour := clLime                                                           // green on black background if color drawing with color codes disabled
  else
    Colour := colFractal;                                                      // fractal color on background color
  Draw.Picture.Bitmap.Canvas.Pen.Color := Colour;
  Draw.Picture.Bitmap.Canvas.Pen.Width := 1;
  Angle1 := 0;                                                                 // turtle starting angle
  for I := 1 to Length(TurtleCommands) do begin
    // Do for each turtle command of the command string
    Command := TurtleCommands[I];
    case Command of
      '0'..'9': begin
        if ColFractal = colBg then
          // If color drawing and color codes are enabled, change fractal color
          Draw.Picture.Bitmap.Canvas.Pen.Color := Colours[StrToInt(TurtleCommands[I])];
      end;
      'F': begin
        // Move the turtle forward while drawing a line
        X2 := Round(X1 + L * Cos(DegToRad(Angle1))); Y2 := Round(Y1 - L * Sin(DegToRad(Angle1)));
        Draw.Picture.Bitmap.Canvas.Line(X1, Y1, X2, Y2);
        X1 := X2; Y1 := Y2;
      end;
      'G': begin
        // Move the turtle forward without drawing a line
        X2 := Round(X1 + L * Cos(DegToRad(Angle1))); Y2 := Round(Y1 - L * Sin(DegToRad(Angle1)));
        Draw.Picture.Bitmap.Canvas.MoveTo(X2, Y2);
        X1 := X2; Y1 := Y2;
      end;
      '+': begin
        // Turn the turtle to the right
        Angle2 := Angle1 - Angle;
        if Angle2 < -360 then
          Angle2 += 360;
        Angle1 := Angle2;
      end;
      '-': begin
        // Turn the turtle to the left
        Angle2 := Angle1 + Angle;
        if Angle2 > 360 then
          Angle2 -= 360;
        Angle1 := Angle2;
      end;
    end;
  end;
end;

{ Draw the fractal (for actual L-System) }

procedure DrawFractal(var Draw: TImage; X, Y, L, Iterations: Integer; Axiom: string; var Rules: TRules; Angle: Real; ColFractal, ColBg: TColor);

var
  I, J: Integer;
  TurtleCommands, NewTurtleCommands: string;

begin
  // Move the turtle as defined by the L-System axiom
  TurtleCommands := Axiom;
  MoveTurtle(Draw, TurtleCommands, L, X, Y, Angle, ColFractal, ColBg);
  // Do specified number of iterations
  for I := 1 to Iterations do begin
    // Get new turtle command string by applying the rules to the actual one
    // The usage of lowercase letters makes sure that all rules are applied
    // to the actual command string (i.e. are applied simultaneously)
    NewTurtleCommands := TurtleCommands;
    for J := 0 to Length(Rules) - 1 do begin
      NewTurtleCommands := StringReplace(NewTurtleCommands, Rules[J].VarName, LowerCase(Rules[J].VarReplace), [rfReplaceAll]);
    end;
    TurtleCommands := UpperCase(NewTurtleCommands);
    // Move the turtle as defined by the new command string
    MoveTurtle(Draw, TurtleCommands, L, X, Y, Angle, ColFractal, ColBg);
  end;
end;

{*************}
{ TfSFractals }
{*************}

{ Application start: Initialisation }

procedure TfSFractals.FormCreate(Sender: TObject);

var
  Param: string;

begin
  // Create a bitmap object and assign it to the image component (the drawing surface)
  Bitmap := TBitmap.Create;
  Bitmap.Width := imDraw.Width;
  Bitmap.Height := imDraw.Height;
  imDraw.Picture.Graphic := Bitmap;
  // Initial variable values
  clFractal := clBlack; clBackground := clBlack; bOpenFile := False;
  sDir := GetCurrentDir + '/samples'; DoDirSeparators(sDir);
  FormClear(edX0, edY0, edD, edFractal, imDraw, clBackground);
  // If the application has been opened by double-clicking a .sfl file,
  // get this file's path and set the bOpenFile variable to True.
  // When the form shows up (FormActivate method) with bOpenFile=True,
  // the corresponding L-System will be loaded
  sFilename := '';
  if ParamCount > 0 then begin
    Param := ParamStr(1);
    if ExtractFileExt(Param) = '.sfl' then begin
      sFilename := Param;
      bOpenFile := True;
    end;
  end;
end;

{ Initial application window show-up: Open L-system file (if application was started by double-clicking a .sfl file) }

procedure TfSFractals.FormActivate(Sender: TObject);

// Placing this code here (and not within the FormCreate method) makes sure all
// form components have been created when the L-System definition is loaded into the TMemo field

begin
  if bOpenFile then begin
    // Clear the form
    FormClear(edX0, edY0, edD, edFractal, imDraw, clBackground);
    // Load the L-System from file
    edFractal.Lines.LoadFromFile(sFilename);
    sDir := ExtractFilePath(sFilename);
    // This method has to be executed at application start only. To "block it" up from
    // this moment, set bOpenFile to False
    bOpenFile := False;
  end;
end;

{ Menu item "Fractal > New": Prepare for a new fractal drawing }

procedure TfSFractals.mFractalNewClick(Sender: TObject);

begin
  FormClear(edX0, edY0, edD, edFractal, imDraw, clBackground);
  sFilename := '';
end;

{ Menu item "Fractal > Open ...": Load fractal L-System definition from file }

procedure TfSFractals.mFractalOpenClick(Sender: TObject);

begin
  dlgOpenFractals.InitialDir := sDir; dlgOpenFractals.Filename := '';
  if dlgOpenFractals.Execute then begin
    sFilename := dlgOpenFractals.Filename;
    FormClear(edX0, edY0, edD, edFractal, imDraw, clBackground);
    edFractal.Lines.LoadFromFile(sFilename);
    if not mOptionsAlphabetColors.Checked then
      // Remove color codes (if colors are disabled in the "Options" menu)
      RemoveColorCodes(edFractal);
    sDir := ExtractFilePath(sFilename);                                        // remember directory
  end;
end;

{ Menu item "Fractal > Save As ...": Save fractal L-System definition to file }

procedure TfSFractals.mFractalSaveAsClick(Sender: TObject);

var
  Ret: Cardinal;
  FileExt: string;
  DoSave: Boolean;

begin
  if edFractal.Text <> '' then begin;
    dlgSaveFractals.InitialDir := sDir;
    sFileName := ExtractFilename(sFilename); FileExt := ExtractFileExt(sFilename);
    sFilename := StringReplace(sFilename, Fileext, '', []); sFilename += '.sfl';
    dlgSaveFractals.FileName := sFilename;
    if dlgSaveFractals.Execute then begin
      sFilename := dlgSaveFractals.FileName; DoSave := True;
      if FileExists(sFilename) then begin
        // Override existing file only, if user wants so
        DoSave := False;
        Ret := MessageDlg('Save file as', 'File already exists! Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
        if Ret = mrYes then
          DoSave := True;
      end;
      if DoSave then begin
        edFractal.Lines.SaveToFile(sFilename);
        sDir := ExtractFileDir(sFilename);                                     // remember directory
      end;
    end;
  end;
end;

{ Menu item "Fractal > Exit": Exit application }

procedure TfSFractals.mFractalExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Alphabet > Use - to turn right, + to turn left": Toggle turning commands swapping }

procedure TfSFractals.mOptionsAlphabetSwapTurnClick(Sender: TObject);

begin
  if mOptionsAlphabetSwapTurn.Checked then
    mOptionsAlphabetSwapTurn.Checked := False
  else
    mOptionsAlphabetSwapTurn.Checked := True;
end;

{ Menu item "Options > Alphabet > Use f to move without drawing": Toggle usage of G or f for move without drawing }

procedure TfSFractals.mOptionsAlphabetChangeMoveClick(Sender: TObject);

begin
  if mOptionsAlphabetChangeMove.Checked then
    mOptionsAlphabetChangeMove.Checked := False
  else
    mOptionsAlphabetChangeMove.Checked := True;
end;

{ Menu item "Options > Alphabet > Enable alphabet color codes": Toggle enabling/disabling of alphabet color codes }

procedure TfSFractals.mOptionsAlphabetColorsClick(Sender: TObject);

begin
  if mOptionsAlphabetColors.Checked then begin
    mOptionsAlphabetColors.Checked := False;
    if mOptionsDrawing4.Checked then
      mOptionsDrawing3.Click;
    mOptionsDrawing4.Enabled := False;
  end
  else begin
    mOptionsAlphabetColors.Checked := True;
    mOptionsDrawing4.Enabled := True;
  end;
end;

{ Menu items "Options > Drawing > ...": Select colors of fractal and drawing surface background }

procedure TfSFractals.mOptionsDrawing1Click(Sender: TObject);

begin
  mOptionsDrawing1.Checked := True; mOptionsDrawing2.Checked := False;
  mOptionsDrawing3.Checked := False; mOptionsDrawing4.Checked := False;
  clFractal := clBlack; clBackground := clWhite;                               // black on white background
end;

procedure TfSFractals.mOptionsDrawing2Click(Sender: TObject);

begin
  mOptionsDrawing1.Checked := False; mOptionsDrawing2.Checked := True;
  mOptionsDrawing3.Checked := False; mOptionsDrawing4.Checked := False;
  clFractal := clWhite; clBackground := clBlack;                               // white on black background
end;

procedure TfSFractals.mOptionsDrawing3Click(Sender: TObject);

begin
  mOptionsDrawing1.Checked := False; mOptionsDrawing2.Checked := False;
  mOptionsDrawing3.Checked := True; mOptionsDrawing4.Checked := False;
  clFractal := clLime; clBackground := clBlack;                                // green on black background
end;

procedure TfSFractals.mOptionsDrawing4Click(Sender: TObject);

begin
  mOptionsDrawing1.Checked := False; mOptionsDrawing2.Checked := False;
  mOptionsDrawing3.Checked := False; mOptionsDrawing4.Checked := True;
  clFractal := clBlack; clBackground := clBlack;                               // colored on black background
end;

{ Menu item "Help > L-Systems": Open webbrowser with fractals and L-Systems help text }

procedure TfSFractals.mHelpLSystemsClick(Sender: TObject);

begin
  OpenDocument('lsystems.html');
end;

{ Menu item "Help > Help": Open webbrowser with application help text }

procedure TfSFractals.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('sfractals.html');
end;

{ Menu item "Help > About": Display application about }

procedure TfSFractals.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Fractals:' + LineEnding;
  S += 'Using a basic L-System to craete simple fractals.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February - May 2021.';
  MessageDlg('About "SimpleFractals"', S, mtInformation, [mbOK], 0);
end;

{ Buttton "Draw": Draw the fractal (with actual L-System definition) }

procedure TfSFractals.btDrawClick(Sender: TObject);

const
  XMax = 999; YMax = 649;
  MessTypes: array[1..3] of string = (
    'Invalid drawing data', 'Fractal error', 'Invalid fractal data'
  );

var
  X0, Y0, D, Iterations, MessType, L, R, N, I, P: Integer;
  Angle: Real;
  Axiom, Org, Line, Mess: string;
  NeedAngle: Boolean;
  Fractal: array of string;
  Rules: TRules;

begin
  Mess := '';
  X0 := -1; Y0 := -1; D := 0; Angle := 0;
  N := 0; SetLength(Fractal, N);
  if edFractal.Text <> '' then begin
    // Read L-System from TMemo field
    for I := 0 to edFractal.Lines.Count - 1 do begin
      Line := edFractal.Lines[I];
      // Remove comments
      P := Pos('#', Line);
      if P > 0 then
        Delete(Line, P, Length(Line) - P + 1);
      if Line <> '' then begin
        // Add current Memo line to array
        Inc(N);
        SetLength(Fractal, N);
        Fractal[N - 1] := StringReplace(Line, ' ', '', [rfReplaceAll]);
      end;
    end;
  end;
  // Check L-System validity
  if Length(Fractal) = 0 then begin
    MessType := 2; Mess := 'Missing fractal definition (L-System)';
    edFractal.SetFocus;
  end;
  // Parse fractal definition
  if Mess = '' then begin
    MessType := 3; L := 0;
    if (LeftStr(Fractal[L], 8) = 'origin=(') and (RightStr(Fractal[0], 1) = ')') then begin
      // Check origin parameter (if present)
      Org := Copy(Fractal[L], 9, Length(Fractal[L]) - 9);
      P := Pos(',', Org);
      if (P = 0) or (P = Length(Org)) or (not IsPositiveNumber(Copy(Org, 1, P - 1), 'integer')) or
        (not IsPositiveNumber(Copy(Org, P + 1, Length(Org)), 'integer')) then
        Mess := 'Invalid origin definition'
      else begin
        // Copy origin X- and Y-coordinates to corresponding edit fields
        edX0.Text := Copy(Org, 1, P - 1); edY0.Text := Copy(Org, P + 1, Length(Org));
      end;
      Inc(L);
    end;
  end;
  if Mess = '' then begin
    if LeftStr(Fractal[L], 13) = 'displacement=' then begin
      // Check displacement parameter (if present)
      if not IsPositiveNumber(Copy(Fractal[L], 14, Length(Fractal[L]) - 13), 'integer') then
        Mess := 'Invalid displacement definition'
      else
        // Copy displacement to corresponding edit fields
        edD.Text := Copy(Fractal[L], 14, Length(Fractal[L]) - 13);
      Inc(L);
    end;
  end;
  if Mess = '' then begin
    // Check axiom (must be present)
    if (LeftStr(Fractal[L], 7) <> 'axiom="') or (RightStr(Fractal[L], 1) <> '"') then
      Mess := 'Missing or invalid axiom definition'
    else begin
      Axiom := Copy(Fractal[L], 8, Length(Fractal[L]) - 8);
      if mOptionsAlphabetChangeMove.Checked then
        // If 'f' is used instead of 'G', change all 'f' to 'G'
        Axiom := StringReplace(Axiom, 'f', 'G', [rfReplaceAll]);
      if mOptionsAlphabetSwapTurn.Checked then begin
        // If '+' is used for 'turn left', exchange all '+' and '-'
        Axiom := StringReplace(Axiom, '+', '*', [rfReplaceAll]);
        Axiom := StringReplace(Axiom, '-', '+', [rfReplaceAll]);
        Axiom := StringReplace(Axiom, '*', '-', [rfReplaceAll]);
      end;
      // Check axiom's alphabet
      Mess := CheckValidAlphabet(Axiom);
    end;
  end;
  if Mess = '' then begin
    // Check rules (must be present)
    R := 0; SetLength(Rules, R);
    if (LeftStr(Fractal[L + 1], 7) <> 'rules={') or ((RightStr(Fractal[L + 1], 1) <> '}') and (RightStr(Fractal[L + 1], 1) <> ',')) then
      Mess := 'Missing or invalid rules definition'
    else begin
      // Add individual rules to array
      repeat
        Inc(L);
        Inc(R); SetLength(Rules, R);
        if R = 1 then
          Rules[R - 1].Rule := Copy(Fractal[L], 8, Length(Fractal[L]) - 8)
        else
          Rules[R - 1].Rule := Copy(Fractal[L], 1, Length(Fractal[L]) - 1);
        // Movement and turn commands adaption (as above)
        if mOptionsAlphabetChangeMove.Checked then
          Rules[R - 1].Rule := StringReplace(Rules[R - 1].Rule, 'f', 'G', [rfReplaceAll]);
        if mOptionsAlphabetSwapTurn.Checked then begin
          Rules[R - 1].Rule := StringReplace(Rules[R - 1].Rule, '+', '*', [rfReplaceAll]);
          Rules[R - 1].Rule := StringReplace(Rules[R - 1].Rule, '-', '+', [rfReplaceAll]);
          Rules[R - 1].Rule := StringReplace(Rules[R - 1].Rule, '*', '-', [rfReplaceAll]);
        end;
        // Check current rule's alphabet
        Mess := CheckValidRule(Rules[R - 1])
      until (R = Length(Fractal) - 1) or (RightStr(Fractal[L], 2) = '"}') or (Mess <> '');
      Inc(L);
    end;
  end;
  if Mess = '' then begin
    if R = Length(Fractal) - 1 then                                            // is the case, if rule closing '}' not found
      Mess := 'Invalid rule definition syntax';
  end;
  if Mess = '' then begin
    // Check iteration (must be present)
    if (LeftStr(Fractal[L], 11) <> 'iterations=') or
      not IsPositiveNumber(Copy(Fractal[L], 12, Length(Fractal[L]) - 11), 'integer') then
      Mess := 'Missing or invalid iterations definition'
    else begin
      Iterations := StrToInt(Copy(Fractal[L], 12, Length(Fractal[L]) - 11));
      Inc(L);
    end;
  end;
  if Mess = '' then begin
    // Check turning angle (may be omitted if there are no '+' or '-' commands)
    NeedAngle := False;
    // Check within axiom
    P := Pos('+', Axiom);
    if P = 0 then
      P := Pos('-', Axiom);
    if P <> 0 then
      NeedAngle := True
    // Check within rules
    else begin
      for I := 0 to Length(Rules) - 1 do begin
        P := Pos('+', Rules[I].Rule);
        if P = 0 then
          P := Pos('-', Rules[I].Rule);
        if P <> 0 then
          NeedAngle := True;
      end;
    end;
    if NeedAngle then begin
      // If the angle is mandatory, check if it is valid
      if (LeftStr(Fractal[L], 6) <> 'angle=') or
        not IsPositiveNumber(Copy(Fractal[L], 7, Length(Fractal[L]) - 6), 'real') then
        Mess := 'Missing or invalid angle'
      else
        Angle := StrToFloat(Copy(Fractal[L], 7, Length(Fractal[L]) - 6));
    end;
  end;
  if Mess = '' then begin
    // Check origin X- and Y-coordinates and displacement values (they may be entered/changed in the form edit fields)
    if edX0.Text <> '' then
      X0 := StrToInt(edX0.Text);
    if edY0.Text <> '' then
      Y0 := StrToInt(edY0.Text);
    if edD.Text <> '' then
      D := StrToInt(edD.Text);
    if (X0 < 0) or (X0 > XMax) then begin
      MessType := 1; Mess := 'Initial X-coordinate must be between 0 and ' + IntToStr(XMax);
      edX0.SetFocus;
    end
    else if (Y0 < 0) or (Y0 > YMax) then begin
      MessType := 1; Mess := 'Initial Y-coordinate must be between 0 and ' + IntToStr(YMax);
      edY0.SetFocus;
    end
    else if D <= 1 then begin
      MessType := 1; Mess := 'Displacement must be greater than 1';
      edD.SetFocus;
    end
  end;
  if Mess = '' then begin
    // If the L-System and the drawing parameters are valid, draw the fractal
    DrawFractal(imDraw, X0, Y0, D, Iterations, Axiom, Rules, Angle, clFractal, clBackground);
  end
  else begin
    // If the L-System or the drawing parameters are invalid, display error message
    MessageDlg(MessTypes[MessType], Mess, mtError, [mbOK], 0);
  end;
end;

{ Change L-System origin parameter, if user changes value in origin X-coordinate edit field }

procedure TfSFractals.edX0EditingDone(Sender: TObject);

var
  I, P: Integer;
  Line, Org: string;

begin
  if (edFractal.Text <> '') and (edX0.Text <> '') then begin
    for I := 0 to edFractal.Lines.Count - 1 do begin
      Line := StringReplace(edFractal.Lines[I], ' ', '', [rfReplaceAll]);
      if LeftStr(Line, 8) = 'origin=(' then begin
        Org := Copy(Line, 9, Length(Line) - 9);
        P := Pos(',', Org);
        Line := StringReplace(Line, Copy(Org, 1, P), edX0.Text + ',', []);
        Line := StringReplace(Line, '=', ' = ', []);
        edFractal.Lines[I] := Line;
      end;
    end;
  end;
end;

{ Change L-System origin parameter, if user changes value in origin Y-coordinate edit field }

procedure TfSFractals.edY0EditingDone(Sender: TObject);

var
  I, P: Integer;
  Line, Org: string;

begin
  if (edFractal.Text <> '') and (edY0.Text <> '') then begin
    for I := 0 to edFractal.Lines.Count - 1 do begin
      Line := StringReplace(edFractal.Lines[I], ' ', '', [rfReplaceAll]);
      if LeftStr(Line, 8) = 'origin=(' then begin
        Org := Copy(Line, 9, Length(Line) - 9);
        P := Pos(',', Org);
        Line := StringReplace(Line, Copy(Org, P, Length(Org)), ',' + edY0.Text, []);
        Line := StringReplace(Line, '=', ' = ', []);
        edFractal.Lines[I] := Line;
      end;
    end;
  end;
end;

{ Change L-System displacement parameter, if user changes value in displacement edit field }

procedure TfSFractals.edDEditingDone(Sender: TObject);

var
  I: Integer;
  Line: string;

begin
  if (edFractal.Text <> '') and (edD.Text <> '') then begin
    for I := 0 to edFractal.Lines.Count - 1 do begin
      Line := StringReplace(edFractal.Lines[I], ' ', '', [rfReplaceAll]);
      if LeftStr(Line, 13) = 'displacement=' then begin
        Line := StringReplace(Line, Copy(Line, 14, Length(Line)), edD.Text, []);
        Line := StringReplace(Line, '=', ' = ', []);
        edFractal.Lines[I] := Line;
      end;
    end;
  end;
end;

end.

