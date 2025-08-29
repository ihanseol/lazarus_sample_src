{***************************************}
{* Main unit for "Mirrors" application *}
{***************************************}

unit mirrors_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, PopupNotifier, mirrors_u2;

type
  TFieldShapes = array[0..9, 0..9] of TShape;
  TCanonButtons = array[0..3, 0..9] of TButton;
  TFields = array[0..9, 0..9] of Char;
  { TfMirrors }
  TfMirrors = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsMLeft, mOptionsMLeft0, mOptionsMLeft1, mOptionsMLeft2, mOptionsMLeft3, mOptionsMLeft4: TMenuItem;
    mOptionsMRight, mOptionsMRight0, mOptionsMRight1, mOptionsMRight2, mOptionsMRight3, mOptionsMRight4: TMenuItem;
    mOptionsErrors, MenuItem15, mOptionsShow: TMenuItem;
    mLevel, mLevel0, mLevel1, mLevel2, mLevel3, mLevel4: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    edLevel: TEdit;
    edMirrors: TEdit;
    edErrors: TEdit;
    shField00, shField01, shField02, shField03, shField04, shField05, shField06, shField90, shField91, shField92: TShape;
    shField93, shField94, shField95, shField96, shField80, shField81, shField82, shField83, shField84, shField85: TShape;
    shField70, shField71, shField72, shField73, shField74, shField75, shField60, shField61, shField62, shField63: TShape;
    shField64, shField50, shField51, shField52, shField53, shField40, shField41, shField42, shField30, shField31: TShape;
    shField20, shField10, shField18, shField19, shField11, shField12, shField13, shField14, shField15, shField21: TShape;
    shField28, shField29, shField22, shField23, shField24, shField25, shField32, shField38, shField39, shField33: TShape;
    shField34, shField35, shField43, shField48, shField49, shField44, shField45, shField54, shField58, shField59: TShape;
    shField55, shField65, shField68, shField69, shField07, shField08, shField09, shField16, shField17, shField26: TShape;
    shField27, shField36, shField37, shField46, shField47, shField56, shField57, shField66, shField67, shField78: TShape;
    shField79, shField76, shField77, shField86, shField88, shField89, shField87, shField97, shField98, shField99: TShape;
    btR0, btR1, btR2, btR3, btR4, btR5, btR6, btR7, btR8, btR9: TButton;
    btT0, btT1, btB0, btB1, btB2, btB3, btB4, btB5, btB6, btB7: TButton;
    btB8, btB9, btT2, btT3, btT4, btT5, btT6, btT7, btT8, btT9: TButton;
    btL0, btL9, btL1, btL2, btL3, btL4, btL5, btL6, btL7, btL8: TButton;
    shShotH, shShotV: TShape;
    shLegendLeft, shLegendRight: TShape;
    shLegendShot: TShape;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mLevel0Click(Sender: TObject);
    procedure mLevel1Click(Sender: TObject);
    procedure mLevel2Click(Sender: TObject);
    procedure mLevel3Click(Sender: TObject);
    procedure mLevel4Click(Sender: TObject);
    procedure mOptionsMLeft0Click(Sender: TObject);
    procedure mOptionsMLeft1Click(Sender: TObject);
    procedure mOptionsMLeft2Click(Sender: TObject);
    procedure mOptionsMLeft3Click(Sender: TObject);
    procedure mOptionsMLeft4Click(Sender: TObject);
    procedure mOptionsMRight0Click(Sender: TObject);
    procedure mOptionsMRight1Click(Sender: TObject);
    procedure mOptionsMRight2Click(Sender: TObject);
    procedure mOptionsMRight3Click(Sender: TObject);
    procedure mOptionsMRight4Click(Sender: TObject);
    procedure mOptionsErrorsClick(Sender: TObject);
    procedure mOptionsShowClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btB0Click(Sender: TObject);
    procedure btB1Click(Sender: TObject);
    procedure btB2Click(Sender: TObject);
    procedure btB3Click(Sender: TObject);
    procedure btB4Click(Sender: TObject);
    procedure btB5Click(Sender: TObject);
    procedure btB6Click(Sender: TObject);
    procedure btB7Click(Sender: TObject);
    procedure btB8Click(Sender: TObject);
    procedure btB9Click(Sender: TObject);
    procedure btL0Click(Sender: TObject);
    procedure btL1Click(Sender: TObject);
    procedure btL2Click(Sender: TObject);
    procedure btL3Click(Sender: TObject);
    procedure btL4Click(Sender: TObject);
    procedure btL5Click(Sender: TObject);
    procedure btL6Click(Sender: TObject);
    procedure btL7Click(Sender: TObject);
    procedure btL8Click(Sender: TObject);
    procedure btL9Click(Sender: TObject);
    procedure btR0Click(Sender: TObject);
    procedure btR1Click(Sender: TObject);
    procedure btR2Click(Sender: TObject);
    procedure btR3Click(Sender: TObject);
    procedure btR4Click(Sender: TObject);
    procedure btR5Click(Sender: TObject);
    procedure btR6Click(Sender: TObject);
    procedure btR7Click(Sender: TObject);
    procedure btR8Click(Sender: TObject);
    procedure btR9Click(Sender: TObject);
    procedure btT0Click(Sender: TObject);
    procedure btT1Click(Sender: TObject);
    procedure btT2Click(Sender: TObject);
    procedure btT3Click(Sender: TObject);
    procedure btT4Click(Sender: TObject);
    procedure btT5Click(Sender: TObject);
    procedure btT6Click(Sender: TObject);
    procedure btT7Click(Sender: TObject);
    procedure btT8Click(Sender: TObject);
    procedure btT9Click(Sender: TObject);
    procedure shField00MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField01MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField02MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField03MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField04MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField05MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField06MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField07MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField08MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField09MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField10MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField11MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField12MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField13MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField14MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField15MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField16MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField17MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField18MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField19MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField20MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField21MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField22MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField23MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField24MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField25MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField26MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField27MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField28MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField29MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField30MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField31MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField32MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField33MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField34MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField35MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField36MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField37MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField38MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField39MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField40MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField41MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField42MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField43MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField44MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField45MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField46MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField47MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField48MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField49MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField50MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField51MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField52MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField53MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField54MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField55MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField56MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField57MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField58MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField59MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField60MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField61MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField62MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField63MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField64MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField65MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField66MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField67MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField68MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField69MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField70MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField71MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField72MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField73MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField74MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField75MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField76MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField77MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField78MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField79MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField80MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField81MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField82MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField83MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField84MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField85MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField86MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField87MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField88MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField89MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField90MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField91MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField92MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField93MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField94MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField95MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField96MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField97MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField98MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shField99MouseDown(Sender: TObject; Button: TMouseButton);
  private
    iLevel, iMirrors, iMirror, iErrors, iError: Integer;
    bShown: Boolean;
    iColorLeft, iColorRight: TColor;
    aFields: TFields;
    shFields: TFieldShapes;
    btCanons: TCanonButtons;
  end;

const
  LevelMirrors: array[0..4] of Integer = (3, 6, 10, 15, 22);
  LevelErrors:  array[0..4] of Integer = (MaxInt, 2, 3, 5, 0);
  Levels: array[0..4] of string = ('Beginner', 'Easy', 'Intermediate', 'Difficult', 'Expert');
  Colors: array[0..4] of TColor = (clGreen, clBlue, clYellow, clLime, clAqua);

var
  fMirrors: TfMirrors;

implementation

{$R *.lfm}

{ Get number of mirrors for current level }

function MirrorNumber(Level: Integer): Integer;

begin
  MirrorNumber := LevelMirrors[Level];
end;

{ Get number of allowed mistakes for current level }

function ErrorNumber(Level: Integer): Integer;

var
  E: Integer;

begin
  if Level = 0 then                                                            // unlimited mistakes allowed for "beginner"
    E := MaxInt
  else if Level = 4 then                                                       // no mistake allowed for "expert"
    E := 0
  else begin                                                                   // number of mistakes allowed depending on level or ignored
    if fMirrors.mOptionsErrors.Checked then
      E := MaxInt
    else
      E := LevelErrors[Level];
  end;
  ErrorNumber := E;
end;

{ Clear form fields }

procedure ClearFields;

var
  I, J: Integer;

begin
  // Reset default color of board fields
  for J := 0 to 9 do begin
    for I := 0 to 9 do begin
      fMirrors.shFields[J, I].Brush.Color := clDefault;
    end;
  end;
  // Reset normal font style for shoot buttons
  for J := 0 to 3 do begin
    for I := 0 to 9 do begin
      fMirrors.btCanons[J, I].Font.Style := [];
    end;
  end;
  // Hide the "exiting the board" part of the light ray trajectory
  fMirrors.shShotH.Visible := False;
  fMirrors.shShotV.Visible := False;
end;

{ Shooting the light ray (from a given light canon) }

procedure Shot(CanonPos: Char; FieldPos: Integer; var Fields: TFields; ColorLeft, ColorRight: TColor; Mirror, Err: Integer);

var
  X, Y, DX, DY: Integer;

begin
  // Proceed only if the game isn't already over
  if (Mirror > 0) and (Err > -1) then begin
    ClearFields;                                                               // clear form fields
    // Determine coordinates of first field and field coordinates increments for actual shoot direction
    case CanonPos of
      'T': begin
             Y := 0; X := FieldPos;
             DX := 0; DY := 1;
             fMirrors.btCanons[0, FieldPos].Font.Style := [fsBold];
           end;
      'B': begin
             Y := 9; X := FieldPos;
             DX := 0; DY := -1;
             fMirrors.btCanons[1, FieldPos].Font.Style := [fsBold];
           end;
      'L': begin
             Y := FieldPos; X := 0;
             DX := 1; DY := 0;
             fMirrors.btCanons[2, FieldPos].Font.Style := [fsBold];
           end;
      'R': begin
             Y := FieldPos; X := 9;
             DX := -1; DY := 0;
             fMirrors.btCanons[3, FieldPos].Font.Style := [fsBold];
           end;
    end;
    // Let the light ray travel through the board until it falls outside of it
    repeat
      // Free field (no mirror)
      if (Fields[Y, X] = ' ') then begin
        if fMirrors.mOptionsShow.Checked then
          fMirrors.shFields[Y, X].Brush.Color := clRed;
      end
      // Mirror encountered in this field
      else begin
        // Left-refracting mirror: determine new coordinate increments after refraction to the left
        if UpperCase(Fields[Y, X]) = 'L' then begin
          if (DX = 1) and (DY = 0) then begin
            DX := 0; DY := -1;
          end
          else if (DX = -1) and (DY = 0) then begin
            DX := 0; DY := 1;
          end
          else if (DX = 0) and (DY = 1) then begin
            DX := 1; DY := 0;
          end
          else if (DX = 0) and (DY = -1) then begin
            DX := -1; DY := 0;
          end;
        end
        // Right-refracting mirror: determine new coordinate increments after refraction to the right
        else begin
          if (DX = 1) and (DY = 0) then begin
            DX := 0; DY := 1;
          end
          else if (DX = -1) and (DY = 0) then begin
            DX := 0; DY := -1;
          end
          else if (DX = 0) and (DY = 1) then begin
            DX := -1; DY := 0;
          end
          else if (DX = 0) and (DY = -1) then begin
            DX := 1; DY := 0;
          end;
        end;
      end;
      // Let travel the ray to the next field
      X += DX; Y += DY;
    until (Y < 0) or (Y > 9) or (X < 0) or (X > 9);
    // Display the "board exiting part" of the light ray (at correct position)
    X += DX; Y += DY;
    if DX = 1 then begin
      // Last move was rightwards
      fMirrors.shShotH.Left := 580;
      fMirrors.shShotH.Top := Y * 50 + 128;
    end
    else if DX = -1 then begin
      // Last move was leftwards
      fMirrors.shShotH.Left := 50;
      fMirrors.shShotH.Top := Y * 50 + 128;
    end
    else if DY = 1 then begin
      // Last move was bottomwards
      fMirrors.shShotV.Left := X * 50 + 80;
      fMirrors.shShotV.Top := 628;
    end
    else if DY = -1 then begin
      // Last move was topwards
      fMirrors.shShotV.Left := X * 50 + 80;;
      fMirrors.shShotV.Top := 98;
    end;
    if DX <> 0 then
      // Last move was horizontal
      fMirrors.shShotH.Visible := True
    else
      // Last move was vertical
      fMirrors.shShotV.Visible := True;
    // Display the mirrors that have to be shown, i.e. mirrors already found by player (uppercase
    // letters in array) or, if "Show light ray trajectory" is checked, all mirrors
    for Y := 0 to 9 do begin
      for X := 0 to 9 do begin
        if (Fields[Y, X] = 'L') or (fMirrors.mOptionsShow.Checked and (Fields[Y, X] = 'l')) then
          fMirrors.shFields[Y, X].Brush.Color := ColorLeft
        else if (Fields[Y, X] = 'R') or (fMirrors.mOptionsShow.Checked and (Fields[Y, X] = 'r')) then
          fMirrors.shFields[Y, X].Brush.Color := ColorRight;
      end;
    end;
  end;
end;

{ Field selection by left or right mouse button click }

procedure SelectField(Y, X: Integer; Button: TMouseButton; var Fields: TFields; ColorLeft, ColorRight: TColor; Mirror0, Err0: Integer; var Mirror, Err: Integer);

var
  S: string;
  IsErr: Boolean;

begin
  // There are still mirrors to find and there are still mistakes allowed
  if (Mirror > 0) and (Err > -1) then begin
    IsErr := False;
    // Check fields only if there isn't a mirror that the player already has found (uppercase letter in array)
    if Fields[Y, X] in [' ', 'l', 'r'] then begin
      // If the field is empty, clicking is a mistake
      if Fields[Y, X] = ' ' then
        IsErr := True
      else if Fields[Y, X] = 'l' then begin
        // If the field contains a left-refracting mirror, left-clicking the mouse = mirror found, right-clicking = mistake
        if Button = mbLeft then begin
          Fields[Y, X] := 'L';
          fMirrors.shFields[Y, X].Brush.Color := ColorLeft;
          Dec(Mirror);
        end
        else
          IsErr := True;
      end
      else begin
        // If the field contains a left-refracting mirror, left-clicking the mouse = mirror found, left-clicking = mistake
        if Button = mbRight then begin
          Fields[Y, X] := 'R';
          fMirrors.shFields[Y, X].Brush.Color := ColorRight;
          Dec(Mirror);
        end
        else
          IsErr := True;
      end;
    end;
    // Display message if there was a mistake (that is actually allowed)
    if IsErr then begin
      Dec(Err);
      if Err > -1 then begin
        if Button = mbLeft then
          S := 'left-refracting mirror'
        else
          S := 'right-refracting mirror';
        MessageDlg('Bad choice', 'This field does not contain a ' + S + '!', mtInformation, [mbOK], 0);
      end;
    end;
    // Display mirrors and allowed mistakes left after this field selection
    fMirrors.edMirrors.Text := IntToStr(Mirror) + '/' + IntToStr(Mirror0);
    if Err0 = MaxInt then
      fMirrors.edErrors.Text := '---'
    else if Err >= 0 then begin
      fMirrors.edErrors.Text := IntToStr(Err) + '/' + IntToStr(Err0);
      if Err = 0 then
        fMirrors.edErrors.Color := clYellow;
    end
  end;
  // All mirrors found
  if Mirror = 0 then begin
    if fMirrors.bShown then                                                    // bsShown is True, if the mirrors have been shown any time during the actual game
      S := 'Ok. Perhaps now trying without showing the mirrors?'
    else begin
      if fMirrors.mLevel4.Checked then
        S := 'WOW! You really did it! A real "Mirrors" expert!'
      else if fMirrors.mLevel3.Checked then
        S := 'Great! You did it! Perhaps wanna try the "Expert" level?'
      else
        S := 'You did it! Perhaps wanna try a higher level?';
    end;
    MessageDlg('Game over', S, mtInformation, [mbOK], 0);
    fMirrors.edMirrors.Text := '0';
    fMirrors.edMirrors.Color := clLime;
    Mirror := -1;                                                              // just to be sure to avoid repetitive messages
  end
  // Number of mistakes bigger than allowed
  else if Err = -1 then begin
    MessageDlg('Game over', 'Sorry, but you made more mistakes than allowed!', mtError, [mbOK], 0);
    fMirrors.edErrors.Text := 'XXXX'; fMirrors.edErrors.Color := clRed;
    Err := -2;                                                                 // just to be sure to avoid repetitive messages
  end;
end;

{***********}
{ TfMirrors }
{***********}

{ Application start: Initialisations }

procedure TfMirrors.FormCreate(Sender: TObject);

begin
  // Create array with board fields as elements
  shFields[0, 0] := shField00; shFields[0, 1] := shField01; shFields[0, 2] := shField02; shFields[0, 3] := shField03; shFields[0, 4] := shField04;
  shFields[0, 5] := shField05; shFields[0, 6] := shField06; shFields[0, 7] := shField07; shFields[0, 8] := shField08; shFields[0, 9] := shField09;
  shFields[1, 0] := shField10; shFields[1, 1] := shField11; shFields[1, 2] := shField12; shFields[1, 3] := shField13; shFields[1, 4] := shField14;
  shFields[1, 5] := shField15; shFields[1, 6] := shField16; shFields[1, 7] := shField17; shFields[1, 8] := shField18; shFields[1, 9] := shField19;
  shFields[2, 0] := shField20; shFields[2, 1] := shField21; shFields[2, 2] := shField22; shFields[2, 3] := shField23; shFields[2, 4] := shField24;
  shFields[2, 5] := shField25; shFields[2, 6] := shField26; shFields[2, 7] := shField27; shFields[2, 8] := shField28; shFields[2, 9] := shField29;
  shFields[3, 0] := shField30; shFields[3, 1] := shField31; shFields[3, 2] := shField32; shFields[3, 3] := shField33; shFields[3, 4] := shField34;
  shFields[3, 5] := shField35; shFields[3, 6] := shField36; shFields[3, 7] := shField37; shFields[3, 8] := shField38; shFields[3, 9] := shField39;
  shFields[4, 0] := shField40; shFields[4, 1] := shField41; shFields[4, 2] := shField42; shFields[4, 3] := shField43; shFields[4, 4] := shField44;
  shFields[4, 5] := shField45; shFields[4, 6] := shField46; shFields[4, 7] := shField47; shFields[4, 8] := shField48; shFields[4, 9] := shField49;
  shFields[5, 0] := shField50; shFields[5, 1] := shField51; shFields[5, 2] := shField52; shFields[5, 3] := shField53; shFields[5, 4] := shField54;
  shFields[5, 5] := shField55; shFields[5, 6] := shField56; shFields[5, 7] := shField57; shFields[5, 8] := shField58; shFields[5, 9] := shField59;
  shFields[6, 0] := shField60; shFields[6, 1] := shField61; shFields[6, 2] := shField62; shFields[6, 3] := shField63; shFields[6, 4] := shField64;
  shFields[6, 5] := shField65; shFields[6, 6] := shField66; shFields[6, 7] := shField67; shFields[6, 8] := shField68; shFields[6, 9] := shField69;
  shFields[7, 0] := shField70; shFields[7, 1] := shField71; shFields[7, 2] := shField72; shFields[7, 3] := shField73; shFields[7, 4] := shField74;
  shFields[7, 5] := shField75; shFields[7, 6] := shField76; shFields[7, 7] := shField77; shFields[7, 8] := shField78; shFields[7, 9] := shField79;
  shFields[8, 0] := shField80; shFields[8, 1] := shField81; shFields[8, 2] := shField82; shFields[8, 3] := shField83; shFields[8, 4] := shField84;
  shFields[8, 5] := shField85; shFields[8, 6] := shField86; shFields[8, 7] := shField87; shFields[8, 8] := shField88; shFields[8, 9] := shField89;
  shFields[9, 0] := shField90; shFields[9, 1] := shField91; shFields[9, 2] := shField92; shFields[9, 3] := shField93; shFields[9, 4] := shField94;
  shFields[9, 5] := shField95; shFields[9, 6] := shField96; shFields[9, 7] := shField97; shFields[9, 8] := shField98; shFields[9, 9] := shField99;
  // Create array with shoot buttons as elements
  btCanons[0, 0] := btT0; btCanons[0, 1] := btT1; btCanons[0, 2] := btT2; btCanons[0, 3] := btT3; btCanons[0, 4] := btT4;
  btCanons[0, 5] := btT5; btCanons[0, 6] := btT6; btCanons[0, 7] := btT7; btCanons[0, 8] := btT8; btCanons[0, 9] := btT9;
  btCanons[1, 0] := btB0; btCanons[1, 1] := btB1; btCanons[1, 2] := btB2; btCanons[1, 3] := btB3; btCanons[1, 4] := btB4;
  btCanons[1, 5] := btB5; btCanons[1, 6] := btB6; btCanons[1, 7] := btB7; btCanons[1, 8] := btB8; btCanons[1, 9] := btB9;
  btCanons[2, 0] := btL0; btCanons[2, 1] := btL1; btCanons[2, 2] := btL2; btCanons[2, 3] := btL3; btCanons[2, 4] := btL4;
  btCanons[2, 5] := btL5; btCanons[2, 6] := btL6; btCanons[2, 7] := btL7; btCanons[2, 8] := btL8; btCanons[2, 9] := btL9;
  btCanons[3, 0] := btR0; btCanons[3, 1] := btR1; btCanons[3, 2] := btR2; btCanons[3, 3] := btR3; btCanons[3, 4] := btR4;
  btCanons[3, 5] := btR5; btCanons[3, 6] := btR6; btCanons[3, 7] := btR7; btCanons[3, 8] := btR8; btCanons[3, 9] := btR9;
  // Set default values
  iLevel := 2; iMirrors := MirrorNumber(iLevel);
  iColorLeft := Colors[0]; iColorRight := Colors[1];
  // Start random number generator
  Randomize;
  // Prepare for new game (by simulating a click on the "New" menu item)
  mGameNew.Click;
end;

{ Menu item "Game > New": Prepare for a new game (including random mirrors generation) }

procedure TfMirrors.mGameNewClick(Sender: TObject);

var
  I, J, K, M: Integer;

begin
  // Reset form fields and variables
  ClearFields;
  for J := 0 to 9 do begin
    for I := 0 to 9 do begin
      aFields[J, I] := ' ';
    end;
  end;
  iMirrors := MirrorNumber(iLevel); iMirror := iMirrors;
  iErrors := ErrorNumber(iLevel); iError := iErrors;
  bShown := False;
  edLevel.Text := Levels[iLevel];
  edMirrors.Text := IntToStr(iMirrors) + '/' + IntToStr(iMirrors);
  if iErrors = MaxInt then
    edErrors.Text := '---'
  else
    edErrors.Text := IntToStr(iErrors) + '/' + IntToStr(iErrors);
  edMirrors.Color := clDefault; edErrors.Color := clDefault;
  // Generate random type mirrors, placed at random positions
  for K := 1 to iMirrors do begin
    repeat
      J := Random(10); I := Random(10);
    until aFields[J, I] = ' ';
    M := Random(2);
    // Lowercase letters indicate mirrors not yet found by player
    if M = 0 then
      aFields[J, I] := 'l'
    else
      aFields[J, I] := 'r';
  end;
end;

{ Menu item "Game > Exit": Exit the application }

procedure TfMirrors.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Level > ...": Choose game difficulty }

procedure TfMirrors.mLevel0Click(Sender: TObject);

begin
  MLevel0.Checked := True;  MLevel1.Checked := False; MLevel2.Checked := False;
  MLevel3.Checked := False; MLevel4.Checked := False;
  iLevel := 0;
end;

procedure TfMirrors.mLevel1Click(Sender: TObject);

begin
  MLevel0.Checked := False; MLevel1.Checked := True;  MLevel2.Checked := False;
  MLevel3.Checked := False; MLevel4.Checked := False;
  iLevel := 1;
end;

procedure TfMirrors.mLevel2Click(Sender: TObject);

begin
  MLevel0.Checked := False; MLevel1.Checked := False; MLevel2.Checked := True;
  MLevel3.Checked := False; MLevel4.Checked := False;
  iLevel := 2;
end;

procedure TfMirrors.mLevel3Click(Sender: TObject);

begin
  MLevel0.Checked := False; MLevel1.Checked := False; MLevel2.Checked := False;
  MLevel3.Checked := True;  MLevel4.Checked := False;
  iLevel := 3;
end;

procedure TfMirrors.mLevel4Click(Sender: TObject);

begin
  MLevel0.Checked := False; MLevel1.Checked := False; MLevel2.Checked := False;
  MLevel3.Checked := False; MLevel4.Checked := True;
  iLevel := 4;
end;

{ Menu item "Settings > Left mirrors color > ...": Choose field color for left-refracting mirrors }

procedure TfMirrors.mOptionsMLeft0Click(Sender: TObject);

begin
  if not mOptionsMRight0.Checked then begin
    mOptionsMLeft0.Checked := True;  mOptionsMLeft1.Checked := False; mOptionsMLeft2.Checked := False;
    mOptionsMLeft3.Checked := False; mOptionsMLeft4.Checked := False;
    iColorLeft := Colors[0];
    shLegendLeft.Brush.Color := iColorLeft;
  end;
end;

procedure TfMirrors.mOptionsMLeft1Click(Sender: TObject);

begin
  if not mOptionsMRight1.Checked then begin
    mOptionsMLeft0.Checked := False; mOptionsMLeft1.Checked := True;  mOptionsMLeft2.Checked := False;
    mOptionsMLeft3.Checked := False; mOptionsMLeft4.Checked := False;
    iColorLeft := Colors[1];
    shLegendLeft.Brush.Color := iColorLeft;
  end;
end;

procedure TfMirrors.mOptionsMLeft2Click(Sender: TObject);

begin
  if not mOptionsMRight2.Checked then begin
    mOptionsMLeft0.Checked := False; mOptionsMLeft1.Checked := False; mOptionsMLeft2.Checked := True;
    mOptionsMLeft3.Checked := False; mOptionsMLeft4.Checked := False;
    iColorLeft := Colors[2];
    shLegendLeft.Brush.Color := iColorLeft;
  end;
end;

procedure TfMirrors.mOptionsMLeft3Click(Sender: TObject);

begin
  if not mOptionsMRight3.Checked then begin
    mOptionsMLeft0.Checked := False; mOptionsMLeft1.Checked := False; mOptionsMLeft2.Checked := False;
    mOptionsMLeft3.Checked := True;  mOptionsMLeft4.Checked := False;
    iColorLeft := Colors[3];
    shLegendLeft.Brush.Color := iColorLeft;
  end;
end;

procedure TfMirrors.mOptionsMLeft4Click(Sender: TObject);

begin
  if not mOptionsMRight4.Checked then begin
    mOptionsMLeft0.Checked := False; mOptionsMLeft1.Checked := False; mOptionsMLeft2.Checked := False;
    mOptionsMLeft3.Checked := False; mOptionsMLeft4.Checked := True;
    iColorLeft := Colors[4];
    shLegendLeft.Brush.Color := iColorLeft;
  end;
end;

{ Menu item "Settings > Right mirrors color > ...": Choose field color for right-refracting mirrors }

procedure TfMirrors.mOptionsMRight0Click(Sender: TObject);

begin
  if not mOptionsMLeft0.Checked then begin
    mOptionsMRight0.Checked := True;  mOptionsMRight1.Checked := False; mOptionsMRight2.Checked := False;
    mOptionsMRight3.Checked := False; mOptionsMRight4.Checked := False;
    iColorRight := Colors[0];
    shLegendRight.Brush.Color := iColorRight;
  end;
end;

procedure TfMirrors.mOptionsMRight1Click(Sender: TObject);

begin
  if not mOptionsMLeft1.Checked then begin
    mOptionsMRight0.Checked := False; mOptionsMRight1.Checked := True;  mOptionsMRight2.Checked := False;
    mOptionsMRight3.Checked := False; mOptionsMRight4.Checked := False;
    iColorRight := Colors[1];
    shLegendRight.Brush.Color := iColorRight;
  end;
end;

procedure TfMirrors.mOptionsMRight2Click(Sender: TObject);

begin
  if not mOptionsMLeft2.Checked then begin
    mOptionsMRight0.Checked := False; mOptionsMRight1.Checked := False; mOptionsMRight2.Checked := True;
    mOptionsMRight3.Checked := False; mOptionsMRight4.Checked := False;
    iColorRight := Colors[2];
    shLegendRight.Brush.Color := iColorRight;
  end;
end;

procedure TfMirrors.mOptionsMRight3Click(Sender: TObject);

begin
  if not mOptionsMLeft3.Checked then begin
    mOptionsMRight0.Checked := False; mOptionsMRight1.Checked := False; mOptionsMRight2.Checked := False;
    mOptionsMRight3.Checked := True;  mOptionsMRight4.Checked := False;
    iColorRight := Colors[3];
    shLegendRight.Brush.Color := iColorRight;
  end;
end;

procedure TfMirrors.mOptionsMRight4Click(Sender: TObject);

begin
  if not mOptionsMLeft4.Checked then begin
    mOptionsMRight0.Checked := False; mOptionsMRight1.Checked := False; mOptionsMRight2.Checked := False;
    mOptionsMRight3.Checked := False; mOptionsMRight4.Checked := True;
    iColorRight := Colors[4];
    shLegendRight.Brush.Color := iColorRight;
  end;
end;

{ Menu item "Settings > Ignore error count": Ignore or not counting mistakes }

procedure TfMirrors.mOptionsErrorsClick(Sender: TObject);

begin
  if mOptionsErrors.Checked then
    mOptionsErrors.Checked := False
  else
    mOptionsErrors.Checked := True;
end;

{ Menu item "Settings > Show ray trajectory": Show or not the ray's trajectory and the mirrors }

procedure TfMirrors.mOptionsShowClick(Sender: TObject);

begin
  if mOptionsShow.Checked then
    mOptionsShow.Checked := False
  else begin
    mOptionsShow.Checked := True;
    bShown := True;                                                            // set to True if during actual game, this option was any time selected
  end;
end;

{ Menu item "Help > Help": Display program help text }

procedure TfMirrors.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Dispaly program about text }

procedure TfMirrors.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'Freely invented logic game, based on an idea I realized in Visual Basic in 2003.' + Chr(13) + Chr(13);
    S += '© allu, August, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Shoot buttons pushed: Shoot a light ray originating from this canon }

procedure TfMirrors.btT0Click(Sender: TObject);

begin
  Shot('T', 0, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL0Click(Sender: TObject);

begin
  Shot('L', 0, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB0Click(Sender: TObject);

begin
  Shot('B', 0, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB1Click(Sender: TObject);

begin
  Shot('B', 1, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB2Click(Sender: TObject);

begin
  Shot('B', 2, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB3Click(Sender: TObject);

begin
  Shot('B', 3, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB4Click(Sender: TObject);

begin
  Shot('B', 4, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB5Click(Sender: TObject);

begin
  Shot('B', 5, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB6Click(Sender: TObject);

begin
  Shot('B', 6, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB7Click(Sender: TObject);

begin
  Shot('B', 7, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB8Click(Sender: TObject);

begin
  Shot('B', 8, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btB9Click(Sender: TObject);

begin
  Shot('B', 9, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL1Click(Sender: TObject);

begin
  Shot('L', 1, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL2Click(Sender: TObject);

begin
  Shot('L', 2, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL3Click(Sender: TObject);

begin
  Shot('L', 3, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL4Click(Sender: TObject);

begin
  Shot('L', 4, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL5Click(Sender: TObject);

begin
  Shot('L', 5, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL6Click(Sender: TObject);

begin
  Shot('L', 6, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL7Click(Sender: TObject);

begin
  Shot('L', 7, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL8Click(Sender: TObject);

begin
  Shot('L', 8, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btL9Click(Sender: TObject);

begin
  Shot('L', 9, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR0Click(Sender: TObject);

begin
  Shot('R', 0, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR1Click(Sender: TObject);

begin
  Shot('R', 1, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR2Click(Sender: TObject);

begin
  Shot('R', 2, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR3Click(Sender: TObject);

begin
  Shot('R', 3, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR4Click(Sender: TObject);

begin
  Shot('R', 4, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR5Click(Sender: TObject);

begin
  Shot('R', 5, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR6Click(Sender: TObject);

begin
  Shot('R', 6, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR7Click(Sender: TObject);

begin
  Shot('R', 7, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR8Click(Sender: TObject);

begin
  Shot('R', 8, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btR9Click(Sender: TObject);

begin
  Shot('R', 9, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT1Click(Sender: TObject);

begin
  Shot('T', 1, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT2Click(Sender: TObject);

begin
  Shot('T', 2, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT3Click(Sender: TObject);

begin
  Shot('T', 3, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT4Click(Sender: TObject);

begin
  Shot('T', 4, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT5Click(Sender: TObject);

begin
  Shot('T', 5, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT6Click(Sender: TObject);

begin
  Shot('T', 6, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT7Click(Sender: TObject);

begin
  Shot('T', 7, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT8Click(Sender: TObject);

begin
  Shot('T', 8, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

procedure TfMirrors.btT9Click(Sender: TObject);

begin
  Shot('T', 9, aFields, iColorLeft, iColorRight, iMirror, iError);
end;

{ Board fields selection by mouse-click: Check if there is a mirror }

procedure TfMirrors.shField00MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField01MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField02MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField03MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField04MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField05MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField06MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField07MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField08MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField09MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(0, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField10MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField11MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField12MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField13MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField14MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField15MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField16MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField17MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField18MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField19MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(1, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField20MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField21MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField22MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField23MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField24MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField25MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField26MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField27MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField28MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField29MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(2, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField30MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField31MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField32MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField33MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField34MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField35MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField36MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField37MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField38MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField39MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(3, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField40MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField41MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField42MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField43MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField44MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField45MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField46MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField47MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField48MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField49MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(4, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField50MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField51MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField52MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField53MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField54MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField55MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField56MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField57MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField58MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField59MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(5, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField60MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField61MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField62MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField63MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField64MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField65MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField66MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField67MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField68MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField69MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(6, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField70MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField71MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField72MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField73MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField74MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField75MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField76MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField77MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField78MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField79MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(7, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField80MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField81MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField82MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField83MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField84MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField85MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField86MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField87MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField88MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField89MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(8, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField90MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 0, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField91MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 1, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField92MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 2, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField93MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 3, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField94MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 4, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField95MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 5, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField96MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 6, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField97MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 7, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField98MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 8, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

procedure TfMirrors.shField99MouseDown(Sender: TObject; Button: TMouseButton);

begin
  SelectField(9, 9, Button, aFields, iColorLeft, iColorRight, iMirrors, iErrors, iMirror, iError);
end;

end.

