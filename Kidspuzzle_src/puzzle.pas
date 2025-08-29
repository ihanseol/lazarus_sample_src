{****************************************}
{* Main unit for Kidspuzzle application *}
{****************************************}

unit puzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, IniFiles, LCLIntf, Dos;

const
  Version = '4.0';
  MaxHeight = 600; MaxWidth = 1200;
  MaxRows = 8; MaxCols = 15;

type
  TPuzzle = record
    // TPuzzle elements (= puzzle description)
    //   PVersion = application version number; PUpdate = puzzle picture update number
    //   PName = puzzle name; PDir = puzzle directory
    //   PFile = base of puzzle file names; PExt = extension of picture files
    //   DW, DH = image width and height to be used for puzzle picture display
    //     (in order to be reasonable big and fit in window)
    //   PY, PX = number of pieces in a row resp. a column
    //   PW, PH = width and hight of pieces
    //   SelColor = color of selection shape
    //     (in order to be easily visible, depending on background = puzzle picture colors)
    PVersion, PUpdate, PName, PDir, PFile, PExt: string;
    DW, DH, PY, PX, PW, PH: Cardinal;
    SelColor: TColor;
  end;
  TPieceImages = array[1..MaxRows, 1..MaxCols] of TImage;                      // puzzle piece images
  TPieceFileId = array[1..MaxRows, 1..MaxCols] of string[4];                   // puzzle piece file identifiers (for restore and puzzle-done check)
  TPuzzleState = record
    PuzzleDirectory: string[255];
    PieceFileIds: TPieceFileId;
  end;
  {**********}
  { TfPuzzle }
  {**********}
  TfPuzzle = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleNew, mPuzzleOpen, mPuzzleSave, mPuzzleExit: TMenuItem;
    mUpdate, mUpdateDownload: TMenuItem;
    mOptions, mOptionsOverride, mOptionsDelete, mOptionsAskForSave: TMenuItem;
    mOptionsOverrideAsk, mOptionsOverrideAuto, mOptionsOverrideNo: TMenuItem;
    mOptionsDeleteAsk, mOptionsDeleteAuto, mOptionsDeleteNo: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stPuzzle: TStaticText;
    imP0000, imTemp: TImage;
    imP0101, imP0102, imP0103, imP0104, imP0105, imP0106, imP0107, imP0108, imP0109, imP0110: TImage;
    imP0201, imP0202, imP0203, imP0204, imP0205, imP0206, imP0207, imP0208, imP0209, imP0210: TImage;
    imP0301, imP0302, imP0303, imP0304, imP0305, imP0306, imP0307, imP0308, imP0309, imP0310: TImage;
    imP0401, imP0402, imP0403, imP0404, imP0405, imP0406, imP0407, imP0408, imP0409, imP0410: TImage;
    imP0501, imP0502, imP0503, imP0504, imP0505, imP0506, imP0507, imP0508, imP0509, imP0510: TImage;
    imP0601, imP0602, imP0603, imP0604, imP0605, imP0606, imP0607, imP0608, imP0609, imP0610: TImage;
    imP0111, imP0112, imP0211, imP0212, imP0311, imP0312, imP0411, imP0412, imP0511, imP0512: TImage;
    imP0611, imP0612, imP0113, imP0114, imP0115, imP0213, imP0214, imP0215, imP0313, imP0314: TImage;
    imP0315, imP0413, imP0414, imP0415, imP0513, imP0514, imP0515, imP0613, imP0614, imP0615: TImage;
    imP0701, imP0702, imP0703, imP0704, imP0705, imP0706, imP0707, imP0708, imP0709, imP0710: TImage;
    imP0711, imP0712, imP0713, imP0714, imP0715, imP0801, imP0802, imP0803, imP0804, imP0805: TImage;
    imP0806, imP0807, imP0808, imP0809, imP0810, imP0811, imP0812, imP0813, imP0814, imP0815: TImage;
    edHelp: TMemo;
    shSelect: TShape;
    btAction: TButton;
    dgPuzzleSelect: TSelectDirectoryDialog;
    dlgPuzzleOpen: TOpenDialog;
    dlgPuzzleSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleNewClick(Sender: TObject);
    procedure mPuzzleOpenClick(Sender: TObject);
    procedure mPuzzleSaveClick(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mUpdateDownloadClick(Sender: TObject);
    procedure mOptionsOverrideAskClick(Sender: TObject);
    procedure mOptionsOverrideAutoClick(Sender: TObject);
    procedure mOptionsOverrideNoClick(Sender: TObject);
    procedure mOptionsDeleteAskClick(Sender: TObject);
    procedure mOptionsDeleteAutoClick(Sender: TObject);
    procedure mOptionsDeleteNoClick(Sender: TObject);
    procedure mOptionsAskForSaveClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure imP0101Click(Sender: TObject);
    procedure imP0102Click(Sender: TObject);
    procedure imP0103Click(Sender: TObject);
    procedure imP0104Click(Sender: TObject);
    procedure imP0105Click(Sender: TObject);
    procedure imP0106Click(Sender: TObject);
    procedure imP0107Click(Sender: TObject);
    procedure imP0108Click(Sender: TObject);
    procedure imP0109Click(Sender: TObject);
    procedure imP0110Click(Sender: TObject);
    procedure imP0201Click(Sender: TObject);
    procedure imP0202Click(Sender: TObject);
    procedure imP0203Click(Sender: TObject);
    procedure imP0204Click(Sender: TObject);
    procedure imP0205Click(Sender: TObject);
    procedure imP0206Click(Sender: TObject);
    procedure imP0207Click(Sender: TObject);
    procedure imP0208Click(Sender: TObject);
    procedure imP0209Click(Sender: TObject);
    procedure imP0210Click(Sender: TObject);
    procedure imP0301Click(Sender: TObject);
    procedure imP0302Click(Sender: TObject);
    procedure imP0303Click(Sender: TObject);
    procedure imP0304Click(Sender: TObject);
    procedure imP0305Click(Sender: TObject);
    procedure imP0306Click(Sender: TObject);
    procedure imP0307Click(Sender: TObject);
    procedure imP0308Click(Sender: TObject);
    procedure imP0309Click(Sender: TObject);
    procedure imP0310Click(Sender: TObject);
    procedure imP0401Click(Sender: TObject);
    procedure imP0402Click(Sender: TObject);
    procedure imP0403Click(Sender: TObject);
    procedure imP0404Click(Sender: TObject);
    procedure imP0405Click(Sender: TObject);
    procedure imP0406Click(Sender: TObject);
    procedure imP0407Click(Sender: TObject);
    procedure imP0408Click(Sender: TObject);
    procedure imP0409Click(Sender: TObject);
    procedure imP0410Click(Sender: TObject);
    procedure imP0501Click(Sender: TObject);
    procedure imP0502Click(Sender: TObject);
    procedure imP0503Click(Sender: TObject);
    procedure imP0504Click(Sender: TObject);
    procedure imP0505Click(Sender: TObject);
    procedure imP0506Click(Sender: TObject);
    procedure imP0507Click(Sender: TObject);
    procedure imP0508Click(Sender: TObject);
    procedure imP0509Click(Sender: TObject);
    procedure imP0510Click(Sender: TObject);
    procedure imP0601Click(Sender: TObject);
    procedure imP0602Click(Sender: TObject);
    procedure imP0603Click(Sender: TObject);
    procedure imP0604Click(Sender: TObject);
    procedure imP0605Click(Sender: TObject);
    procedure imP0606Click(Sender: TObject);
    procedure imP0607Click(Sender: TObject);
    procedure imP0608Click(Sender: TObject);
    procedure imP0609Click(Sender: TObject);
    procedure imP0610Click(Sender: TObject);
    procedure imP0111Click(Sender: TObject);
    procedure imP0112Click(Sender: TObject);
    procedure imP0211Click(Sender: TObject);
    procedure imP0212Click(Sender: TObject);
    procedure imP0311Click(Sender: TObject);
    procedure imP0312Click(Sender: TObject);
    procedure imP0411Click(Sender: TObject);
    procedure imP0412Click(Sender: TObject);
    procedure imP0511Click(Sender: TObject);
    procedure imP0512Click(Sender: TObject);
    procedure imP0611Click(Sender: TObject);
    procedure imP0612Click(Sender: TObject);
    procedure imP0113Click(Sender: TObject);
    procedure imP0114Click(Sender: TObject);
    procedure imP0115Click(Sender: TObject);
    procedure imP0213Click(Sender: TObject);
    procedure imP0214Click(Sender: TObject);
    procedure imP0215Click(Sender: TObject);
    procedure imP0313Click(Sender: TObject);
    procedure imP0314Click(Sender: TObject);
    procedure imP0315Click(Sender: TObject);
    procedure imP0413Click(Sender: TObject);
    procedure imP0414Click(Sender: TObject);
    procedure imP0415Click(Sender: TObject);
    procedure imP0513Click(Sender: TObject);
    procedure imP0514Click(Sender: TObject);
    procedure imP0515Click(Sender: TObject);
    procedure imP0613Click(Sender: TObject);
    procedure imP0614Click(Sender: TObject);
    procedure imP0615Click(Sender: TObject);
    procedure imP0701Click(Sender: TObject);
    procedure imP0702Click(Sender: TObject);
    procedure imP0703Click(Sender: TObject);
    procedure imP0704Click(Sender: TObject);
    procedure imP0705Click(Sender: TObject);
    procedure imP0706Click(Sender: TObject);
    procedure imP0707Click(Sender: TObject);
    procedure imP0708Click(Sender: TObject);
    procedure imP0709Click(Sender: TObject);
    procedure imP0710Click(Sender: TObject);
    procedure imP0711Click(Sender: TObject);
    procedure imP0712Click(Sender: TObject);
    procedure imP0713Click(Sender: TObject);
    procedure imP0714Click(Sender: TObject);
    procedure imP0715Click(Sender: TObject);
    procedure imP0801Click(Sender: TObject);
    procedure imP0802Click(Sender: TObject);
    procedure imP0803Click(Sender: TObject);
    procedure imP0804Click(Sender: TObject);
    procedure imP0805Click(Sender: TObject);
    procedure imP0806Click(Sender: TObject);
    procedure imP0807Click(Sender: TObject);
    procedure imP0808Click(Sender: TObject);
    procedure imP0809Click(Sender: TObject);
    procedure imP0810Click(Sender: TObject);
    procedure imP0811Click(Sender: TObject);
    procedure imP0812Click(Sender: TObject);
    procedure imP0813Click(Sender: TObject);
    procedure imP0814Click(Sender: TObject);
    procedure imP0815Click(Sender: TObject);
  private
    iFirstY, iFirstX: Cardinal;
    sPuzzleDir, sPuzzleStateDir: string;
    bSaveDone, bClose, bFirst: Boolean;
    rdPuzzle: TPuzzle;
    rdPuzzleState: TPuzzleState;
    aPuzzle: TPieceFileId;
    imPuzzle: TPieceImages;
  end;

var
  fPuzzle: TfPuzzle;
  PuzzleConfig: TINIFile;

implementation

{$R *.lfm}

{ Write program options to kidspuzzle.conf }

procedure WriteProgramOptions;

var
  I: Cardinal;
  Filename, Line: string;
  Check: Boolean;
  OutFile: Text;

begin
  Line := '';
  for I := 1 to 7 do begin
    case I of
      1: Check := fPuzzle.mOptionsOverrideAsk.Checked;
      2: Check := fPuzzle.mOptionsOverrideAuto.Checked;
      3: Check := fPuzzle.mOptionsOverrideNo.Checked;
      4: Check := fPuzzle.mOptionsDeleteAsk.Checked;
      5: Check := fPuzzle.mOptionsDeleteAuto.Checked;
      6: Check := fPuzzle.mOptionsDeleteNo.Checked;
      7: Check := fPuzzle.mOptionsAskForSave.Checked;
    end;
    if Check then
      Line += '1'
    else
      Line += '0';
  end;
  Filename := GetCurrentDir + '/kidspuzzle.conf'; DoDirSeparators(Filename);
  Assign(OutFile, Filename); Rewrite(OutFile);
  Writeln(OutFile, Line);
  Close(OutFile);
end;

{ Read program options from kidspuzzle.conf }

procedure ReadProgramOptions;

var
  I: Cardinal;
  Filename, Line: string;
  Check: Boolean;
  InFile: Text;

begin
  Filename := GetCurrentDir + '/kidspuzzle.conf'; DoDirSeparators(Filename);
  if not FileExists(Filename) then begin
    WriteProgramOptions;
  end
  else begin
    Assign(InFile, Filename); Reset(InFile);
    Readln(InFile, Line);
    Close(InFile);
    for I := 1 to 7 do begin
      if Line[I] = '0' then
        Check := False
      else
        Check := True;
      case I of
        1: fPuzzle.mOptionsOverrideAsk.Checked := Check;
        2: fPuzzle.mOptionsOverrideAuto.Checked := Check;
        3: fPuzzle.mOptionsOverrideNo.Checked := Check;
        4: fPuzzle.mOptionsDeleteAsk.Checked := Check;
        5: fPuzzle.mOptionsDeleteAuto.Checked := Check;
        6: fPuzzle.mOptionsDeleteNo.Checked := Check;
        7: fPuzzle.mOptionsAskForSave.Checked := Check;
      end;
    end;
  end;
end;

{ Read puzzle configuration data from file }

procedure ReadPuzzleData(Dir: string; out Puzzle: TPuzzle; out Mess: string);

var
  ProgramVersion, PuzzleVersion, P: Cardinal;
  FilePath, Filename: string;

begin
  Mess := '';
  Filename := Dir + '/puzzle.ini'; DoDirSeparators(Filename);
  // Find puzzle configuration file
  if not FileExists(Filename) then begin
    if RightStr(Dir, 17) = 'P016 - Merry Xmas' then
      Mess := 'Default puzzle configuration file not found!'
    else
      Mess := 'Puzzle configuration file not found!'
  end;
  if Mess = '' then begin
    // Create TINIFile object
    PuzzleConfig := TINIFile.Create(Filename);
    // Read configuration data for this puzzle from file puzzle.ini
    with Puzzle do begin
      PVersion := Trim(PuzzleConfig.ReadString('Puzzle', 'version', ''));
      PUpdate := Trim(PuzzleConfig.ReadString('Puzzle', 'update', ''));
      P := Pos('.', Version);
      ProgramVersion := StrToInt(LeftStr(Version, P - 1));
      P := Pos('.', PVersion);
      PuzzleVersion := StrToInt(LeftStr(PVersion, P - 1));
      if PuzzleVersion > ProgramVersion then
        // Puzzle main version must be less or equal than application main version
        Mess := 'Puzzle incompatible with actual application version'
      else begin
        // Puzzle directory should be as in puzzle download archive
        PDir := Trim(PuzzleConfig.ReadString('Puzzle', 'dir', ''));
        if PDir <> RightStr(Dir, Length(PDir)) then
          Mess := 'Invalid puzzle directory'
        else begin
          // Check if puzzle picture file actually exists in selected directory
          PDir := Dir;
          PFile := Trim(PuzzleConfig.ReadString('Puzzle', 'file_base', ''));
          PExt  := Trim(PuzzleConfig.ReadString('Puzzle', 'file_ext', 'jpg'));
          FilePath := Dir + '/' + PFile + '.' + PExt;
          if not FileExists(FilePath) then
            Mess := 'Puzzle files not found';
        end;
      end;
      // Read remaining puzzle configuration data
      PName := Trim(PuzzleConfig.ReadString('Puzzle', 'name', ''));
      if PName = '' then
        PName := Copy(PDir, 8, Length(PDir));
      DW := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'picture_display_width', '0')));
      DH := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'picture_display_height', '0')));
      PY := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'piece_rows', '0')));
      PX := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'piece_cols', '0')));
      if (PY > MaxRows) or (PX > MaxCols) then begin
        // Be sure number of rows and columns do not exceed the actual maximum
        Mess := 'Invalid configuration file: ';
        if PY > MaxRows then
          Mess += 'Invalid number of puzzle piece rows'
        else
          Mess += 'Invalid number of puzzle piece columns';
      end
      else begin
        PW := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'piece_width', '0')));
        PH := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'piece_height', '0')));
        if (PY * PH > MaxHeight) or (PX * PW > MaxWidth) then begin
          // Be sure width and height of puzzle do not exceed maximum size
          Mess := 'Invalid configuration file: ';
          if PY * PH > MaxHeight then
            Mess += 'Invalid puzzle height'
          else
            Mess += 'Invalid puzzle width';
        end
        else
          SelColor := StrToInt(Trim(PuzzleConfig.ReadString('Puzzle', 'selcolor', '$000000')));
      end;
    end;
    PuzzleConfig.Free;
  end;
  if Mess <> '' then
    MessageDlg('Program error', Mess, mtError, [mbOK], 0);
end;

{ Check if actual puzzle has completely be done }

function PuzzleDone(Puzzle: TPuzzle; var FileId: TPieceFileId): Boolean;

var
  I, J: Cardinal;
  S: string;
  Done: Boolean;

begin
  Done := True;
  for I := 1 to Puzzle.PY do begin
    for J:= 1 to Puzzle.PX do begin
      // For each piece, check if the corr. element of the fileid array is equal to "column plus row" string
      S := IntToStr(J);
      if J < 10 then
        S := '0' + S;
      S := '0' + IntToStr(I) + S;
      if FileId[I, J] <> S then
        // If there is one inequality, the puzzle isn't yet completed
        Done := False;
    end;
  end;
  Result := Done;
end;

{ Display pieces of completed puzzle (= initialisation, if a new puzzle is started) }

procedure PuzzleDisplay(Puzzle: TPuzzle; var PuzzlePics: TPieceImages; out FileId: TPieceFileId);

var
  I, J: Cardinal;
  SYX, FileName: string;

begin
  // Puzzle title (dynamic length to center it above the actual puzzle)
  fPuzzle.stPuzzle.Caption := 'Puzzle: ' + Puzzle.PName + ' (' + IntToStr(Puzzle.PY * Puzzle.PX) + ' pieces).';
  fPuzzle.stPuzzle.Width := Puzzle.PX * Puzzle.PW;
  // Puzzle picture (pic at the right; dynamic size and position)
  fPuzzle.imP0000.Width := Puzzle.DW; fPuzzle.imP0000.Height := Puzzle.DH;
  fPuzzle.imP0000.Left := fPuzzle.Width - (fPuzzle.imP0000.Width + 20);
  FileName := Puzzle.PDir + '/' + Puzzle.PFile + '.' + Puzzle.PExt; DoDirSeparators(FileName);
  fPuzzle.imP0000.Picture.LoadFromFile(FileName);
  // Hide all for now (and then show pieces, depending on actual number of puzzle col/row pieces)
  for I := 1 to MaxRows do begin
    for J := 1 to MaxCols do begin
      PuzzlePics[I, J].Visible := False;
    end;
  end;
  // Puzzle pieces (in correct order = completed puzzle)
  for I := 1 to Puzzle.PY do begin
    for J := 1 to Puzzle.PX do begin
      // Adapt piece image: Width, height, left and top position
      PuzzlePics[I, J].Width := Puzzle.PW - 2;
      PuzzlePics[I, J].Height := Puzzle.PH - 2;
      PuzzlePics[I, J].Top := 64 + (I - 1) * Puzzle.PH;
      PuzzlePics[I, J].Left := 16 + (J - 1) * Puzzle.PW;
      // Piece filename suffix (= string representation of column plus row)
      SYX := IntToStr(J);
      if J < 10 then
        SYX := '0' + SYX;
      SYX := IntToStr(I) + SYX;
      if I < 10 then
        SYX := '0' + SYX;
      // Fill filename suffix in corresponding array (simple way to know what image is at which puzzle position)
      FileId[I, J] := SYX;
      // Display the puzzle piece
      FileName := Puzzle.PDir + '/' + Puzzle.PFile + SYX + '.' + Puzzle.PExt; DoDirSeparators(FileName);
      PuzzlePics[I, J].Picture.LoadFromFile(FileName);
      // Make the puzzle piece visible
      PuzzlePics[I, J].Visible := True;
    end;
  end;
  // Reset values of some controls
  fPuzzle.shSelect.Visible := False;
  fPuzzle.btAction.Caption := 'Start'; fPuzzle.btAction.Enabled := True;
  fPuzzle.edHelp.Lines.Clear;
  fPuzzle.edHelp.Lines.AddText('Push the START button to begin...');
end;

{ Shuffle puzzle piece pictures (= creation of the puzzle to be solved) }

procedure PuzzleShuffle(Puzzle: TPuzzle; var PuzzlePics: TPieceImages; var FileId: TPieceFileId);

var
  IX, IY, JX, JY, K: Cardinal;
  S: string;

begin
  // Shuffle a reasonable number of times (depending on number of actual puzzle's pieces )
  for K := 1 to 10 * Puzzle.PY * Puzzle.PX do begin
    // Randomly exchange 2 pieces (and update fileid array accordingly)
    IY := Random(Puzzle.PY) + 1; IX := Random(Puzzle.PX) + 1;
    JY := Random(Puzzle.PY) + 1; JX := Random(Puzzle.PX) + 1;
    fPuzzle.imTemp.Picture := PuzzlePics[IY, IX].Picture; S := FileId[IY, IX];
    PuzzlePics[IY, IX].Picture := PuzzlePics[JY, JX].Picture; FileId[IY, IX] := FileId[JY, JX];
    PuzzlePics[JY, JX].Picture := fPuzzle.imTemp.Picture; FileId[JY, JX] := S;
  end;
end;

{ Perform selection/unselection/piece exchange, when user clicked on a given puzzle piece }

procedure PieceClick(Puzzle: TPuzzle; var PuzzlePics: TPieceImages; var FileId: TPieceFileId; Y, X: Cardinal;
  var First: Boolean; var FirstY, FirstX: Cardinal; var SaveDone: Boolean);

var
  S: string;

begin
  if fPuzzle.btAction.Enabled and (fPuzzle.btAction.Caption = 'Show') then begin
    // Proceed only, if a puzzle game is actually running
    if First then begin
      // First click of this "click-pair": Selection of the puzzle piece to be moved
      FirstY := Y; FirstX := X;                                                // save coordinates of piece clicked
      fPuzzle.shSelect.Top := 64 + 25 + (Y - 1) * Puzzle.PH;                   // display the selection shape
      fPuzzle.shSelect.Left := 16 + 25 + (X - 1) * Puzzle.PW;
      fPuzzle.shSelect.Brush.Color := Puzzle.SelColor;
      fPuzzle.shSelect.Visible := True;
      First := False;                                                          // next click will be the second of this "click-pair"
    end
    else begin
      // Second click of this "click-pair"
      if (Y <> FirstY) or (X <> FirstX) then begin
        // Exchange the 2 pieces (except when click was on same piece as before)
        fPuzzle.imTemp.Picture := PuzzlePics[Y, X].Picture; S := FileId[Y, X];
        PuzzlePics[Y, X].Picture := PuzzlePics[FirstY, FirstX].Picture; FileId[Y, X] := FileId[FirstY, FirstX];
        PuzzlePics[FirstY, FirstX].Picture := fPuzzle.imTemp.Picture; FileId[FirstY, FirstX] := S;
        SaveDone := False;                                                     // puzzle has been changed, so have to save on exit
      end;
      fPuzzle.shSelect.Visible := False;                                       // hide the piece selection shape
      First := True;                                                           // next click will be the first of a new "click-pair"
      // If the puzzle is completed, terminate the game (message + disable of button)
      if PuzzleDone(Puzzle, FileId) then begin
        MessageDlg('Kids Puzzle', 'Puzzle "' + Puzzle.PName + '" successfully completed!', mtInformation, [mbOK], 0);
        fPuzzle.btAction.Enabled := False;
      end;
    end;
  end;
end;

{ Read puzzle state information from saved puzzle state file }

procedure ReadPuzzleStateFile(Filename: string; out Puzzle: TPuzzleState);

var
  PuzzleStateFile: file of TPuzzleState;

begin
  Assign(PuzzleStateFile, Filename); Reset(PuzzleStateFile);
  Read(PuzzleStateFile, Puzzle);
  Close(PuzzleStateFile);
end;

{ Write actual puzzle state information to puzzle state file }

procedure WritePuzzleStateFile(Filename: string; Puzzle: TPuzzleState);

var
  PuzzleStateFile: file of TPuzzleState;

begin
  Assign(PuzzleStateFile, Filename); Rewrite(PuzzleStateFile);
  Write(PuzzleStateFile, Puzzle);
  Close(PuzzleStateFile);
end;

{**********}
{ TfPuzzle }
{**********}

{ Application start: Initialisation }

procedure TfPuzzle.FormCreate(Sender: TObject);

var
  Dir, Mess: string;

begin
  // Create array with puzzle piece images
  imPuzzle[1, 1] := imP0101; imPuzzle[1, 2] := imP0102; imPuzzle[1, 3] := imP0103; imPuzzle[1, 4] := imP0104; imPuzzle[1, 5]  := imP0105;
  imPuzzle[1, 6] := imP0106; imPuzzle[1, 7] := imP0107; imPuzzle[1, 8] := imP0108; imPuzzle[1, 9] := imP0109; imPuzzle[1, 10] := imP0110;
  imPuzzle[1, 11] := imP0111; imPuzzle[1, 12] := imP0112; imPuzzle[1, 13] := imP0113; imPuzzle[1, 14] := imP0114; imPuzzle[1, 15] := imP0115;
  imPuzzle[2, 1] := imP0201; imPuzzle[2, 2] := imP0202; imPuzzle[2, 3] := imP0203; imPuzzle[2, 4] := imP0204; imPuzzle[2, 5]  := imP0205;
  imPuzzle[2, 6] := imP0206; imPuzzle[2, 7] := imP0207; imPuzzle[2, 8] := imP0208; imPuzzle[2, 9] := imP0209; imPuzzle[2, 10] := imP0210;
  imPuzzle[2, 11] := imP0211; imPuzzle[2, 12] := imP0212; imPuzzle[2, 13] := imP0213; imPuzzle[2, 14] := imP0214; imPuzzle[2, 15] := imP0215;
  imPuzzle[3, 1] := imP0301; imPuzzle[3, 2] := imP0302; imPuzzle[3, 3] := imP0303; imPuzzle[3, 4] := imP0304; imPuzzle[3, 5]  := imP0305;
  imPuzzle[3, 6] := imP0306; imPuzzle[3, 7] := imP0307; imPuzzle[3, 8] := imP0308; imPuzzle[3, 9] := imP0309; imPuzzle[3, 10] := imP0310;
  imPuzzle[3, 11] := imP0311; imPuzzle[3, 12] := imP0312; imPuzzle[3, 13] := imP0313; imPuzzle[3, 14] := imP0314; imPuzzle[3, 15] := imP0315;
  imPuzzle[4, 1] := imP0401; imPuzzle[4, 2] := imP0402; imPuzzle[4, 3] := imP0403; imPuzzle[4, 4] := imP0404; imPuzzle[4, 5]  := imP0405;
  imPuzzle[4, 6] := imP0406; imPuzzle[4, 7] := imP0407; imPuzzle[4, 8] := imP0408; imPuzzle[4, 9] := imP0409; imPuzzle[4, 10] := imP0410;
  imPuzzle[4, 11] := imP0411; imPuzzle[4, 12] := imP0412; imPuzzle[4, 13] := imP0413; imPuzzle[4, 14] := imP0414; imPuzzle[4, 15] := imP0415;
  imPuzzle[5, 1] := imP0501; imPuzzle[5, 2] := imP0502; imPuzzle[5, 3] := imP0503; imPuzzle[5, 4] := imP0504; imPuzzle[5, 5]  := imP0505;
  imPuzzle[5, 6] := imP0506; imPuzzle[5, 7] := imP0507; imPuzzle[5, 8] := imP0508; imPuzzle[5, 9] := imP0509; imPuzzle[5, 10] := imP0510;
  imPuzzle[5, 11] := imP0511; imPuzzle[5, 12] := imP0512; imPuzzle[5, 13] := imP0513; imPuzzle[5, 14] := imP0514; imPuzzle[5, 15] := imP0515;
  imPuzzle[6, 1] := imP0601; imPuzzle[6, 2] := imP0602; imPuzzle[6, 3] := imP0603; imPuzzle[6, 4] := imP0604; imPuzzle[6, 5]  := imP0605;
  imPuzzle[6, 6] := imP0606; imPuzzle[6, 7] := imP0607; imPuzzle[6, 8] := imP0608; imPuzzle[6, 9] := imP0609; imPuzzle[6, 10] := imP0610;
  imPuzzle[6, 11] := imP0611; imPuzzle[6, 12] := imP0612; imPuzzle[6, 13] := imP0613; imPuzzle[6, 14] := imP0614; imPuzzle[6, 15] := imP0615;
  imPuzzle[7, 1] := imP0701; imPuzzle[7, 2] := imP0702; imPuzzle[7, 3] := imP0703; imPuzzle[7, 4] := imP0704; imPuzzle[7, 5]  := imP0705;
  imPuzzle[7, 6] := imP0706; imPuzzle[7, 7] := imP0707; imPuzzle[7, 8] := imP0708; imPuzzle[7, 9] := imP0709; imPuzzle[7, 10] := imP0710;
  imPuzzle[7, 11] := imP0711; imPuzzle[7, 12] := imP0712; imPuzzle[7, 13] := imP0713; imPuzzle[7, 14] := imP0714; imPuzzle[7, 15] := imP0715;
  imPuzzle[8, 1] := imP0801; imPuzzle[8, 2] := imP0802; imPuzzle[8, 3] := imP0803; imPuzzle[8, 4] := imP0804; imPuzzle[8, 5]  := imP0805;
  imPuzzle[8, 6] := imP0806; imPuzzle[8, 7] := imP0807; imPuzzle[8, 8] := imP0808; imPuzzle[8, 9] := imP0809; imPuzzle[8, 10] := imP0810;
  imPuzzle[8, 11] := imP0811; imPuzzle[8, 12] := imP0812; imPuzzle[8, 13] := imP0813; imPuzzle[8, 14] := imP0814; imPuzzle[8, 15] := imP0815;
  // Start random number generator
  Randomize;
  // Set program options from file
  ReadProgramOptions;                                                          // read (last selected) program options and modify in "Options" menu
  // Start game with default puzzle ("Easter", included in application download archive) loaded
  sPuzzleDir := GetCurrentDir + '/puzzles'; DoDirSeparators(sPuzzleDir);
  sPuzzleStateDir := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\Documents';
  Dir := sPuzzleDir + '/P016 - Merry Xmas'; DoDirSeparators(Dir);
  ReadPuzzleData(Dir, rdPuzzle, Mess);
  if Mess = '' then
    PuzzleDisplay(rdPuzzle, imPuzzle, aPuzzle);
  bSaveDone := True;                                                           // default puzzle, nothing yet done: no reason to save on exit
end;

{ Menu item "Puzzle > New ...": Load a new puzzle }

procedure TfPuzzle.mPuzzleNewClick(Sender: TObject);

var
  Dir, Mess: string;

begin
  dgPuzzleSelect.InitialDir := sPuzzleDir;
  dgPuzzleSelect.FileName := '';
  if dgPuzzleSelect.Execute then begin
    // User has selected a directory
    Dir := dgPuzzleSelect.FileName;
    ReadPuzzleData(Dir, rdPuzzle, Mess);                                       // read puzzle configuration data from puzzle.ini
    if Mess = '' then begin
      PuzzleDisplay(rdPuzzle, imPuzzle, aPuzzle);                              // display puzzle pieces
      bSaveDone := True;                                                       // new puzzle, nothing yet done: no reason to save on exit
      sPuzzleDir := ExtractFileDir(Dir);                                       // save directory (to open next time at same location)
    end;
  end;
end;

{ Menu item "Puzzle > Open ...": Open a puzzle state file (= load puzzle at saved state from file) }

procedure TfPuzzle.mPuzzleOpenClick(Sender: TObject);

var
  Ret, I, J: Cardinal;
  Filename, Dir, Mess: string;
  DoDelete: Boolean;

begin
  dlgPuzzleOpen.InitialDir := sPuzzleStateDir;
  dlgPuzzleOpen.FileName := '';
  if dlgPuzzleOpen.Execute then begin
    // User has selected a file
    Filename := dlgPuzzleOpen.FileName;
    sPuzzleStateDir := ExtractFileDir(Filename);                               // save directory (to open next time at same location)
    ReadPuzzleStateFile(Filename, rdPuzzleState);                              // read puzzle state information
    Dir := rdPuzzleState.PuzzleDirectory;                                      // directory, where are located this puzzle's files
    ReadPuzzleData(Dir, rdPuzzle, Mess);                                       // read puzzle congiguration data from file puzzle.ini
    if Mess = '' then begin
      PuzzleDisplay(rdPuzzle, imPuzzle, aPuzzle);                              // display puzzle pieces
      // Get row-column position of puzzle pieces from puzzle state file data (= restore puzzle at state, as it was when it was saved)
      for I := 1 to rdPuzzle.PY do begin
        for J := 1 to rdPuzzle.PX do
          aPuzzle[I, J] := rdPuzzleState.PieceFileIds[I, J];
      end;
      // Display the puzzle piece images at these positions (by simulating a click on "Restore" button)
      btAction.Caption := 'Restore'; btAction.Enabled := True; btAction.Click;
      // Delete or not the puzzle state file
      if mOptionsDeleteNo.Checked then
        DoDelete := False
      else begin
        DoDelete := True;
        if mOptionsDeleteAsk.Checked then begin
          Ret := MessageDlg('Kidspuzzle', 'Puzzle state file no more needed. Do you want to delete it?', mtConfirmation, [mbYes, mbNo], 0, mbYes);
          if Ret = mrNo then
            DoDelete := False;
        end;
      end;
      if DoDelete then begin
        DeleteFile(Filename);
        bSaveDone := False;                                                    // puzzle already started with state not (anymore) saved
      end
      else
        bSaveDone := True;                                                     // puzzle already started with state (already) saved
    end;
  end;
end;

{ Menu item "Puzzle > Save ...": Save actual puzzle state to file }

procedure TfPuzzle.mPuzzleSaveClick(Sender: TObject);

var
  Ret, I, J: Cardinal;
  Filename: string;
  DoOverride: Boolean;

begin
  dlgPuzzleSave.InitialDir := sPuzzleStateDir;
  dlgPuzzleSave.FileName := '';
  if dlgPuzzleSave.Execute then begin
    // User has selected a file
    Filename := dlgPuzzleSave.FileName;
    sPuzzleStateDir := ExtractFileDir(Filename);                               // save directory (to open next time at same location)
    // Override existing file only, if user wants so
    DoOverride := True; bClose := True;
    if mOptionsOverrideAsk.Checked or mOptionsOverrideNo.Checked then begin
      if FileExists(Filename) then begin
        if mOptionsOverrideAsk.Checked then begin
          Ret := MessageDlg('Kidspuzzle', 'File already exists. Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
          if Ret = mrNo then begin
            DoOverride := False;
            bClose := False;                                                   // file has to be saved before application can be closed
          end;
        end
        else begin
          MessageDlg('Kidspuzzle', 'File already exists. Please, choose another filename!', mtError, [mbOK], 0);
          DoOverride := False;
          bClose := False;                                                     // file has to be saved before application can be closed
        end;
      end;
    end;
    // Write actual puzzle state information to file
    if DoOverride then begin
      rdPuzzleState.PuzzleDirectory := rdPuzzle.PDir;                          // directory, where actual puzzle's files are located
      // Fill puzzle state array with actual puzzle piece row-column positions
      for I := 1 to MaxRows do begin
        for J := 1 to MaxCols do begin
          if (I <= rdPuzzle.PY) and (J <= rdPuzzle.PX) then
            rdPuzzleState.PieceFileIds[I, J] := aPuzzle[I, J]
          else
            rdPuzzleState.PieceFileIds[I, J] := '0000';                        // just filling not used array elements
        end;
      end;
      WritePuzzleStateFile(Filename, rdPuzzleState);
      bSaveDone := True;                                                       // puzzle started with state now saved
    end;
  end;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfPuzzle.mPuzzleExitClick(Sender: TObject);

var
  Ret: Cardinal;

begin
  bClose := True;                                                              // will be set to False in mPuzzleSave.Click if file can't be overridden
  if mOptionsAskForSave.Checked and not PuzzleDone(rdPuzzle, aPuzzle) then begin
    if not bSaveDone then begin
      // Ask user, if she wants to save actual puzzle state
      Ret := MessageDlg('Kidspuzzle', 'Puzzle state not saved. Do you want to save it before exiting?', mtWarning, [mbYes, mbNo], 0, mbYes);
      if Ret = mrYes then begin
        mPuzzleSave.Click;
      end;
    end;
  end;
  if bClose then begin
    WriteProgramOptions;                                                       // Save program options (thus always start with last selected)
    Close;
  end;
end;

{ Menu item "Update > Puzzle download": Point webbrowser to puzzle download page }

procedure TfPuzzle.mUpdateDownloadClick(Sender: TObject);

var
  URL: string;

begin
  URL := 'https://www.streetinfo.lu/computing/lazarus/files/data/Kidspuzzle/';
  OpenURL(URL);
end;

{ Menu items "Options > State files override > ...": Select state file override policy (when saving puzzle state) }

procedure TfPuzzle.mOptionsOverrideAskClick(Sender: TObject);

begin
  mOptionsOverrideAsk.Checked := True; mOptionsOverrideAuto.Checked := False; mOptionsOverrideNo.Checked := False;
end;

procedure TfPuzzle.mOptionsOverrideAutoClick(Sender: TObject);

begin
  mOptionsOverrideAsk.Checked := False; mOptionsOverrideAuto.Checked := True; mOptionsOverrideNo.Checked := False;
end;

procedure TfPuzzle.mOptionsOverrideNoClick(Sender: TObject);

begin
  mOptionsOverrideAsk.Checked := False; mOptionsOverrideAuto.Checked := False; mOptionsOverrideNo.Checked := True;
end;

{ Menu items "Options > State files deletion > ...": Select state file deletion policy (when loading puzzle state) }

procedure TfPuzzle.mOptionsDeleteAskClick(Sender: TObject);

begin
  mOptionsDeleteAsk.Checked := True; mOptionsDeleteAuto.Checked := False; mOptionsDeleteNo.Checked := False;
end;

procedure TfPuzzle.mOptionsDeleteAutoClick(Sender: TObject);

begin
  mOptionsDeleteAsk.Checked := False; mOptionsDeleteAuto.Checked := True; mOptionsDeleteNo.Checked := False;
end;

procedure TfPuzzle.mOptionsDeleteNoClick(Sender: TObject);

begin
  mOptionsDeleteAsk.Checked := False; mOptionsDeleteAuto.Checked := False; mOptionsDeleteNo.Checked := True;
end;

{ Menu item "Options > Ask for state saving on exit": Toggle ask or not for state file saving on application exit }

procedure TfPuzzle.mOptionsAskForSaveClick(Sender: TObject);

begin
  if mOptionsAskForSave.Checked then
    mOptionsAskForSave.Checked := False
  else
    mOptionsAskForSave.Checked := True;
end;

{ Menu item "Help > Help": Display application help (by opening local HTML file in webbrowser) }

procedure TfPuzzle.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Help > About": Display application about }

procedure TfPuzzle.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Classic puzzle game for kids.' + LineEnding + LineEnding;
  S += 'Version 4.0, © allu, April 2020 - November 2021.';
  MessageDlg('About "Kidspuzzle"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Show/Restore": Start game, show completed puzzle resp. restore puzzle }

procedure TfPuzzle.btActionClick(Sender: TObject);

var
  I, J: Cardinal;
  SYX, FileName, S: string;

begin
  // Button "Start": Start a new game with puzzle actually selected
  if btAction.Caption = 'Start' then begin
    PuzzleShuffle(rdPuzzle, imPuzzle, aPuzzle);                                // mix the puzzle pieces (= create puzzle to be solved)
    bFirst := True;                                                            // click on puzzle piece will be first of "click-pair"
    shSelect.Visible := False;
  end
  // Button "Show": Show completed puzzle (for user conveniance and help especially for young kids)
  else if btAction.Caption = 'Show' then begin
    for I := 1 to rdPuzzle.PY do begin
      for J := 1 to rdPuzzle.PX do begin
        // Display piece at their correct position within the puzzle picture
        SYX := IntToStr(J);
        if J < 10 then
          SYX := '0' + SYX;
        SYX := IntToStr(I) + SYX;
        if I < 10 then
          SYX := '0' + SYX;
        FileName := rdPuzzle.PDir + '/' + rdPuzzle.PFile + SYX + '.' + rdPuzzle.PExt; DoDirSeparators(FileName);
        imPuzzle[I, J].Picture.LoadFromFile(FileName);
      end;
    end;
  end
  // Button "Restore": Restore the puzzle at (actual game) state (as saved in aPuzzle array)
  else begin
    for I := 1 to rdPuzzle.PY do begin
      for J := 1 to rdPuzzle.PX do begin
        // Get the piece at a given position from the piece file identifier array
        SYX := aPuzzle[I, J];
        FileName := rdPuzzle.PDir + '/' + rdPuzzle.PFile + SYX + '.' + rdPuzzle.PExt; DoDirSeparators(FileName);
        imPuzzle[I, J].Picture.LoadFromFile(FileName);
      end;
    end;
    // Click on puzzle piece will be first of "click-pair"
    bFirst := True;
    shSelect.Visible := False;
  end;
  // Display help text and set button caption for next push
  edHelp.Lines.Clear;
  if (btAction.Caption = 'Start') or (btAction.Caption = 'Restore') then begin
    S := 'Click a puzzle piece to select it for moving (to unselect it, click it again). ';
    S += 'Then click at the position, where you think, it fits correctly (the piece, that was there, ';
    S += 'being placed at its old position). You may view the completed puzzle with the SHOW button ';
    S += 'and restore it as it is now, later.';
    edHelp.Lines.AddText(S);
    btAction.Caption := 'Show';
  end
  else begin
    edHelp.Lines.AddText('Push RESTORE to restore the puzzle at where you left...');
    btAction.Caption := 'Restore';
  end;
end;

{ Mouse click on a puzzle piece: Do appropriate action (first click: selection; second click: pieces exchange) }

procedure TfPuzzle.imP0101Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0102Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0103Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0104Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0105Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0106Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0107Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0108Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0109Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0110Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0111Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0112Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0113Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0114Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0115Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 1, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0201Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0202Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0203Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0204Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0205Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0206Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0207Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0208Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0209Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0210Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0211Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0212Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0213Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0214Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0215Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 2, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0301Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0302Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0303Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0304Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0305Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0306Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0307Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0308Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0309Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0310Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0311Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0312Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0313Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0314Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0315Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 3, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0401Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0402Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0403Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0404Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0405Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0406Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0407Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0408Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0409Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0410Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0411Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0412Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0413Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0414Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0415Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 4, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0501Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0502Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0503Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0504Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0505Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0506Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0507Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0508Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0509Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0510Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0511Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0512Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0513Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0514Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0515Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 5, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0601Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0602Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0603Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0604Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0605Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0606Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0607Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0608Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0609Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0610Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0611Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0612Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0613Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0614Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0615Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 6, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0701Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0702Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0703Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0704Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0705Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0706Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0707Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0708Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0709Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0710Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0711Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0712Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0713Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0714Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0715Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 7, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0801Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 1, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0802Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 2, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0803Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 3, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0804Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 4, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0805Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 5, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0806Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 6, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0807Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 7, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0808Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 8, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0809Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 9, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0810Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 10, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0811Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 11, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0812Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 12, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0813Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 13, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0814Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 14, bFirst, iFirstY, iFirstX, bSaveDone);
end;

procedure TfPuzzle.imP0815Click(Sender: TObject);

begin
  PieceClick(rdPuzzle, imPuzzle, aPuzzle, 8, 15, bFirst, iFirstY, iFirstX, bSaveDone);
end;

end.

