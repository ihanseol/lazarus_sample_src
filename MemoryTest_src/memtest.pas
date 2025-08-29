{************************}
{* MemoryTest main unit *}
{************************}

unit memtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, PopupNotifier,
  newtest, displayone, displayall, help;

type
  TObjList = array[1..20] of Integer;
  TRandomObjects = array[1..50] of Integer;
  { TfMemTest }
  TfMemTest = class(TForm)
    mMemTest: TMainMenu;
    mTest: TMenuItem;
    mTestNew: TMenuItem;
    mTestExit: TMenuItem;
    mObjects: TMenuItem;
    mObjNumbers, mObjShapes2D, mObjShapes3D, mObjColors, mObjFruits, mObjObjects: TMenuItem;
    mPictures: TMenuItem;
    mPicSelection: TMenuItem;
    mPicSel2, mPicSel5, mPicSel8, mPicSel10: TMenuItem;
    mPicTime: TMenuItem;
    mPicTime1, mPicTime2, mPicTime5, mPicTime10: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    StaticText1, StaticText2: TStaticText;
    Label1, Label2, Label3: TLabel;
    imObject1,  imObject2,  imObject3,  imObject4,  imObject5: TImage;
    imObject6,  imObject7,  imObject8,  imObject9,  imObject10: TImage;
    imObject11, imObject12, imObject13, imObject14, imObject15: TImage;
    imObject16, imObject17, imObject18, imObject19, imObject20: TImage;
    edObjects: TEdit;
    edTime: TEdit;
    edDisplay: TEdit;
    btStart: TButton;
    tiMemTest: TTimer;
    Popup: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mObjNumbersClick(Sender: TObject);
    procedure mObjShapes2DClick(Sender: TObject);
    procedure mObjShapes3DClick(Sender: TObject);
    procedure mObjColorsClick(Sender: TObject);
    procedure mObjFruitsClick(Sender: TObject);
    procedure mObjObjectsClick(Sender: TObject);
    procedure mPicSel2Click(Sender: TObject);
    procedure mPicSel5Click(Sender: TObject);
    procedure mPicSel8Click(Sender: TObject);
    procedure mPicSel10Click(Sender: TObject);
    procedure mPicTime1Click(Sender: TObject);
    procedure mPicTime2Click(Sender: TObject);
    procedure mPicTime5Click(Sender: TObject);
    procedure mPicTime10Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure imObject1Click(Sender: TObject);
    procedure imObject2Click(Sender: TObject);
    procedure imObject3Click(Sender: TObject);
    procedure imObject4Click(Sender: TObject);
    procedure imObject5Click(Sender: TObject);
    procedure imObject6Click(Sender: TObject);
    procedure imObject7Click(Sender: TObject);
    procedure imObject8Click(Sender: TObject);
    procedure imObject9Click(Sender: TObject);
    procedure imObject10Click(Sender: TObject);
    procedure imObject11Click(Sender: TObject);
    procedure imObject12Click(Sender: TObject);
    procedure imObject13Click(Sender: TObject);
    procedure imObject14Click(Sender: TObject);
    procedure imObject15Click(Sender: TObject);
    procedure imObject16Click(Sender: TObject);
    procedure imObject17Click(Sender: TObject);
    procedure imObject18Click(Sender: TObject);
    procedure imObject19Click(Sender: TObject);
    procedure imObject20Click(Sender: TObject);
    procedure tiMemTestTimer(Sender: TObject);
  private
    iObjects, iObject, iTimer, iTimeLeft, iObjList, iCheckObject: Integer;
    sObjects, sDisplay: string;
    bShowPic: Boolean;
    aObjList: TObjList;
    aRandomObjects: TRandomObjects;
end;

var
  fMemTest: TfMemTest;
  ObjImg: array[1..20] of TImage;

implementation

{$R *.lfm}

{ Get objects of selected subset of actual objects set }

procedure GetObjects(Obj: string; var N: Integer; ReadFile: Boolean; var ObjList: TObjList);

var
  N0, M, I, J, R: Integer;
  Filename: string;
  OK: Boolean;

begin
  // "Real" total number of objects (considering special case "Colored objects")
  if Obj = 'Objects' then
      N0 := 20
    else
      N0 := 10;
  M := N0 div 10;
  // Read object images from file (if argument that tells to do so is set)
  if ReadFile then begin
    for I := 1 to N0 do begin
      Filename := 'objects/' + Obj + IntToStr(I) + '.jpg';
      DoDirSeparators(Filename);
      ObjImg[I].Picture.LoadFromFile(Filename);
    end;
  end;
  // Fill the objects list with random objects (of actual objects subset)
  ObjList[1] := Random(N0) + 1;
  for I := 2 to M * N do begin
    repeat
      R  := Random(N0) + 1;
      // All objects must be unique
      OK := True;
      for J := 1 to I - 1 do begin
        if ObjList[J] = R then
          OK := False;
      end;
    until OK;
    ObjList[I] := R;
  end;
  // Make randomly chosen subset objects visible
  for I := 1 to 20 do
    ObjImg[I].Visible := False;
  for I := 1 to M * N do
    ObjImg[ObjList[I]].Visible := True;
end;

{ Check if user chosen object identical to the one displayed at this position before }

procedure CheckObject(var OX, N: Integer; PX: Integer; var RObj: TRandomObjects);

begin
  // Proceed only if clicking the objects is enabled (i.e. there is effectively a check to do)
  if OX >= 0 then begin
    Inc(OX);
    // Proceed if this an object of the actual subset and if not yet all objects checked
    if ObjImg[PX].Enabled and (OX <= N) then begin
      // If user chosen object is the correct one
      if PX = RObj[OX] then begin
        // Display the object on the "Check" form
        fAll.ObjImg[OX].Picture := ObjImg[PX].Picture;
        fAll.ObjImg[OX].Visible := True;
        // If all objects have been checked
        if OX = N then begin
          MessageDlg('Memory test', 'Well done! Continuing with ' + IntToStr(N + 1) + ' objects', mtInformation, [mbOK], 0);
        end
      end
      // If user chosen object is not the correct one
      else begin
        MessageDlg('Memory test', 'Wrong object at position ' + IntToStr(OX) + '!', mtError, [mbOK], 0);
        // Check is termiinated; clicking objects will have no more effect
        OX := -1;
      end;
    end;
    // If all objects have been found or there was a user object error
    if (OX = N) or (OX = -1) then begin
      // close the "Check" form
      fAll.Close;
      if OX <> -1 then begin
        // All objects found; increment number of objects to be displayed (for next test)
        Inc(N);
        fMemTest.edObjects.Text := IntToStr(N);
        fNew.edObjects.Text := IntToStr(N);
      end;
      // Object clicking no effect; ready to start another test
      OX := -1;
      fMemTest.btStart.Caption := 'Start';
      fMemTest.btStart.Enabled := True;
    end;
  end;
end;

{*************}
{* TfMemTest *}
{*************}

{ Application start: Initialisation }

procedure TfMemTest.FormCreate(Sender: TObject);

begin
  // Create array of object images
  ObjImg[1]  := imObject1;  ObjImg[2]  := imObject2;  ObjImg[3]  := imObject3;
  ObjImg[4]  := imObject4;  ObjImg[5]  := imObject5;  ObjImg[6]  := imObject6;
  ObjImg[7]  := imObject7;  ObjImg[8]  := imObject8;  ObjImg[9]  := imObject9;
  ObjImg[10] := imObject10; ObjImg[11] := imObject11; ObjImg[12] := imObject12;
  ObjImg[13] := imObject13; ObjImg[14] := imObject14; ObjImg[15] := imObject15;
  ObjImg[16] := imObject16; ObjImg[17] := imObject17; ObjImg[18] := imObject18;
  ObjImg[19] := imObject19; ObjImg[20] := imObject20;
  // Set start-up values
  sObjects := 'Numbers';
  sDisplay := 'one'; iTimer := 2000;
  iObjList := 10; iObjects := 5; iCheckObject := -1;
  // Get subset objects
  GetObjects(sObjects, iObjList, True, aObjList);
  // Start random number generator
  Randomize;
end;

{ Menu item "Test > New": Prepare to start new test}

procedure TfMemTest.mTestNewClick(Sender: TObject);

begin
  // Read test parameters from data input form
  fNew.ShowModal;
  iObjects := StrToInt(fNew.edObjects.Text);
  if fNew.rbDisplay1.Checked then
    sDisplay := 'one'
  else if fNew.rbDisplayAll.Checked then
    sDisplay := 'all';
  // Update values on main form
  edObjects.Text := IntToStr(iObjects);
  edTime.Text := IntToStr(iTimer div 1000);
  edDisplay.Text := sDisplay;
  btStart.Caption := 'Start';
end;

{ Menu item "Test > Exit": Exit the application}

procedure TfMemTest.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Objects": Select object set and get subset objects }

procedure TfMemTest.mObjNumbersClick(Sender: TObject);

begin
  if not mObjNumbers.Checked then begin
    mObjNumbers.Checked := True;  mObjShapes2D.Checked := False;  mObjShapes3D.Checked := False;
    mObjColors.Checked := False;  mObjFruits.Checked := False;    mObjObjects.Checked := False;
    sObjects := 'Numbers';
    GetObjects(sObjects, iObjList, True, aObjList);
  end;
end;

procedure TfMemTest.mObjShapes2DClick(Sender: TObject);

begin
  if not mObjShapes2D.Checked then begin
    mObjNumbers.Checked := False;  mObjShapes2D.Checked := True;  mObjShapes3D.Checked := False;
    mObjColors.Checked := False;   mObjFruits.Checked := False;   mObjObjects.Checked := False;
    sObjects := 'Shapes2d';
    GetObjects(sObjects, iObjList, True, aObjList);
  end;
end;

procedure TfMemTest.mObjShapes3DClick(Sender: TObject);

begin
  if not mObjShapes3D.Checked then begin
    mObjNumbers.Checked := False;  mObjShapes2D.Checked := False;  mObjShapes3D.Checked := True;
    mObjColors.Checked := False;   mObjFruits.Checked := False;    mObjObjects.Checked := False;
    sObjects := 'Shapes3d';
    GetObjects(sObjects, iObjList, True, aObjList);
  end;
end;

procedure TfMemTest.mObjColorsClick(Sender: TObject);

begin
  if not mObjColors.Checked then begin
    mObjNumbers.Checked := False;  mObjShapes2D.Checked := False;  mObjShapes3D.Checked := False;
    mObjColors.Checked := True;    mObjFruits.Checked := False;    mObjObjects.Checked := False;
    sObjects := 'Colors';
    GetObjects(sObjects, iObjList, True, aObjList);
  end;
end;

procedure TfMemTest.mObjFruitsClick(Sender: TObject);

begin
  if not mObjFruits.Checked then begin
    mObjNumbers.Checked := False;  mObjShapes2D.Checked := False;  mObjShapes3D.Checked := False;
    mObjColors.Checked := False;   mObjFruits.Checked := True;     mObjObjects.Checked := False;
    sObjects := 'Fruits';
    GetObjects(sObjects, iObjList, True, aObjList);
  end;
end;

procedure TfMemTest.mObjObjectsClick(Sender: TObject);

begin
  if not mObjObjects.Checked then begin
    mObjNumbers.Checked := False;  mObjShapes2D.Checked := False;  mObjShapes3D.Checked := False;
    mObjColors.Checked := False;   mObjFruits.Checked := False;    mObjObjects.Checked := True;
    sObjects := 'Objects';
    GetObjects(sObjects, iObjList, True, aObjList);
  end;
end;

{ Menu items "Pictures > Selection": Select number of subset objects and get those}

procedure TfMemTest.mPicSel2Click(Sender: TObject);

begin
  if not mPicSel2.Checked then begin
    mPicSel2.Checked  := True;  mPicSel5.Checked  := False;  mPicSel8.Checked  := False;  mPicSel10.Checked := False;
    iObjList := 2;
    GetObjects(sObjects, iObjList, False, aObjList);
  end;
end;

procedure TfMemTest.mPicSel5Click(Sender: TObject);

begin
  if not mPicSel5.Checked then begin
    mPicSel2.Checked  := False;  mPicSel5.Checked  := True;  mPicSel8.Checked  := False;  mPicSel10.Checked := False;
    iObjList := 5;
    GetObjects(sObjects, iObjList, False, aObjList);
  end;
end;

procedure TfMemTest.mPicSel8Click(Sender: TObject);

begin
  if not mPicSel8.Checked then begin
    mPicSel2.Checked  := False;  mPicSel5.Checked  := False;  mPicSel8.Checked  := True;  mPicSel10.Checked := False;
    iObjList := 8;
    GetObjects(sObjects, iObjList, False, aObjList);
  end;
end;

procedure TfMemTest.mPicSel10Click(Sender: TObject);

begin
  if not mPicSel10.Checked then begin
    mPicSel2.Checked  := False;  mPicSel5.Checked  := False;  mPicSel8.Checked  := False;  mPicSel10.Checked := True;
    iObjList := 10;
    GetObjects(sObjects, iObjList, False, aObjList);
  end;
end;

{ Menu items "Pictures > Time": Get objects display time }

procedure TfMemTest.mPicTime1Click(Sender: TObject);

begin
  if not mPicTime1.Checked then begin
    mPicTime1.Checked  := True;  mPicTime2.Checked  := False;  mPicTime5.Checked  := False;  mPicTime10.Checked := False;
    iTimer := 1000;
    edTime.Text := IntToStr(iTimer div 1000);
  end;
end;

procedure TfMemTest.mPicTime2Click(Sender: TObject);

begin
  if not mPicTime2.Checked then begin
    mPicTime1.Checked  := False;  mPicTime2.Checked  := True;  mPicTime5.Checked  := False;  mPicTime10.Checked := False;
    iTimer := 2000;
    edTime.Text := IntToStr(iTimer div 1000);
  end;
end;

procedure TfMemTest.mPicTime5Click(Sender: TObject);

begin
  if not mPicTime5.Checked then begin
    mPicTime1.Checked  := False;  mPicTime2.Checked  := False;  mPicTime5.Checked  := True;  mPicTime10.Checked := False;
    iTimer := 5000;
    edTime.Text := IntToStr(iTimer div 1000);
  end;
end;

procedure TfMemTest.mPicTime10Click(Sender: TObject);

begin
  if not mPicTime10.Checked then begin
    mPicTime1.Checked  := False;  mPicTime2.Checked  := False;  mPicTime5.Checked  := False;  mPicTime10.Checked := True;
    iTimer := 10000;
    edTime.Text := IntToStr(iTimer div 1000);
  end;
end;

{ Menu item "Help > Help": Display Help content }

procedure TfMemTest.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application info }

procedure TfMemTest.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if Popup.Visible then
    Popup.Visible := False
  else begin
    S := Chr(13) + 'Test your memory and try to ameliorate it!';
    S += Chr(13) + Chr(13) + '© allu, February, 2018.';
    Popup.Text := S;
    Popup.Visible := True;
  end;
end;

{ Button "Start/Check": Start the test / Check user objects memory }

procedure TfMemTest.btStartClick(Sender: TObject);

var
  I: Integer;

begin
  // Starting the test
  if btStart.Caption = 'Start' then begin
    iObject := 1;
    // One by one objects display
    if sDisplay = 'one' then begin
      // Display will be done by timer routine on fOne form
      fOne.edCancel.Text := '';
      bShowPic := True;
      if not fOne.Visible then
        fOne.Show;
    end
    // All together objects isplay
    else begin
      // Display will be done by timer routine on fAll form
      iTimeLeft := 10 * (iTimer div 1000);
      fAll.Label1.Visible := True;
      fAll.edTime.Visible := True;
      fAll.edButton.Text := '';
      fAll.btOK.Visible := True;
      for I := 1 to 50 do
        fAll.ObjImg[I].Visible := False;
      if not fAll.Visible then
        fAll.Show;
    end;
    // Start the timer
    tiMemTest.Interval := 10;                                                  // 10 msec = immediate execution; real timer value then set in timer routine
    tiMemTest.Enabled := True;
    btStart.Enabled := False;
  end
  // Checking user objects memory
  else begin
    // Display will be done on fAll form (each time the user clicks the object she thinks remember at actual position)
    for I := 1 to 50 do
      fAll.ObjImg[I].Visible := False;
    fAll.btOK.Visible := False;
    fAll.Label1.Visible := False;
    fAll.edTime.Visible := False;
    fAll.edButton.Text := '';
    fAll.Show;
    iCheckObject := 0;
    btStart.Enabled := False;
    fMemTest.SetFocus;
  end;
end;

{ Timer routine: Randomly seected objects display }

procedure TfMemTest.tiMemTestTimer(Sender: TObject);

var
  M, R, I: Integer;
  Filename: string;

begin
  // Real objects total (considering special case "Colored objects")
  if sOBjects = 'Objects' then
    M := 2
  else
    M := 1;
  // Test = One by one objects display
  if sDisplay = 'one' then begin
    if (fOne.edCancel.Text = 'C') or (iObject > iObjects) then begin
      // Test canceled or all objects displayed
      tiMemTest.Enabled := False;                                              // disable the timer
      fOne.Close;                                                              // close fOne (form where the objects were displayed)
      if fOne.edCancel.Text <> 'C' then                                        // if all objects were displayed, prepare to check user's memory
        btStart.Caption := 'Check';
      btStart.Enabled := True;
    end
    else begin
      // Current object display: alternation of random object select/display and pause displaying empty picture
      if bShowPic then begin
        // Object select and display
        tiMemTest.Interval := iTimer;                                          // display time timer value
        fOne.stObject.Caption := 'Object ' + IntToStr(iObject) + '/' + IntToStr(iObjects);
        R := Random(M * iObjList) + 1;                                         // random object from actual subset
        Filename := 'objects/' + sObjects + IntToStr(aObjList[R]) + '.jpg';
        fOne.imObject.Picture.LoadFromFile(Filename);                          // display object on fOne form
        aRandomObjects[iObject] := aObjList[R];                                // save object index in list for user memory check
        Inc(iObject);
        bShowPic := False;
      end
      else begin
        // Pause (empty image)
        tiMemTest.Interval := 250;                                             // pause time timer value
        fOne.imObject.Picture.Clear;
        bShowPic := True;
      end;
    end;
  end
  // Test = All together objects display
  else begin
    if (fAll.edButton.Text <> '') or (iTimeLeft = 0) then begin
      // Test canceled, display terminated by user or display time over
      tiMemTest.Enabled := False;                                              // disable timer
      fAll.Close;                                                              // close fALL (form where the objects were displayed)
      if fAll.edButton.Text <> 'cancel' then                                   // except if test was canceled, prepare to check user's memory
        btStart.Caption := 'Check';
      btStart.Enabled := True;
    end
    else begin
      // Objects select and display resp. time left update
      if iObject = 1 then begin
        // At first routine execution, select and display all objects
        tiMemTest.Interval := 1000;                                            //  1 sec timer for display time left update
        for I := 1 to iObjects do begin
          R := Random(M * iObjList) + 1;                                       // randomly chosen object
          Filename := 'objects/' + sObjects + IntToStr(aObjList[R]) + '.jpg';
          fAll.ObjImg[I].Picture.LoadFromFile(Filename);                       // object display on form fAll
          fAll.ObjImg[I].Visible := True;
          aRandomObjects[I] := aObjList[R];                                    // save object index to list for user memory check
        end;
        iObject := I;
      end;
      Dec(iTimeLeft);                                                          // time left - 1 (every sec)
      fAll.edTime.Text := IntToStr(iTimeLeft);
    end;
  end;
end;

{ Checking user object memory (clicking the object remembered at actual position ) }

procedure TfMemTest.imObject1Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 1, aRandomObjects);
end;

procedure TfMemTest.imObject2Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 2, aRandomObjects);
end;

procedure TfMemTest.imObject3Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 3, aRandomObjects);
end;

procedure TfMemTest.imObject4Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 4, aRandomObjects);
end;

procedure TfMemTest.imObject5Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 5, aRandomObjects);
end;

procedure TfMemTest.imObject6Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 6, aRandomObjects);
end;

procedure TfMemTest.imObject7Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 7, aRandomObjects);
end;

procedure TfMemTest.imObject8Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 8, aRandomObjects);
end;

procedure TfMemTest.imObject9Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 9, aRandomObjects);
end;

procedure TfMemTest.imObject10Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 10, aRandomObjects);
end;

procedure TfMemTest.imObject11Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 11, aRandomObjects);
end;

procedure TfMemTest.imObject12Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 12, aRandomObjects);
end;

procedure TfMemTest.imObject13Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 13, aRandomObjects);
end;

procedure TfMemTest.imObject14Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 14, aRandomObjects);
end;

procedure TfMemTest.imObject15Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 15, aRandomObjects);
end;

procedure TfMemTest.imObject16Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 16, aRandomObjects);
end;

procedure TfMemTest.imObject17Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 17, aRandomObjects);
end;

procedure TfMemTest.imObject18Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 18, aRandomObjects);
end;

procedure TfMemTest.imObject19Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 19, aRandomObjects);
end;

procedure TfMemTest.imObject20Click(Sender: TObject);

begin
  CheckObject(iCheckObject, iObjects, 20, aRandomObjects);
end;

{ Routine to intercept push of "Cancel" button on fAll form during user object memory check }

procedure TfMemTest.FormActivate(Sender: TObject);

begin
  if btStart.Caption = 'Check' then begin
    if fAll.edButton.Text = 'cancel' then begin
      iCheckObject := -1;
      btStart.Caption := 'Start';
      btStart.Enabled := True;
    end;
  end;
end;

end.

