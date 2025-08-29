{**************************************}
{* Main unit for Priorite application *}
{**************************************}

unit priorite_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, PopupNotifier, priorite_u2;

type
  TCar = record
    Road, Direction, Opposite, Right, Left: Char;
  end;
  TCars = array[1..4] of TCar;
  TBArray4 = array[1..4] of Boolean;
  TIArray7 = array[1..7] of Integer;
  { TfPriorite }
  TfPriorite = class(TForm)
    mPriorite: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mOptions, mOptionsNQuestions: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    shBackground: TShape; stQuestion: TStaticText;
    shRoadLeft1, shRoadLeft2, shRoadRight1, shRoadRight2: TShape;
    shRoadTop1a, shRoadTop1b, shRoadTop2a, shRoadTop2b, shRoadTop0a, shRoadTop0b: TShape;
    shRoadBottom1, shRoadBottom2, shRoadBottom0: TShape;
    imCarLeft: TImage; shMoveLeftBase, shMoveLeftStraight, shMoveLeftRight, shMoveLeftLeft: TShape;
    imCarRight: TImage; shMoveRightBase, shMoveRightStraight, shMoveRightRight, shMoveRightLeft: TShape;
    imCarTop: TImage; shMoveTopBase, shMoveTopStraight, shMoveTopRight, shMoveTopLeft: TShape;
    imCarBottom: TImage; shMoveBottomBase, shMoveBottomStraight, shMoveBottomRight, shMoveBottomLeft: TShape;
    imStopLeft: TImage; shStopLeft: TShape;
    imStopRight: TImage; shStopRight: TShape;
    imStopTop: TImage; shStopTop: TShape;
    imStopBottom: TImage; shStopBottom: TShape;
    imParking: TImage; shParking: TShape;
    Label1: TLabel;
    laPriority1: TLabel; imPriority1a, imPriority1b: TImage;
    laPriority2: TLabel; imPriority2a, imPriority2b: TImage;
    laPriority3: TLabel; imPriority3a, imPriority3b: TImage;
    laPriority4: TLabel; imPriority4: TImage;
    edEval: TEdit;
    memoHelp: TMemo;
    Label2: TLabel;
    Label3: TLabel; edQuestions: TEdit;
    Label4: TLabel; edCorrect: TEdit;
    Label5: TLabel; edFalse: TEdit;
    Label6: TLabel; edSuccess: TEdit;
    btQuestion: TButton;
    btAnswer: TButton;
    popupAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsNQuestionsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure imCarLeftDblClick(Sender: TObject);
    procedure imCarRightDblClick(Sender: TObject);
    procedure imCarTopDblClick(Sender: TObject);
    procedure imCarBottomDblClick(Sender: TObject);
    procedure imPriority1aClick(Sender: TObject);
    procedure imPriority1aDblClick(Sender: TObject);
    procedure imPriority1bClick(Sender: TObject);
    procedure imPriority1bDblClick(Sender: TObject);
    procedure imPriority2aClick(Sender: TObject);
    procedure imPriority2aDblClick(Sender: TObject);
    procedure imPriority2bClick(Sender: TObject);
    procedure imPriority2bDblClick(Sender: TObject);
    procedure imPriority3aClick(Sender: TObject);
    procedure imPriority3aDblClick(Sender: TObject);
    procedure imPriority3bClick(Sender: TObject);
    procedure imPriority3bDblClick(Sender: TObject);
    procedure imPriority4Click(Sender: TObject);
    procedure imPriority4DblClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuestions, iCorrect, iFalse, iQuestion, iCars, iPX, iCarsDone: Integer;
    cStopRoads, cNoCar, cParking: Char;
    bNoAnswer: Boolean;
    Cars: TCars;
    abCarsDone: TBArray4;
    aiPriorities, aiUserPriorities: TIArray7;
  end;

var
  fPriorite: TfPriorite;
  imPriorities: array[1..7] of TImage;

procedure ResetForm;
procedure DisplayCars(N: Integer; Cars: TCars);
procedure DisplayStops(Stop1, Stop2: Char); procedure DisplayStop(Stop: Char);
procedure DisplayParking;
procedure HideRoad(Road: Char);
procedure QuestionGenerate(NQ: Integer; var Q, N: Integer; var Cars: TCars);
procedure CalculatePriorities(Cars: TCars; N: Integer; StopRoads, NoCar, Parking: Char; var Priorities: TIArray7);
procedure AnswerFieldPlaceCar(var NDone: Integer; var CarsDone: TBArray4; N: Integer; Cars: TCars; Road: Char; var PX: Integer; var Priorities: TIArray7);
procedure AnswerFieldRemoveCar(var NDone: Integer; var CarsDone: TBArray4; P: Integer; var PX: Integer; var Priorities: TIArray7);
procedure AnswerFieldActivate(P: Integer; var PX: Integer; Priorities: TIArray7);
procedure DisplayCounters(Total, Correct, False: Integer);
procedure DisplayEndOfTest(Questions, Correct: Integer);
procedure CarRoads(var Car: TCar);
function RoadCar(Cars: TCars; N: Integer; Road: Char): Integer;
function ValidMove(Car: TCar; NoRoad: Char): Boolean;

implementation

{************************}
{* Implementation notes *}
{************************}

// The layout of the crossing including the roads, the cars with the indication of their move direction, the traffic signs and the
// parking consists of a mixture of predefined images and shapes, which are set visible or invisible depending on the actual situation.

// The priorities calculated by the program are stored as an array where each element corresponds to one of the answer fields, where
// the user can put the cars, and whose content consists of a number representing a given car. When the user places cars onto the
// answer fields a similar array is filled-in. If all 7 elements (with 0 meaning no car placed there) of the 2 arrays are equal,
// the user's answer to the test question is correct.

{$R *.lfm}

{ Reset the form objects = making objects visible/invisible }

procedure ResetForm;

var
  FName: string;

begin
  // Make vertical roads visible
  fPriorite.shRoadTop1a.Visible := True; fPriorite.shRoadTop1b.Visible := True;
  fPriorite.shRoadTop2a.Visible := True; fPriorite.shRoadTop2b.Visible := True;
  fPriorite.shRoadTop0a.Visible := False; fPriorite.shRoadTop0b.Visible := False;                  // this is set visible if there isn't a "top" road
  fPriorite.shRoadBottom1.Visible := True; fPriorite.shRoadBottom2.Visible := True;
  fPriorite.shRoadBottom0.Visible := False;                                                        // this is set visible if there isn't a "bottom" road
  // Hide stop signs
  fPriorite.imStopLeft.Visible := False; fPriorite.shStopLeft.Visible := False;
  fPriorite.imStopRight.Visible := False; fPriorite.shStopRight.Visible := False;
  fPriorite.imStopTop.Visible := False; fPriorite.shStopTop.Visible := False;
  fPriorite.imStopBottom.Visible := False; fPriorite.shStopBottom.Visible := False;
  // Hide parking sign
  fPriorite.imParking.Visible := False; fPriorite.shParking.Visible := False;
  // Hide all 4 cars
  fPriorite.imCarLeft.Visible := False; fPriorite.imCarRight.Visible := False;
  fPriorite.imCarTop.Visible := False; fPriorite.imCarBottom.Visible := False;
  // Hide car move direction indicators
  fPriorite.shMoveLeftBase.Visible := False; fPriorite.shMoveLeftStraight.Visible := False;
  fPriorite.shMoveLeftRight.Visible := False; fPriorite.shMoveLeftLeft.Visible := False;
  fPriorite.shMoveRightBase.Visible := False; fPriorite.shMoveRightStraight.Visible := False;
  fPriorite.shMoveRightRight.Visible := False; fPriorite.shMoveRightLeft.Visible := False;
  fPriorite.shMoveTopBase.Visible := False; fPriorite.shMoveTopStraight.Visible := False;
  fPriorite.shMoveTopRight.Visible := False; fPriorite.shMoveTopLeft.Visible := False;
  fPriorite.shMoveBottomBase.Visible := False; fPriorite.shMoveBottomStraight.Visible := False;
  fPriorite.shMoveBottomRight.Visible := False; fPriorite.shMoveBottomLeft.Visible := False;
  // Display all 7 answer fields (as white rectangles = empty and not selected) and corresponding labels
  FName := './pics/nocar.jpg';
  DoDirSeparators(FName);
  fPriorite.imPriority1a.Picture.LoadFromFile(FName); fPriorite.imPriority1a.Visible := True; fpriorite.laPriority1.Visible := True;
  fPriorite.imPriority1b.Picture.LoadFromFile(FName); fPriorite.imPriority1b.Visible := True;
  fPriorite.imPriority2a.Picture.LoadFromFile(FName); fPriorite.imPriority2a.Visible := True; fpriorite.laPriority2.Visible := True;
  fPriorite.imPriority2b.Picture.LoadFromFile(FName); fPriorite.imPriority2b.Visible := True;
  fPriorite.imPriority3a.Picture.LoadFromFile(FName); fPriorite.imPriority3a.Visible := True; fpriorite.laPriority3.Visible := True;
  fPriorite.imPriority3b.Picture.LoadFromFile(FName); fPriorite.imPriority3b.Visible := True;
  fPriorite.imPriority4.Picture.LoadFromFile(FName); fPriorite.imPriority4.Visible := True; fpriorite.laPriority4.Visible := True;
  // Clear the evaluation field
  fPriorite.edEval.Color := clDefault; fPriorite.edEval.Text := '';
  // Enable "Question" button, disable "Answer" button
  fPriorite.btQuestion.Enabled := True; fPriorite.btAnswer.Enabled := False;
end;

{ Display (= make visible) cars and move direction indicators }

procedure DisplayCars(N: Integer; Cars: TCars);

var
  I: Integer;
  FName: string;

begin
  for I := 1 to N do begin
    // Place each car with given move direction indicators on a given road
    // Car pictures are loaded from file (1 image for each road = each car orientation)
     case Cars[I].Road of
       'L': begin
              FName := './pics/Car' + IntToStr(I) + '_l.jpg';
              DoDirSeparators(FName);
              fPriorite.imCarLeft.Picture.LoadFromFile(FName);
              fPriorite.imCarLeft.Visible := True;
              fPriorite.shMoveLeftBase.Visible := True;
              case Cars[I].Direction of
                'S': fPriorite.shMoveLeftStraight.Visible := True;
                'L': begin
                       fPriorite.shMoveLeftStraight.Visible := True;
                       fPriorite.shMoveLeftLeft.Visible := True;
                     end;
                'R': fPriorite.shMoveLeftRight.Visible := True;
              end;
            end;
       'R': begin
              FName := './pics/Car' + IntToStr(I) + '_r.jpg';
              DoDirSeparators(FName);
              fPriorite.imCarRight.Picture.LoadFromFile(FName);
              fPriorite.imCarRight.Visible := True;
              fPriorite.shMoveRightBase.Visible := True;
              case Cars[I].Direction of
                'S': fPriorite.shMoveRightStraight.Visible := True;
                'L': begin
                       fPriorite.shMoveRightStraight.Visible := True;
                       fPriorite.shMoveRightLeft.Visible := True;
                     end;
                'R': fPriorite.shMoveRightRight.Visible := True;
              end;
            end;
       'T': begin
              FName := './pics/Car' + IntToStr(I) + '_t.jpg';
              DoDirSeparators(FName);
              fPriorite.imCarTop.Picture.LoadFromFile(FName);
              fPriorite.imCarTop.Visible := True;
              fPriorite.shMoveTopBase.Visible := True;
              case Cars[I].Direction of
                'S': fPriorite.shMoveTopStraight.Visible := True;
                'L': begin
                       fPriorite.shMoveTopStraight.Visible := True;
                       fPriorite.shMoveTopLeft.Visible := True;
                     end;
                'R': fPriorite.shMoveTopRight.Visible := True;
              end;
            end;
       'B': begin
              FName := './pics/Car' + IntToStr(I) + '_b.jpg';
              DoDirSeparators(FName);
              fPriorite.imCarBottom.Picture.LoadFromFile(FName);
              fPriorite.imCarBottom.Visible := True;
              fPriorite.shMoveBottomBase.Visible := True;
              case Cars[I].Direction of
                'S': fPriorite.shMoveBottomStraight.Visible := True;
                'L': begin
                       fPriorite.shMoveBottomStraight.Visible := True;
                       fPriorite.shMoveBottomLeft.Visible := True;
                     end;
                'R': fPriorite.shMoveBottomRight.Visible := True;
              end;
            end;
     end;
  end;
end;

{ Display (= make visible) stop signs }

procedure DisplayStops(Stop1, Stop2: Char);

begin
  DisplayStop(Stop1);
  DisplayStop(Stop2);
end;

{ Display (= make visible) stop sign }

procedure DisplayStop(Stop: Char);

begin
  // Display stop sign on given road
  case Stop of
    'L': begin
           fPriorite.imStopLeft.Visible := True;
           fPriorite.shStopLeft.Visible := True;
         end;
    'R': begin
           fPriorite.imStopRight.Visible := True;
           fPriorite.shStopRight.Visible := True;
         end;
    'T': begin
           fPriorite.imStopTop.Visible := True;
           fPriorite.shStopTop.Visible := True;
         end;
    'B': begin
           fPriorite.imStopBottom.Visible := True;
           fPriorite.shStopBottom.Visible := True;
           end;
  end;
end;

{ Display (= make visible) parking }

procedure DisplayParking;

begin
  // "Transform" road into parking
  fPriorite.shRoadTop1b.Visible := False; fPriorite.shRoadTop2b.Visible := False; fPriorite.shRoadTop0b.Visible := True;
  // Display parking sign
  fPriorite.imParking.Visible := True; fPriorite.shParking.Visible := True;
end;

{ Hide (= make invisible) road (for examples with 3 roads only) }

procedure HideRoad(Road: Char);

begin
  if Road = 'T' then begin
    // "Top" road
    fPriorite.shRoadTop1a.Visible := False; fPriorite.shRoadTop1b.Visible := False;
    fPriorite.shRoadTop2a.Visible := False; fPriorite.shRoadTop2b.Visible := False;
    fPriorite.shRoadTop0a.Visible := True;
  end
  else begin
    // "Bottom" road
    fPriorite.shRoadBottom1.Visible := False; fPriorite.shRoadBottom2.Visible := False;
    fPriorite.shRoadBottom0.Visible := True;
  end;
end;

{ Generate test question and display corresponding crossing situation }

procedure QuestionGenerate(NQ: Integer; var Q, N: Integer; var Cars: TCars);

const
  Roads: array[1..4] of Char = ('L', 'R', 'T', 'B');
  Directions: array[1..3] of Char = ('S', 'R', 'L');

var
  R, I, J: Integer;
  NoRoad: Char;

begin
  // 2, 3 or 4 cars
  R := Random(10) + 1;
  case R of
        1: N := 2;
     2..6: N := 3;
    7..10: N := 4;
  end;
  fPriorite.cStopRoads := '-'; fPriorite.cNoCar := '-'; fPriorite.cParking := '-';
  // 2 cars test question
  if N = 2 then begin
    // 2 main types of questions in this case
    R := Random(2) + 1;
    if R = 1 then begin
      // Cars on opposite roads
      R := Random(4) + 1;
      case R of
        // Random roads and random straight/left move directions
        1: begin
             Cars[1].Road := 'L'; Cars[1].Direction := 'S';
             Cars[2].Road := 'R'; Cars[2].Direction := 'L';
           end;
        2: begin
             Cars[1].Road := 'L'; Cars[1].Direction := 'L';
             Cars[2].Road := 'R'; Cars[2].Direction := 'S';
           end;
        3: begin
             Cars[1].Road := 'T'; Cars[1].Direction := 'S';
             Cars[2].Road := 'B'; Cars[2].Direction := 'L';
           end;
        4: begin
             Cars[1].Road := 'T'; Cars[1].Direction := 'S';
             Cars[2].Road := 'B'; Cars[2].Direction := 'L';
           end;
      end;
      // Some examples with both cars turning to the left
      R := Random(10) + 1;
      if R = 1 then begin
        if Cars[1].Direction = 'S' then
          Cars[1].Direction := 'L'
        else
          Cars[2].Direction := 'L';
      end;
      CarRoads(Cars[1]); CarRoads(Cars[2]);                                                        // determine relative road positions of car 1
    end
    else begin
      // Cars on adjacent roads (with random move directions)
      Cars[1].Road := Roads[Random(4) + 1];
      CarRoads(Cars[1]);                                                                           // determine relative road positions of car 1
      Cars[1].Direction := Directions[Random(3) + 1];                                              // random move direction
      repeat
        R := Random(4) + 1;
      until (Roads[R] <> Cars[1].Road) and (Roads[R] <> Cars[1].Opposite);
      Cars[2].Road := Roads[R];
      CarRoads(Cars[2]);                                                                           // determine relative road positions of car 2
      Cars[2].Direction := Directions[Random(3) + 1];                                              // random move direction
    end;
    // Hide priority 3 and 4 answer fields
    fPriorite.imPriority3a.Visible := False; fpriorite.laPriority3.Visible := False;
    fPriorite.imPriority3b.Visible := False;
    fPriorite.imPriority4.Visible := False; fpriorite.laPriority4.Visible := False;
  end
  // 3 cars test question
  else if N = 3 then begin
    // 2 main question types
    R := Random(2) + 1;
    if R = 1 then begin
      // 4 roads test question
      R := Random(10) + 1;
      if R = 1 then begin
        // Some examples with parking
        DisplayParking;
        fPriorite.cParking := 'T';
      end;
      R := Random(3) + 1;
      if R = 1 then begin
        // Examples with stop sign(s)
        R := Random(2) + 1;
        if (R = 1) and (fPriorite.cParking = '-') then begin
          DisplayStops('L', 'R');
          fPriorite.cStopRoads := 'H';
        end
        else begin
          if fPriorite.cParking = '-' then begin
            DisplayStops('T', 'B');
            fPriorite.cStopRoads := 'V';
          end
          else begin
            DisplayStop('B');
            fPriorite.cStopRoads := 'B';
          end;
        end;
      end;
      // Random road without a car (must not be parking)
      repeat
        R := Random(4) + 1;
      until Roads[R] <> fPriorite.cParking;
      fPriorite.cNoCar := Roads[R];
      // Place the 3 cars (on the 4 roads)
      I := 1; J := 1;
      while I <= 3 do begin
        if J = R then
          Inc(J);
        Cars[I].Road := Roads[J];
        Inc(I); Inc(J);
      end;
      for I := 1 to 3 do begin
        CarRoads(Cars[I]);                                                                         // determine road positions relative to car
        Cars[I].Direction := Directions[Random(3) + 1];                                            // random move direction
      end;
    end
    else begin
      // 3 roads test question
      R := Random(2) + 1;
      // No road at "top" or at "bottom"
      if R = 1 then
        NoRoad := 'T'
      else
        NoRoad := 'B';
      fPriorite.cNoCar := NoRoad;
      HideRoad(NoRoad);                                                                            // hide the "non existing" road
      R := Random(4) + 1;
      if R = 1 then begin
        // Examples with stop sign
        if NoRoad = 'T'  then
          fPriorite.cStopRoads := 'B'
        else
          fPriorite.cStopRoads := 'T';
        DisplayStop(fPriorite.cStopRoads);
      end;
      // Place the 3 cars (on the 3 existing roads)
      I := 1; J := 1;
      while I <= 3 do begin
        if Roads[J] = NoRoad then
          Inc(J);
        Cars[I].Road := Roads[J];
        Inc(I); Inc(J);
      end;
      for I := 1 to 3 do begin
        CarRoads(Cars[I]);                                                                         // determine road positions relative to car
        repeat
          Cars[I].Direction := Directions[Random(3) + 1];
        until ValidMove(Cars[I], NoRoad);                                                          // car can't move towards "non existing" road
      end;
    end;
    // Hide priority 4 answer field
    fPriorite.imPriority4.Visible := False; fpriorite.laPriority4.Visible := False;
  end
  // 4 cars test question
  else begin
    R := Random(10) + 1;
    if R = 1 then begin
      // Some examples with parking
      DisplayParking;
      fPriorite.cParking := 'T';
    end;
    R := Random(2) + 1;
    // All examples with stop signs
    if (R = 1) and (fPriorite.cParking = '-') then begin
      // Stops at "left" and at "right" roads (only if there is no parking)
      DisplayStops('L', 'R');
      fPriorite.cStopRoads := 'H';
    end
    else begin
      if fPriorite.cParking = '-' then begin
        // Stops at "top" and at "bottom" roads (if there is no parking)
        DisplayStops('T', 'B');
        fPriorite.cStopRoads := 'V';
      end
      else begin
        // Stop at "bottom" road only (if there is a parking)
        DisplayStop('B');
        fPriorite.cStopRoads := 'B';
      end;
    end;
    // Place the 4 cars
    Cars[1].Road := 'L'; Cars[2].Road := 'R'; Cars[3].Road := 'T'; Cars[4].Road := 'B';
    for I := 1 to 4 do begin
      CarRoads(Cars[I]);                                                                           // determine road positions relative to car
      Cars[I].Direction := Directions[Random(3) + 1];                                              // random move direction
    end;
  end;
  // Update question title
  fPriorite.stQuestion.Caption := 'Question ' + IntToStr(Q) + ' / ' + IntToStr(NQ);
  // Display the cars
  DisplayCars(N, Cars);
  // Enable "Answer" button and give it the focus
  fPriorite.btAnswer.Enabled := True;
  fPriorite.btAnswer.SetFocus;
end;

{ Calculate priorities for actual crossing position }

procedure CalculatePriorities(Cars: TCars; N: Integer; StopRoads, NoCar, Parking: Char; var Priorities: TIArray7);

var
  CX, PX, I, J, Temp: Integer;
  Max, OldMax: Real;
  PValues: array[1..4] of Real;

begin
  for I := 1 to 4 do
    PValues[I] := 0;
  // Crossing without stop signs
  if StopRoads = '-' then begin
    if N = 2 then begin
      // Crossing with 2 cars
      if Cars[2].Road = Cars[1].Opposite then begin
        // Cars are on opposite roads ("left turner must wait")
        if ((Cars[1].Direction = 'S') or (Cars[1].Direction = 'R')) and (Cars[2].Direction = 'L') then begin
          Priorities[1] := 1; Priorities[2] := 2;
        end
        else if ((Cars[2].Direction = 'S') or (Cars[2].Direction = 'R')) and (Cars[1].Direction = 'L') then begin
          Priorities[1] := 2; Priorities[2] := 1;
        end
        else begin
          Priorities[1] := 1; Priorities[5] := 2;                                                  // two "left turners" go at the same time
        end;
      end
      else begin
        // Cars are on adjacent roads ("right free priority rule")
        if Cars[2].Right = Cars[1].Road then begin
          Priorities[1] := 1; Priorities[2] := 2;
        end
        else begin
          Priorities[1] := 2; Priorities[2] := 1;
        end;
      end;
    end
    else begin
      // Crossing with 3 cars
      if (Parking = 'T') and (NoCar = 'B') then begin
        // The 2 non-parking cars are on opposite roads ("left turner must wait")
        PX := 3;
        if ((Cars[1].Direction = 'S') or (Cars[1].Direction = 'R')) and (Cars[2].Direction = 'L') then begin
          Priorities[1] := 1; Priorities[2] := 2;
        end
        else if ((Cars[2].Direction = 'S') or (Cars[2].Direction = 'R')) and (Cars[1].Direction = 'L') then begin
          Priorities[1] := 2; Priorities[2] := 1;
        end
        else begin
          Priorities[1] := 1; Priorities[5] := 2; PX := 2;
        end;
      end
      else begin
        // The 2 resp. 3 non-parking cars are on adjacent roads ("right free priority rule")
        PX := 3;
        for I := 1 to N do begin
          for J := 1 to N do begin
            if (Cars[J].Road <> Parking) and ((Cars[J].Right = NoCar) or (Cars[J].Right = Parking)) then
              CX := J;
          end;
          Priorities[I] := CX;
          NoCar := Cars[CX].Road;
        end;
      end;
      // Car on parking has lowest priority
      if Parking <> '-' then
        Priorities[PX] := RoadCar(Cars, N, 'T');                                                   // use function to determine number of car on parking
    end;
  end
  // Crossing with stop signs (all 4 cars and part of the 3 cars examples)
  else begin
    for I := 1 to N do begin
      // Give each car a "priority value" based on stop signs position and move direction
      with Cars[I] do begin
        if Cars[I].Road = Parking then
          // Car on parking with minimal priority value
          PValues[I] := -3
        else begin
          // Priority value = -1 if car has a stop sign, +1 otherwise
          case Road of
            'L': if (StopRoads = 'H') or (StopRoads = 'L') then
                   PValues[I] := -1
                 else
                   PValues[I] := 1;
            'R': if (StopRoads = 'H') or (StopRoads = 'R') then
                   PValues[I] := -1
                 else
                   PValues[I] := 1;
            'T': if (StopRoads = 'V') or (StopRoads = 'T') then
                   PValues[I] := -1
                 else
                   PValues[I] := 1;
            'B': if (StopRoads = 'V') or (StopRoads = 'B') then
                   PValues[I] := -1
                 else
                   PValues[I] := 1;
          end;
          CX := RoadCar(Cars, N, Opposite);                                                        // get number of car on opposite road
          if ((Cars[CX].Direction = 'S') or (Cars[CX].Direction = 'R')) and ((Cars[I].Direction = 'L')) then
            // Adapt priority value if car turns left and car on opposite road don't
            PValues[I] -= 0.5;
        end;
      end;
    end;
    // Determine answer field for each car
    PX := 0; OldMax := 0;
    for I := 1 to N do begin
      // For number of cars at the crossing, determine priority values in descending order and place corresponding car onto next answer field
      Max := PValues[1]; CX := 1;
      for J := 1 to N do begin
        if PValues[J] > Max then begin
          Max := PValues[J]; CX := J;                                                              // car number corresponding to actual maximum priority value
        end;
      end;
      if Max = OldMax then
        // Same priority value as before: the 2 cars may move at the same time
        Priorities[PX + 4] := CX
      else begin
        // Priority value is less than before: place car on "next vertical" answer field
        Inc(PX);
        Priorities[PX] := CX;
        OldMax := PValues[CX];
      end;
      PValues[CX] := -5;                                                                           // set to -5 in order to remove value from array
    end;
  end;
  // Re-arrange cars at same "horizontal" answer fields
  for I := 1 to 3 do begin
    if (Priorities[I + 4] <> 0) and (Priorities[I + 4] < Priorities[I]) then begin
      Temp := Priorities[I]; Priorities[I] := Priorities[I + 4]; Priorities[I + 4] := Temp;
    end;
  end;
end;

{ Place car on activated answer field }

procedure AnswerFieldPlaceCar(var NDone: Integer; var CarsDone: TBArray4; N: Integer; Cars: TCars; Road: Char; var PX: Integer; var Priorities: TIArray7);

var
  CX: Integer;
  FName: string;

begin
  // Do only if answering is enabled and there is an answer field activated
  if fPriorite.btAnswer.Enabled and (PX > 0) then begin
    CX := RoadCar(Cars, N, Road);                                                                  // get number of car on actual road
    FName := './pics/Car' + IntToStr(CX) + '_l.jpg';
    DoDirSeparators(FName);
    if not CarsDone[CX] then begin
      // Do only if this car hasn't yet been placed in answer field
      imPriorities[PX].Picture.LoadFromFile(FName);                                                // display car picture in active answer field
      Priorities[PX] := CX;                                                                        // save car number in user answers priority array
      CarsDone[CX] := True; Inc(NDone);                                                            // mark the car as placed in answer field
      PX := -1;                                                                                    // field index = -1 to indicate no fields active
    end;
  end;
end;

{ Remove car from given answer field }

procedure AnswerFieldRemoveCar(var NDone: Integer; var CarsDone: TBArray4; P: Integer; var PX: Integer; var Priorities: TIArray7);

var
  FName: string;

begin
  // Do only if there is a car in the field
  if Priorities[P] <> 0 then begin
    FName := './pics/nextcar.jpg';
    DoDirSeparators(FName);
    imPriorities[P].Picture.LoadFromFile(FName);                                                   // overwrite car picture by activated empty field
    if (PX > 0) and (Priorities[PX] = 0) then begin                                                // if previous field wasn't a car, deactivate it
      FName := './pics/nocar.jpg';
      DoDirSeparators(FName);
      imPriorities[PX].Picture.LoadFromFile(FName);
    end;
    CarsDone[Priorities[P]] := False; Dec(NDone);                                                  // mark the car removed as not yet placed in answer field
    Priorities[P] := 0;                                                                            // update user answers priority array (0 = no car)
    PX := P;                                                                                       // new index of activated answer field
  end;
end;

{ Activate given answer field }

procedure AnswerFieldActivate(P: Integer; var PX: Integer; Priorities: TIArray7);

var
  FName: string;

begin
  // Do only if there isn't a car
  if Priorities[P] = 0 then begin
    if (PX > 0) and (Priorities[PX] = 0) then begin                                                // if previous field was active field, deactivate it
      FName := './pics/nocar.jpg';
      DoDirSeparators(FName);
      imPriorities[PX].Picture.LoadFromFile(FName);
    end;
    PX := P;                                                                                       // new index of activated answer field
    FName := './pics/nextcar.jpg';
    DoDirSeparators(FName);
    imPriorities[PX].Picture.LoadFromFile(FName);                                                  // activate the field you wanted to do so
  end;
end;

{ Display test result counters }

procedure DisplayCounters(Total, Correct, False: Integer);

var
  P: Real;

begin
  P := 100 * (Correct / Total);
  P := Int(100 * P) / 100;                                                                         // success percentage with 2 decimals
  fPriorite.edQuestions.Text := IntToStr(Total);
  fPriorite.edCorrect.Text := IntToStr(Correct);
  fPriorite.edFalse.Text := IntToStr(False);
  fPriorite.edSuccess.Text := FloatToStr(P) + '%';
  if P < 50 then
    fPriorite.edSuccess.Color := clRed
  else if P < 60 then
    fPriorite.edSuccess.Color := clYellow
  else
    fPriorite.edSuccess.Color := clLime;
end;

{ Display end of test (final evaluation) message }

procedure DisplayEndOfTest(Questions, Correct: Integer);

var
  P: Real;
  S: string;

begin
  P := 100 * (Correct / Questions);
  if P < 25 then
    S := 'Plutôt décevant! Gros efforts à faire!'
  else if P < 50 then
    S := 'Insuffisant! Efforts à faire!'
  else if P < 75 then
    S := 'Suffisant! Mais il y a moyen de faire mieux!'
  else if P < 90 then
    S := 'Bien! Toutes les chances pour réussir l''examen!'
  else
    S := 'Excellent! Un vrai champion!';
  MessageDlg('Fin du test', S, mtInformation, [mbOK], 0);
end;

{ Determine relative road positions for given car }

procedure CarRoads(var Car: TCar);

begin
  with Car do begin
    case Road of
      'L': begin
             Opposite := 'R'; Right := 'B'; Left := 'T';
           end;
      'R': begin
             Opposite := 'L'; Right := 'T'; Left := 'B';
           end;
      'T': begin
             Opposite := 'B'; Right := 'L'; Left := 'R';
           end;
      'B': begin
             Opposite := 'T'; Right := 'R'; Left := 'L';
           end;
    end;
  end;
end;

{ Determine number of car present on a given road }

function RoadCar(Cars: TCars; N: Integer; Road: Char): Integer;

var
  CX, I: Integer;

begin
  CX := 0;
  for I := 1 to N do
    if Cars[I].Road = Road then
      CX := I;
  RoadCar := CX;
end;

{ Check if a car move is valid (at a 3 roads crossing) }

function ValidMove(Car: TCar; NoRoad: Char): Boolean;

var
  Valid: Boolean;

begin
  Valid := True;
  with Car do begin
    if ((Direction = 'L') and (Left = NoRoad)) or
       ((Direction = 'R') and (Right = NoRoad)) or
       ((Direction = 'S') and (Opposite = NoRoad)) then
      Valid := False;
  end;
  ValidMove := Valid;
end;

{**************}
{* TfPriorite *}
{**************}

{ Application start: Initialisation }

procedure TfPriorite.FormCreate(Sender: TObject);

begin
  // Store answer fields into an array
  imPriorities[1] := imPriority1a; imPriorities[2] := imPriority2a; imPriorities[3] := imPriority3a; imPriorities[4] := imPriority4;
  imPriorities[5] := imPriority1b; imPriorities[6] := imPriority2b; imPriorities[7] := imPriority3b;
  // Variable initialisation
  iQuestions := 20;
  stQuestion.Caption := '';
  // Reset form objects
  ResetForm;
  // Start random generator
  Randomize;
end;

{ Menu item "Test > Nouveau": Start new test }

procedure TfPriorite.mTestNewClick(Sender: TObject);

begin
  // Clear variables
  iQuestion := 0; iCorrect := 0; iFalse := 0; bNoAnswer := False;
  fPriorite.edQuestions.Text := ''; fPriorite.edCorrect.Text := ''; fPriorite.edFalse.Text := '';
  fPriorite.edSuccess.Text := ''; fPriorite.edSuccess.Color := clDefault;
  // Disable "Options" menu
  mOptions.Enabled := False;
  // Enable "Question" button, disable "Answer" button
  btQuestion.Enabled := True; btAnswer.Enabled := False;
  // Generate 1st question (by simulating click on "Question" button
  btQuestion.Click;
end;

{ Menu item "Test > Quitter": Exit the application }

procedure TfPriorite.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Nombre de questions = n": Get number of test questions from user }

procedure TfPriorite.mOptionsNQuestionsClick(Sender: TObject);

begin
  fData.ShowModal;                                                                                 // display data entry form
  iQuestions := StrToInt(fData.edNQuestions.Text);                                                 // read number of questions from this form
  mOptionsNQuestions.Caption := 'Questions = ' + fData.edNQuestions.Text;
end;

{ Menu item "Aide > Info": Display program "about" text (using a popup notifier) }

procedure TfPriorite.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if popupAbout.Visible then
    popupAbout.Hide
  else begin
    S := 'Code de la route: Priorités.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, Mars 2018';
    popupAbout.Text := S;
    popupAbout.Show;
  end;
end;

{ Button "Question": Generate test question and display corresponding crossing }

procedure TfPriorite.btQuestionClick(Sender: TObject);

var
  I: Integer;
  FName: string;

begin
  if bNoAnswer then begin
    // There was a non-answered question before (consider as false answer)
    Inc(iFalse);
    DisplayCounters(iQuestion, iCorrect, iFalse);
  end;
  // Still test questions left
  if iQuestion < iQuestions then begin
    // Reset variables
    ResetForm;
    Inc(iQuestion);
    iCarsDone := 0;
    for I := 1 to 4 do
      abCarsDone[I] := False;
    for I := 1 to 7 do begin
      aiPriorities[I] := 0;
      aiUserPriorities[I] := 0;
    end;
    // Generate test question and display corresponding crossing
    QuestionGenerate(iQuestions, iQuestion, iCars, Cars);
    // Activate first answer field
    FName := './pics/nextcar.jpg';
    DoDirSeparators(FName);
    iPX := 1; imPriority1a.Picture.LoadFromFile(FName);
    // Note that question hasn't yet been answered
    bNoAnswer := True;
  end
  // All questions done
  else begin
    // Display end of test message
    DisplayEndOfTest(iQuestions, iCorrect);
    // Disable both buttons
    btQuestion.Enabled := False; btAnswer.Enabled := False;
    // Enable Options menu item
    mOptions.Enabled := True;
  end;
end;

{ Button "Answer": Check user answer and update evaluation counters }

procedure TfPriorite.btAnswerClick(Sender: TObject);

var
  I, Temp: Integer;
  OK: Boolean;

begin
  // User has placed all cars for actual test question
  if iCarsDone = iCars then begin
    // Rearrange cars on answer fields
    for I := 1 to 3 do begin
      if aiUserPriorities[I + 4] <> 0 then begin
        if aiUserPriorities[I] = 0 then begin
          aiUserPriorities[I] := aiUserPriorities[I + 4];
          aiUserPriorities[I + 4] := 0;
        end
        else if aiUserPriorities[I + 4] < aiUserPriorities[I] then begin
          Temp := aiUserPriorities[I]; aiUserPriorities[I] := aiUserPriorities[I + 4]; aiUserPriorities[I + 4] := Temp;
        end;
      end;
    end;
    // Calculate priorities for actual situation
    CalculatePriorities(Cars, iCars, cStopRoads, cNoCar, cParking, aiPriorities);
    // Check user answer
    OK := True;
    for I := 1 to 7 do begin
      if aiPriorities[I] <> aiUserPriorities[I] then
        OK := False;
    end;
    // All cars placed correctly
    if OK then begin
      Inc(iCorrect);
      edEval.Text := 'Bonne réponse!';
      edEval.Color := clLime;
    end
    // At least one car at wrong position
    else begin
      Inc(iFalse);
      edEval.Text := 'Mauvaise réponse!';
      edEval.Color := clRed;
    end;
    // Display evaluation counters
    DisplayCounters(iQuestion, iCorrect, iFalse);
    // Disable "Answer" button
    btAnswer.Enabled := False;
    // All questions done now
    if iQuestion = iQuestions then begin
      // Display end of test message
      DisplayEndOfTest(iQuestions, iCorrect);
      // Disable both buttons
      btQuestion.Enabled := False; btAnswer.Enabled := False;
      // Enable Options menu item
      mOptions.Enabled := True;
    end;
  end
  // User hasn't placed all cars for actual test question
  else
    MessageDlg('Réponse invalide', 'Il faut placer toutes les voitures!', mtError, [mbOK], 0);
  // Note that question has been answered
  bNoAnswer := False;
end;

{ Car on "left" road double-clicked }

procedure TfPriorite.imCarLeftDblClick(Sender: TObject);

begin
  AnswerFieldPlaceCar(iCarsDone, abCarsDone, iCars, Cars, 'L', iPX, aiUserPriorities);
end;

{ Car on "right" road double-clicked }

procedure TfPriorite.imCarRightDblClick(Sender: TObject);

begin
  AnswerFieldPlaceCar(iCarsDone, abCarsDone, iCars, Cars, 'R', iPX, aiUserPriorities);
end;

{ Car on "top" road double-clicked }

procedure TfPriorite.imCarTopDblClick(Sender: TObject);

begin
  AnswerFieldPlaceCar(iCarsDone, abCarsDone, iCars, Cars, 'T', iPX, aiUserPriorities);
end;

{ Car on "bottom" road double-clicked }

procedure TfPriorite.imCarBottomDblClick(Sender: TObject);

begin
  AnswerFieldPlaceCar(iCarsDone, abCarsDone, iCars, Cars, 'B', iPX, aiUserPriorities);
end;

{ Answer fields priority 1 clicked }

procedure TfPriorite.imPriority1aClick(Sender: TObject);

begin
  AnswerFieldActivate(1, iPX, aiUserPriorities);
end;

procedure TfPriorite.imPriority1bClick(Sender: TObject);

begin
  AnswerFieldActivate(5, iPX, aiUserPriorities);
end;

{ Answer fields priority 2 clicked }

procedure TfPriorite.imPriority2aClick(Sender: TObject);

begin
  AnswerFieldActivate(2, iPX, aiUserPriorities);
end;

procedure TfPriorite.imPriority2bClick(Sender: TObject);

begin
  AnswerFieldActivate(6, iPX, aiUserPriorities);
end;

{ Answer fields priority 3 clicked }

procedure TfPriorite.imPriority3aClick(Sender: TObject);

begin
  AnswerFieldActivate(3, iPX, aiUserPriorities);
end;

procedure TfPriorite.imPriority3bClick(Sender: TObject);

begin
  AnswerFieldActivate(7, iPX, aiUserPriorities);
end;

{ Answer field priority 4 clicked }

procedure TfPriorite.imPriority4Click(Sender: TObject);

begin
  AnswerFieldActivate(4, iPX, aiUserPriorities);
end;

{ Answer fields priority 1 double-clicked }

procedure TfPriorite.imPriority1aDblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 1, iPX, aiUserPriorities);
end;

procedure TfPriorite.imPriority1bDblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 5, iPX, aiUserPriorities);
end;

{ Answer fields priority 2 double-clicked }

procedure TfPriorite.imPriority2aDblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 2, iPX, aiUserPriorities);
end;

procedure TfPriorite.imPriority2bDblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 6, iPX, aiUserPriorities);
end;

{ Answer fields priority 3 double-clicked }

procedure TfPriorite.imPriority3aDblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 3, iPX, aiUserPriorities);
end;

procedure TfPriorite.imPriority3bDblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 7, iPX, aiUserPriorities);
end;

{ Answer field priority 4 double-clicked }

procedure TfPriorite.imPriority4DblClick(Sender: TObject);

begin
  AnswerFieldRemoveCar(iCarsDone, abCarsDone, 4, iPX, aiUserPriorities);
end;

end.

