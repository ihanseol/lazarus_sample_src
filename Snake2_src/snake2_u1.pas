{************************************************************************}
{* Main unit for Snake2 application (starting snake2_u2 to do the game) *}
{************************************************************************}

// Change log:
//   * Version 1.0 (Mai 2018):
//     Original application (based on the "snake" command line program from 2016)
//   * Version 2.0 (February 2020)
//       - Game extensions:
//         - Addition of the magenta meals
//         - Addition of the "hidden rocks" feature
//       - Addition of the "Level" menu item
//       - Some minor changes (about dialog box, game description font and text corrections...)

unit snake2_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, snake2_u2;

type
  {**********}
  { TfSnake2 }
  {**********}
  TfSnake2 = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mLevel, mLevel1, mLevel2, mLevel3: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2: TLabel;
    edLength, edSpeed: TEdit;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mLevel1Click(Sender: TObject);
    procedure mLevel2Click(Sender: TObject);
    procedure mLevel3Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure edLengthChange(Sender: TObject);
    procedure edSpeedChange(Sender: TObject);
  private
    iLevel, iLengthMax, iSpeed: Integer;
  end;

var
  fSnake2: TfSnake2;

implementation

{$R *.lfm}

{**********}
{ TfSnake2 }
{**********}

{ Application start: Initialisation }

procedure TfSnake2.FormCreate(Sender: TObject);

begin
  iLevel := 1; iLengthMax := 40; iSpeed := 5;
end;

{ Menu item "Game > New": Start a new Snake game }

procedure TfSnake2.mGameNewClick(Sender: TObject);

begin
  // Check snake's maximum length and speed
  if (iLengthMax < 10) or (iLengthMax > 40) then begin
    MessageDlg('Invalid settings', 'The snake''s maximal length must be between 10 and 40!', mtError, [mbOK], 0);
    edLength.SetFocus;
  end
  else if (iSpeed < 1) or (iSpeed > 10) then begin
    MessageDlg('Invalid settings', 'The snake''s speed must be between 1 and 10!', mtError, [mbOK], 0);
    edSpeed.SetFocus;
  end
  // If the user input is ok, start the game (by showing the fGame form)
  else begin
    fGame.iLevel := iLevel;
    fGame.iLengthMax := iLengthMax;
    fGame.iSpeed := iSpeed;
    fGame.ShowModal;
  end;
end;

{ Menu item "Game > Exit": Exit application }

procedure TfSnake2.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Level > ...": Select game level }

procedure TfSnake2.mLevel1Click(Sender: TObject);

begin
  mLevel1.Checked := True;
  mLevel2.Checked := False;
  mLevel3.Checked := False;
  iLevel := 1;
end;

procedure TfSnake2.mLevel2Click(Sender: TObject);

begin
  mLevel1.Checked := False;
  mLevel2.Checked := True;
  mLevel3.Checked := False;
  iLevel := 2;
end;

procedure TfSnake2.mLevel3Click(Sender: TObject);

begin
  mLevel1.Checked := False;
  mLevel2.Checked := False;
  mLevel3.Checked := True;
  iLevel := 3;
end;

{ Menu item "Help > About": Display Snake2 about }

procedure TfSnake2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Simple SNAKE game.' + LineEnding;
  S += '(Originally based on the "snake" command line program, © allu, 2016)' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, May 2018 - February 2020.';
  MessageDlg('About "Snake2"', S, mtInformation, [mbOK], 0);
end;

{ Read snake maximal length (if user changed it) }

procedure TfSnake2.edLengthChange(Sender: TObject);

begin
  if edLength.Text = '' then
    iLengthMax := 0
  else
    iLengthMax := StrToInt(edLength.Text);
end;

{ Read snake speed (if user changed it) }

procedure TfSnake2.edSpeedChange(Sender: TObject);

begin
  if edSpeed.Text = '' then
    iSpeed := 0
  else
    iSpeed := StrToInt(edSpeed.Text);
end;

end.

