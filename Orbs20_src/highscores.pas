{******************************************}
{* Highscores unit for Orbs20 application *}
{******************************************}

unit highscores;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

type
  {**************}
  { TfHighScores }
  {**************}
  TfHighScores = class(TForm)
    Label1, Label2, Label3: TLabel;
    sgHighScores: TStringGrid;
    edName: TEdit;
    btDelete: TButton;
    btReset: TButton;
    btClose: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  public
    bChanged: Boolean;
  end;

var
  fHighScores: TfHighScores;

implementation

{$R *.lfm}

{**************}
{ TfHighScores }
{**************}

{ Form show-up: Reset variables }

procedure TfHighScores.FormActivate(Sender: TObject);

begin
  edName.Text := '';
  bChanged := False;
end;

{ Button "Delete": Delete user from highscore table }

procedure TfHighScores.btDeleteClick(Sender: TObject);

var
  I: Integer;
  UName: string;
  Found: Boolean;

begin
  Found := False;
  UName := edName.Text;
  if UName <> '' then begin
    if UName = 'Orb' then
      // Keep default user "Orb"
      MessageDlg('Orbs20 highscores', 'You cannot delete the default user "Orb"!', mtError, [mbOK], 0)
    else begin
      // Check if name entered is in table
      for I := 1 to 25 do begin
        if UName = sgHighScores.Cells[0, I] then begin
          // Just clear entry in table (string grid)
          sgHighScores.Cells[0, I] := ''; sgHighScores.Cells[1, I] := '';
          Found := True;
        end;
      end;
      if not Found then
        // Name entered is not in table
        MessageDlg('Orbs20 highscores', 'Invalid user name "' + UName + '"!', mtError, [mbOK], 0);
    end;
  end;
  // If user has been deleted, mark "highscore table changed"
  if not bChanged then
    bChanged := Found;
end;

{ Buttton "Reset": Reset highscore table }

procedure TfHighScores.btResetClick(Sender: TObject);

var
  I, J: Integer;

begin
  // Clear all table (string grid) entries
  for I := 0 to 1 do begin
    for J := 1 to 25 do begin
      sgHighScores.Cells[I, J] := '';
    end;
  end;
  // Add default user "Orb" (with highscore = 0)
  sgHighScores.Cells[0, 1] := 'Orb';
  sgHighScores.Cells[1, 1] := '     0';
  // Mark "highscore table changed"
  bChanged := True;
end;

{ Button "Close": Close the highscore window }

procedure TfHighScores.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Button "Cancel": Mark "highscore table NOT changed" and close the window }

procedure TfHighScores.btCancelClick(Sender: TObject);

begin
  bChanged := False;
  Close;
end;

end.

