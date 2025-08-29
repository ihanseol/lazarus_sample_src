{************************************************************}
{* MP3 file/tag info display unit for SearchMP3 application *}
{************************************************************}

unit mp3files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLIntf;

const
  FilesFoundMax = 2500;                                                        // arbitrary limit of MP3 file number in search result (memo)
  TempFileName = 'SearchMP3.temp';

type
  TFilesFound = array[1..FilesFoundMax] of Integer;
  { TfFilesMP3 }
  TfFilesMP3 = class(TForm)
    memoFiles: TMemo;
    btSave: TButton;
    btBrowse: TButton;
    btPlay: TButton;
    btClose: TButton;
    memoHelp: TMemo;
    dlgSave: TSaveDialog;
    procedure btSaveClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btPlayClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    iSelFile, iFile: Integer;
    sMP3File: string;
  public
    // Variables set by "fSearchMP3" form
    aFilesFound: ^TFilesFound;                                                 // pointer to "files found" array
    iFilesFound: Integer;                                                      // number of files found
  end;

var
  fFilesMP3: TfFilesMP3;
  TempFile: Text;                                                              // text file containing (all) MP3 file paths

// File access routines (also used by "fSearchMP3" form)

procedure OpenFile(var TFile: Text; var TFileEnd: Boolean);
procedure CloseFile(var TFile: Text; var TFileEnd: Boolean);
procedure ReadFile(var TFile: Text; var TFileLine: string; var TFileEnd: Boolean);

{---------------------------------------------------------------------------------------------------------------------------------}

implementation

{$R *.lfm}

{ Open text file }

procedure OpenFile(var TFile: Text; var TFileEnd: Boolean);

begin
  Assign(TFile, TempFileName); Reset(TFile);
  TFileEnd := False;
end;

{ Close text file }

procedure CloseFile(var TFile: Text; var TFileEnd: Boolean);

begin
  Close(TFile);
  TFileEnd := True;
end;

{ Read line from text file (at current position) }

procedure ReadFile(var TFile: Text; var TFileLine: string; var TFileEnd: Boolean);

begin
  TFileEnd := False;
  if not EoF(TFile) then
    Readln(TFile, TFileLine)
  else
    TFileEnd := True;
end;

{ Get found-files-index (from selected text in files list) }

function GetFoundFilesIndex(FilesFound: Integer): Integer;

var
  I, SelFile: Integer;
  SelTxt: string;

begin
  SelTxt := fFilesMP3.memoFiles.SelText;                                       // text selected by user
  SelFile := -1; I := 1;
  repeat                                                                       // parse file list (memo lines) ...
    if fFilesMP3.memoFiles.Lines[I - 1].Contains(SelTxt) then                  // ... until it contains the selected text
      SelFile := I;
    Inc(I);
  until (I > FilesFound) or (SelFile <> -1);
  GetFoundFilesIndex := SelFile;
end;

{ Read file path (corresponding to user selection) from temporary text file }

procedure ReadAllFileLine(var TempFile: Text; var MP3File: string; iFile: Integer);

// Line number in the file = all-files-index (determined from found-files-index)

var
  I: Integer;
  EndFile: Boolean;

begin
  MP3File := '';
  OpenFile(TempFile, EndFile);
  if not EndFile then begin
    for I := 1 to iFile do begin
      if not EndFile then
        ReadFile(TempFile, MP3File, EndFile);
    end;
  end;
  CloseFile(TempFile, EndFile);
end;

{**************}
{* TfFilesMP3 *}
{**************}

{ Button "Save info": Save MP3 file info to text file }

procedure TfFilesMP3.btSaveClick(Sender: TObject);

var
  Dir, FileName: string;

begin
  // Default save location = user's Documents directory
  Dir := GetUserDir + 'Documents';
  if DirectoryExists(Dir) then
    Dir += '\'
  else
    Dir := '';
  dlgSave.InitialDir := Dir;
  // Save the memo content to text file
  if dlgSave.Execute then begin
    FileName := dlgSave.Filename;
    memoFiles.Lines.SaveToFile(FileName);
  end;
end;

{ Button "Open directory": Open file containing directory }

procedure TfFilesMP3.btBrowseClick(Sender: TObject);

var
  MP3Path: string;

begin
  iSelFile := GetFoundFilesIndex(iFilesFound);
  // If the user has selected a file
  if iSelFile <> -1 then begin
    iFile := aFilesFound^[iSelFile];
    ReadAllFileLine(TempFile, sMP3File, iFile);                                // full path file name
    MP3Path := ExtractFilePath(sMP3File);                                      // full path directory name
    OpenDocument(MP3Path);                                                     // open directory (using Windows file explorer)
  end
  // No file has been selected
  else
    MessageDlg('Invalid selection', 'You must select a file before using this button!', mtError, [mbOK], 0);
end;

{ Button "Play file": Play MP3 file delected }

procedure TfFilesMP3.btPlayClick(Sender: TObject);

begin
  iSelFile := GetFoundFilesIndex(iFilesFound);
  if iSelFile <> -1 then begin
    iFile := aFilesFound^[iSelFile];
    ReadAllFileLine(TempFile, sMP3File, iFile);                                // full path file name
    OpenDocument(sMP3File);                                                    // play MP3 file (using Windows default audio player)
  end
  else
    MessageDlg('Invalid selection', 'You must select a file before using this button!', mtError, [mbOK], 0);
end;

{ Button "Close": Close the search results window }

procedure TfFilesMP3.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

