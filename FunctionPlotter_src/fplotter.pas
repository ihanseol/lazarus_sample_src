{*********************************************}
{* Main unit for FunctionPlotter application *}
{*********************************************}

unit fplotter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, Process, graph, help;

type
  TArray = array of string;
  {*******************}
  { TfFunctionPlotter }
  {*******************}
  TfFunctionPlotter = class(TForm)
    cobFunctions: TComboBox;
    edColor2: TEdit;
    edFunction2: TEdit;
    edLabel2: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    mMenu: TMainMenu;
    mFunction, mFunctionExit, mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10, Label11: TLabel;
    edTitle, edFunction, edLabel, edXLabel, edYLabel: TEdit;
    edXRange, edYRange, edColor, edWidth: TEdit;
    cobLegend: TComboBox;
    cbXAxis, cbYAxis, cbGrid: TCheckBox;
    edGnuplot: TMemo;
    btDraw: TButton;
    btReset: TButton;
    btSave: TButton;
    dlgSave: TSaveDialog;
    procedure cobFunctionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mFunctionExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
  private
    sDir: string;
    aCommands: TArray;
  end;

var
  fFunctionPlotter: TfFunctionPlotter;

implementation

{$R *.lfm}

{*******************}
{ TfFunctionPlotter }
{*******************}

{ Application start: Initialization }

procedure TfFunctionPlotter.FormCreate(Sender: TObject);

begin
  sDir := GetCurrentDir;                                                       // directory to store PLT files
end;

{ Menu item "Function > Exit": Exit application }

procedure TfFunctionPlotter.mFunctionExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display application help }

procedure TfFunctionPlotter.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfFunctionPlotter.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Function plotter:' + LineEnding;
  S += 'Draw math and other functions using Gnuplot.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, August-December 2024.';
  MessageDlg('About "FunctionPlotter"', S, mtInformation, [mbOK], 0);
end;

{ Button "Draw" pushed: Draw the function (= call Gnuplot to generate the PNG file) }

procedure TfFunctionPlotter.btDrawClick(Sender: TObject);

var
  N, I, P, Code: Integer;
  R, W: Real;
  Func, Func2, Plot, Range, Labl, Labl2, LWidth, Mess, S: string;
  OK: Boolean;

begin
  fGraph.bGraphOK := False;
  Labl := ''; Labl2 := ''; LWidth := ''; Mess := '';
  // Check user input data
  if edFunction.Text = '' then begin
    // The function is mandatory (of course)
    Mess := 'Missing function definition';
    edFunction.SetFocus;
  end
  else if (fFunctionPlotter.cobFunctions.ItemIndex = 1) and (edFunction2.Text = '') then begin
    // The second function is mandatory if "two functions plot" is selected
    Mess := 'Missing function definition';
    edFunction2.SetFocus;
  end
  else begin
    Func := edFunction.Text;
    if fFunctionPlotter.cobFunctions.ItemIndex = 1 then
      Func2 := edFunction2.Text;
    N := 0; SetLength(aCommands, N);
    // Main plot title
    if edTitle.Text <> '' then begin
      Inc(N); SetLength(aCommands, N);
      aCommands[N - 1] := 'set title "' + edTitle.Text + '"';
    end;
    // Function label (on legend)
    if edLabel.Text = '' then
      edLabel.Text := '<function>';
    if edLabel.Text <> '<function>' then
      Labl := edLabel.Text;
    if fFunctionPlotter.cobFunctions.ItemIndex = 1 then begin
      if edLabel2.Text = '' then
      edLabel2.Text := '<function>';
    if edLabel2.Text <> '<function>' then
      Labl2 := edLabel2.Text;
    end;
    // Graph function line colors
    if edColor.Text = '' then
      edColor.Text := '<default color>';
    if edColor.Text <> '<default color>' then begin
      // RGB color values are checked for validity; color names are not...
      if LeftStr(edColor.Text, 1) = '#' then begin
        if Length(edColor.Text) = 7 then begin
          for I := 2 to 7 do begin
            if not (Copy(edColor.Text, I, 1)[1] in ['0'..'9', 'A'..'F']) then begin
              Mess := 'Invalid RGB color code';
              edColor.SetFocus;
            end;
          end;
        end
        else begin
          Mess := 'Incomplete RGB color code';
          edColor.SetFocus;
        end;
      end;
    end;
    if Mess = '' then begin
      if fFunctionPlotter.cobFunctions.ItemIndex = 1 then begin
        if edColor2.Text = '' then
          edColor2.Text := '<default color>';
        if edColor2.Text <> '<default color>' then begin
          if LeftStr(edColor2.Text, 1) = '#' then begin
            if Length(edColor2.Text) = 7 then begin
              for I := 2 to 7 do begin
                if not (Copy(edColor2.Text, I, 1)[1] in ['0'..'9', 'A'..'F']) then begin
                  Mess := 'Invalid RGB color code';
                  edColor2.SetFocus;
                end;
              end;
            end
            else begin
              Mess := 'Incomplete RGB color code';
              edColor2.SetFocus;
            end;
          end;
        end;
      end;
    end;
    // X-axis values range
    if Mess = '' then begin
      if edXRange.Text = '' then
        edXRange.Text := '-10 ; 10';
      Range := StringReplace(edXRange.Text, ' ', '', [rfReplaceAll]);
      Range := StringReplace(Range, ';', ':', []);
      if Range <> '-10:10' then begin
        // Range must be [a ; b] or [a : b], with a and b numeric
        P := Pos(':', Range);
        if (P < 2) or (P = Length(Range)) then begin
          Mess := 'Invalid x-values range';
          edXRange.SetFocus;
        end
        else begin
          Val(LeftStr(Range, P - 1), R, Code);
          if Code = 0 then
            Val(Copy(Range, P + 1, Length(Range)), R, Code);
          if Code = 0 then begin
            Inc(N); SetLength(aCommands, N);
            Range := StringReplace(Range, ':', ' : ', []);
            aCommands[N - 1] := 'set xrange [' + Range + ']';
          end
          else begin
            Mess := 'Non numeric x-values range ';
            edXRange.SetFocus;
          end;
        end;
      end;
    end;
    if Mess = '' then begin
      // Y-axis values range
      if edYRange.Text = '' then
        edYRange.Text := '<min> ; <max>';
      Range := StringReplace(edYRange.Text, ' ', '', [rfReplaceAll]);
      Range := StringReplace(Range, ';', ':', []);
      if Range <> '<min>:<max>' then begin
        // Range must be [a ; b] or [a : b], with a and b numeric
        P := Pos(':', Range);
        if (P < 2) or (P = Length(Range)) then begin
          Mess := 'Invalid y-values range';
          edYRange.SetFocus;
        end
        else begin
          Val(LeftStr(Range, P - 1), R, Code);
          if Code = 0 then
            Val(Copy(Range, P + 1, Length(Range)), R, Code);
          if Code = 0 then begin
            Inc(N); SetLength(aCommands, N);
            Range := StringReplace(Range, ':', ' : ', []);
            aCommands[N - 1] := 'set yrange [' + Range + ']';
          end
          else begin
            Mess := 'Non numeric y-values range';
            edYRange.SetFocus;
          end;
        end;
      end;
    end;
    if Mess = '' then begin
      // X-axis label
      if edXLabel.Text <> '' then begin
        Inc(N); SetLength(aCommands, N);
        aCommands[N - 1] := 'set xlabel "' + edXLabel.Text + '"';
      end;
      // Y-axis label
      if edYLabel.Text <> '' then begin
        Inc(N); SetLength(aCommands, N);
        aCommands[N - 1] := 'set ylabel "' + edYLabel.Text + '"';
      end;
      // Graph function line width
      if edWidth.Text = '' then
        edWidth.Text := '1';
      if edWidth.Text <> '1' then begin
        // Width must be numeric
        Val(edWidth.Text, W, Code);
        if Code = 0 then
          //Plot += ' lw ' + edWidth.Text
          LWidth := ' lw ' + edWidth.Text
        else begin
          Mess := 'Non numeric line width';
          edWidth.SetFocus;
        end;
      end;
      // Legend position
      if Mess = '' then begin
        if cobLegend.ItemIndex <> 2 then begin
          Inc(N); SetLength(aCommands, N);
          aCommands[N - 1] := 'set key ' + cobLegend.Text;
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    // X-zeroaxis
    if cbXAxis.Checked then begin
      Inc(N); SetLength(aCommands, N);
      aCommands[N - 1] := 'set xzeroaxis';
    end;
    // Y-zeroaxis
    if cbYAxis.Checked then begin
      Inc(N); SetLength(aCommands, N);
      aCommands[N - 1] := 'set yzeroaxis';
    end;
    // Grid
    if cbGrid.Checked then begin
      Inc(N); SetLength(aCommands, N);
      aCommands[N - 1] := 'set grid';
    end;
    Plot := 'plot ' + Func;
    if Labl <> '' then
      Plot += ' t "' + Labl + '"';
    if edColor.Text <> '<default color>' then
      Plot += ' lc rgb "' + edColor.Text + '"';
    if LWidth <> '' then
      Plot += LWidth;
    if fFunctionPlotter.cobFunctions.ItemIndex = 1 then begin
      Plot += ', \' + LineEnding;
      Plot += '     ' + Func2;
      if Labl2 <> '' then
        Plot += ' t "' + Labl2 + '"';
      if edColor2.Text <> '<default color>' then
        Plot += ' lc rgb "' + edColor2.Text + '"';
      if LWidth <> '' then
        Plot += LWidth;
    end;
  end;
  edGnuplot.Lines.Clear;
  if Mess = '' then begin
    // User input is ok: Graph can be generated
    Inc(N); SetLength(aCommands, N);
    // Gnuplot will create a PNG file
    aCommands[N - 1] := 'set terminal png size 1080,620';
    Inc(N); SetLength(aCommands, N);
    aCommands[N - 1] := 'set output "fplotter.png"';
    Inc(N); SetLength(aCommands, N);
    aCommands[N - 1] := Plot;
    // Display the Gnuplot commands in the TMemo field
    for N := 0 to Length(aCommands) - 1 do
      edGnuplot.Lines.Add(aCommands[N]);
    // Delete old files
    if FileExists('fplotter.plt') then
      DeleteFile('fplotter.plt');
    if FileExists('fplotter.png') then
      DeleteFile('fplotter.png');
    // Create the input file for Gnuplot
    edGnuplot.Lines.SaveToFile('fplotter.plt');
    // Run Gnuplot (using the commands of the PLT file created)
    OK := RunCommand('gnuplot', ['fplotter.plt'], S);
    // If Gnuplot terminated successfully, open the Graph window
    if OK then begin
      fGraph.sPLTDir := sDir;
      fGraph.bGraphOK := True;                                                 // the "graph" unit loads the PNG file only is this variable is set to True
      fGraph.Show
    end
    // Error message if Gnuplot failed
    else
      MessageDlg('Application error', 'Running gnuplot failed!', mtError, [mbOK], 0);
  end
  // Error message if invalid user input data
  else
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
end;

{ Button "Reset" pushed: Reset all graph properties and parameters to default values }

procedure TfFunctionPlotter.btResetClick(Sender: TObject);

begin
  edTitle.Text := ''; edFunction.Text := ''; edFunction2.Text := '';
  edLabel.Text := '<function>'; edLabel2.Text := '<function>';
  edColor.Text := '<default color>'; edColor2.Text := '<default color>';  
  edXLabel.Text := ''; edYLabel.Text := '';
  edXRange.Text:= '-10 ; 10'; edYRange.Text:= '<min> ; <max>';
  edWidth.Text := '1'; cobLegend.ItemIndex := 2;
  cbXAxis.Checked := False; cbYAxis.Checked := False; cbGrid.Checked := False;
  edGnuplot.Lines.Clear;
end;

{ Button "Save" pushed: Save the Gnuplot commnads to PLT file }

procedure TfFunctionPlotter.btSaveClick(Sender: TObject);

var
  Ret: Cardinal;
  Filename: string;
  DoOverride: Boolean;

begin
  dlgSave.InitialDir := sDir;
  dlgSave.FileName := '';
  if dlgSave.Execute then begin
    // User has selected a file
    Filename := dlgSave.FileName;
    sDir := ExtractFileDir(Filename);                                          // save directory (to open next time at same location)
    // Override existing file only, if user wants so
    DoOverride := True;
    if FileExists(Filename) then begin
      Ret := MessageDlg('Save Gnuplot commands', 'File already exists. Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
      if Ret = mrNo then
        DoOverride := False;
    end;
    // Save Gnuplot commands to file
    if DoOverride then
      edGnuplot.Lines.SaveToFile(Filename);
  end;
end;

procedure TfFunctionPlotter.cobFunctionsChange(Sender: TObject);

begin
  if cobFunctions.ItemIndex = 0 then begin
    edFunction2.Text := ''; edLabel2.Text := '<function>'; edColor2.Text := '<default color>';
    edFunction2.Enabled := False; edLabel2.Enabled := False; edColor2.Enabled := False;
  end
  else begin
    edFunction2.Enabled := True; edLabel2.Enabled := True; edColor2.Enabled := True;
  end;
end;

end.

