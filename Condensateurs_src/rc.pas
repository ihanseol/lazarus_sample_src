{*******************************************}
{*    Electrical circuits:  RC circuits    *}
{*-----------------------------------------*}
{* Main unit for Condensateurs application *}
{*******************************************}

unit rc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Menus, PopupNotifier, help;

type
  { TFormRC }
  TFormRC = class(TForm)
    MainMenu: TMainMenu;
    MenuCalc: TMenuItem;
    MenuCalcU: TMenuItem;
    MenuCalcI: TMenuItem;
    MenuCalcBoth: TMenuItem;
    MenuCalcExit: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpPhysics: TMenuItem;
    MenuHelpProgram: TMenuItem;
    MenuHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    RadioButtonCharge: TRadioButton;
    RadioButtonDischarge: TRadioButton;
    Label1: TLabel;
    Voltage: TEdit;
    UnitVoltage: TComboBox;
    Label2: TLabel;
    Resistance: TEdit;
    UnitResistance: TComboBox;
    Label3: TLabel;
    Capacity: TEdit;
    UnitCapacity: TComboBox;
    Graph: TImage;
    Label4: TLabel;
    CVoltageMax: TEdit;
    Label5: TLabel;
    CCurrentMax: TEdit;
    Label6: TLabel;
    Tau: TEdit;
    ButtonCalc: TButton;
    PopupAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure MenuCalcIClick(Sender: TObject);
    procedure MenuCalcUClick(Sender: TObject);
    procedure MenuCalcBothClick(Sender: TObject);
    procedure MenuCalcExitClick(Sender: TObject);
    procedure MenuHelpPhysicsClick(Sender: TObject);
    procedure MenuHelpProgramClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure RadioButtonChargeChange(Sender: TObject);
    procedure RadioButtonDischargeChange(Sender: TObject);
    procedure ButtonCalcClick(Sender: TObject);
    private
      Bitmap : TBitmap;
      iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom: Integer;
      sAction, sValue: string;
  end;

var
  FormRC: TFormRC;

procedure FormClean(W, H: Integer);
procedure GraphClean(W, H: Integer);
procedure DrawAxis(XL, XR, YT, YB: Integer; LegendX, Legend1Y, Legend2Y: string);
procedure DrawGraphs(U0, I0, Tau: Real; XL, XR, YT, YB: Integer; UU, UI, UT: string);
function ConvertVoltage(U: Real; UUnit: string): string;
function ConvertCurrent(I: Real; IUnit: string): string;
function ConvertTime(T: Real; TUnit: string): string;
function Format(N: Real): string;

implementation

{$R *.lfm}

{ Clear the form }

procedure FormClean(W, H: Integer);

begin
  // Clean result values
  FormRC.CVoltageMax.Text := '';
  FormRC.CCurrentMax.Text := '';
  FormRC.Tau.Text := '';
  // Clean the drawing surface
  GraphClean(W, H);
end;

{ Clean the graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  FormRC.Graph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis }

procedure DrawAxis(XL, XR, YT, YB: Integer; LegendX, Legend1Y, Legend2Y: string);

var
  YAX, YLX: Integer;

begin
  FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  FormRC.Graph.Picture.Bitmap.Canvas.Pen.Width := 1;
  if (FormRC.sAction = 'Discharge') and (FormRC.sValue = 'i') then begin
    YAX := YT; YLX := YT - 20;                                                                     // values for X-axis above the graph
  end
  else begin
    YAX := YB; YLX := YB + 5;                                                                      // values for X-axis below the graph
  end;
  FormRC.Graph.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);                             // draw X-axis
  FormRC.Graph.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);                               // draw Y-axis
  FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlack;
  FormRC.Graph.Picture.Bitmap.Canvas.TextOut(XR - 5, YLX, LegendX);                                // display X-axis legend
  if Copy(Legend1Y, Length(Legend1Y) - 1, 1) = 'V' then
    FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clRed                                         // red for voltage
  else
    FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlue;                                       // blue for current
  FormRC.Graph.Picture.Bitmap.Canvas.TextOut(6, 10, Legend1Y);                                     // display Y-axis legend
  if Legend2Y <> '' then begin                                                                     // display 2nd Y-axis legend (if any)
    FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlue;
    FormRC.Graph.Picture.Bitmap.Canvas.TextOut(6, 23, Legend2Y);
  end;
end;

{ Draw the grap(s) }

procedure DrawGraphs(U0, I0, Tau: Real; XL, XR, YT, YB: Integer; UU, UI, UT: string);

  var
    TTotal: Real;
    Y0: Integer;

  { Calculate u(t)/i(t) and graph X and Y values for given time }

  procedure GraphXY(U0, I0, Tau, Total, T: Real; XL, XR, YT, YB: Integer; var V: Real; var X, Y: Integer);

  var
    V0: Real;

  begin
    // Formulas for capacitor charge
    if FormRC.sAction = 'charge' then begin
      if U0 <> 0 then begin
        // Voltage
        V := U0 * (1 - Exp(-T / Tau)); V0 := U0;
      end
      else begin
        // Current
        V := I0 * Exp(-T / Tau); V0 := I0;
      end;
    end
    // Formulas for capacitor discharge
    else begin
      if U0 <> 0 then begin
        // Voltage
        V := U0 * (Exp(-T / Tau)); V0 := U0;
      end
      else begin
        // Current
        V := -I0 * Exp(-T / Tau); V0 := I0;
      end;
    end;
    // X-value (time) on graph
    X := Round((T / Total) * (XR - XL)) + XL;
    // Y-value (voltage or current) on graph
    if (FormRC.sAction = 'Discharge') and (U0 = 0) then
      // Discharge current (negative value)
      Y := YT - Round((V / V0) * (YB - YT))
    else
      // All other cases (positive value)
      Y := YB - Round((V / V0) * (YB - YT));
  end;

  { Draw U/I the graph and display some important X/Y points }

  procedure DrawGraph(U0, I0, Tau, TTotal: Real; XL, XR, YT, YB: Integer);

  var
    T, V, Total: Real;
    X, Y, YL, J: Integer;

  begin
    Total := TTotal + Tau / 2;                                                                     // continuing graph a little bit beyond t = 5τ
    X := 0; Y := 0; V := 0;
    // Display for t = τ, t = 2τ and t = 3τ
    for J := 1 to 3 do begin
      if not ((FormRC.sValue = 'both') and (J = 3)) then begin                                     // do not display for t = 3τ if both curves are printed
        GraphXY(U0, I0, Tau, Total, J * Tau, XL, XR, YT, YB, V, X, Y);                             // calculate u(t)/i(t) and X and Y values for actual t
        if (FormRC.sAction = 'Discharge') and (U0 = 0) then begin                                  // values depending on position of X-axis
          Y0 := YT; YL := YT - 20;
        end
        else begin
          Y0 := YB; YL := YB + 5;
        end;
        // Display lines from curve to axises
        FormRC.Graph.Picture.Bitmap.Canvas.Pen.Width := 1;
        FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clSilver;
        FormRC.Graph.Picture.Bitmap.Canvas.Line(XL, Y, X, Y);                                      // draw line from curve to Y-axis
        FormRC.Graph.Picture.Bitmap.Canvas.Line(X, Y0, X, Y);                                      // draw line from curve to X-axis
        // Display markers on axises
        FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
        FormRC.Graph.Picture.Bitmap.Canvas.Line(X, Y0 - 5, X, Y0 + 5);                             // draw marker on X-axis
        FormRC.Graph.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);                             // draw marker on Y-axis
        // Add t value to legend
        FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlack;
        FormRC.Graph.Picture.Bitmap.Canvas.TextOut(X - 25, YL, ConvertTime(J * Tau, UT));
        // Add u(t)/i(t) value to legend
        if I0 = 0 then begin                                                                       // voltage value in red
          FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clRed;
          FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y - 7, ConvertVoltage(V, UU));
        end
        else begin                                                                                 // current value in blue
          FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlue;
          FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y - 7, ConvertCurrent(V, UI));
        end;
      end;
    end;
    // Display for t = 5τ (capacitor completely charged/discharged)
    // Note:
    //   Displaying both curves on the same graph was intially not planned
    //   By adding this feature later, the code of this part of the program
    //   has become illogical and difficult to read. Sorry...
    GraphXY(U0, I0, Tau, Total, TTotal, XL, XR, YT, YB, V, X, Y);                                  // calculate u(t)/i(t) and X and Y values for t = 5τ
    FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
    FormRC.Graph.Picture.Bitmap.Canvas.Line(X, Y0 - 5, X, Y0 + 5);                                 // display marker on X-axis
    FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlack;
    FormRC.Graph.Picture.Bitmap.Canvas.TextOut(X - 25, YL, ConvertTime(TTotal, UT));               // display t value
    // Voltage graph when charging the capacitor
    if (FormRC.sAction = 'charge') and (I0 = 0) then begin
      // Display lines from curve to axises
      FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clSilver;
      FormRC.Graph.Picture.Bitmap.Canvas.Line(XL, Y, X, Y);
      FormRC.Graph.Picture.Bitmap.Canvas.Line(X, Y0, X, Y);
      // Display markers on axises
      FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
      FormRC.Graph.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
      FormRC.Graph.Picture.Bitmap.Canvas.Line(X, Y0 - 5, X, Y0 + 5);
      // Display voltage value (if this is a 1-curve graph)
      if FormRC.sValue <> 'both' then begin
        FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clRed;
        FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y - 7, ConvertVoltage(V, UU));
      end;
    end;
    // Graphs that are not a voltage curve display during charge of capacitor
    if not ((FormRC.sAction = 'charge') and (I0 = 0)) then begin
      GraphXY(U0, I0, Tau, Total, 0, XL, XR, YT, YB, V, X, Y);                                     // calculate u(t)/i(t) and X and Y values for t = 0
      FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
      if FormRC.sValue <> 'both' then                                                              // draw marker on Y-axis
        FormRC.Graph.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
    end;
    // 2-curves graphs
    if FormRC.sValue = 'both' then begin
      // Voltage curve
      if I0 = 0 then begin
        FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clRed;
        FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y - 14, ConvertVoltage(U0, UU));             // display voltage value in red (above usual position)
      end
      // Current curve
      else begin
        FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlue;
        FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y + 2, ConvertCurrent(I0, UI));              // display current value in blue (below usual position)
      end;
    end
    // 1-curve graphs
    else if not ((FormRC.sAction = 'charge') and (I0 = 0)) then begin
      // Voltage curve
      if I0 = 0 then begin
        FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clRed;
        FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y - 7, ConvertVoltage(V, UU));               // display voltage value in red
      end
      // Current curve
      else begin
        FormRC.Graph.Picture.Bitmap.Canvas.Font.Color := clBlue;
        FormRC.Graph.Picture.Bitmap.Canvas.TextOut(5, Y - 7, ConvertCurrent(V, UI));               // display current value in blue
      end;
    end;
    // Draw the curve
    FormRC.Graph.Picture.Bitmap.Canvas.Pen.Width := 2;
    if U0 <> 0 then
      FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clRed                                        // voltage curve in red
    else
      FormRC.Graph.Picture.Bitmap.Canvas.Pen.Color := clBlue;                                      // current curve in blue
    // Graph X and Y values determination for t varying from 0 to total time chosen
    for J := 0 to (XR - XL) do begin                                                               // must use integers in FOR statement
      T := (J / (XR - XL)) * Total;                                                                // actual time t
      GraphXY(U0, I0, Tau, Total, T, XL, XR, YT, YB, V, X, Y);                                     // corresponding u(t)/i(t) and X and Y values on the graph
      if J = 0 then                                                                                // first point of the curve:
        FormRC.Graph.Picture.Bitmap.Canvas.MoveTo(X, Y)                                            // position the pen
      else                                                                                         // other points of the curve:
        FormRC.Graph.Picture.Bitmap.Canvas.LineTo(X, Y);                                           // draw line from previous point to here
    end;
  end;

begin
  TTotal := 5 * Tau;                                                                               // at t = 5τ, capacitor may be said completely charged/discharged
  if (FormRC.sValue = 'u') or (FormRC.sValue = 'both') then
    DrawGraph(U0, 0, Tau, TTotal, XL, XR, YT, YB);                                                 // draw the voltage curve u(t)
  if (FormRC.sValue = 'i') or (FormRC.sValue = 'both') then
    DrawGraph(0, I0, Tau, TTotal, XL, XR, YT, YB);                                                 // draw the current curve i(t)
end;

{ Convert voltage to formated string }

function ConvertVoltage(U: Real; UUnit: string): string;

begin
  if UUnit = 'mV' then
    U *= 1000;
  ConvertVoltage := Format(U);
end;

{ Convert current to formated string }

function ConvertCurrent(I: Real; IUnit: string): string;

begin
  if IUnit = 'µA' then
    I *= 1000 * 1000
  else if IUnit = 'mA' then
    I *= 1000;
  ConvertCurrent := Format(I);
end;

{ Convert time to formated string }

function ConvertTime(T: Real; TUnit: string): string;

begin
  if TUnit = 'µs' then
    T *= 1000 * 1000
  else if TUnit = 'ms' then
    T *= 1000;
  ConvertTime := Format(T);
end;

{ Format number as real with 3 decimal digits }

function Format(N: Real): string;

var
  NF, J: Integer;
  S: string;
  DS: Boolean;

begin
  N := Round(1000 * N) / 1000;
  S := FloatToStr(N);
  // Add 3 0s to integer number
  if N = Trunc(N) then
    S += ',000'
  // Add 0s if non integer number has less than 3 decimal digits
  else begin
    DS := False; NF := 0;
    for J := 1 to Length(S) do begin
      if Copy(S, J, 1) = ',' then                                                                  // find decimal point
        DS := True
      else
        begin
          if DS = True then                                                                        // count decimal digits
            Inc(NF);
        end;
    end;
    if NF < 3 then begin                                                                           // add 0s if number has less than 3 decimal digits
      for J := NF to 2 do
        S += '0';
    end;
  end;
  // Add leading spaces to format number as right-aligned (for nicer display on graph)
  if N > 0 then begin
    if N < 10 then
      S := '    ' + S
    else if N < 100 then
      S := '  ' + S;
  end
  else begin
    if -N < 10 then
      S := '   ' + S
    else if -N < 100 then
      S := ' ' + S;
  end;
  Format := S;
end;

{ TFormRC }

{ Application start: create a graphic area for use with canvas drawing }

procedure TFormRC.FormCreate(Sender: TObject);

begin
  // Graph area
  iImageWidth := Graph.Width; iImageHeight := Graph.Height;
  iGraphLeft  := 60;          iGraphRight  := iImageWidth - 40;
  iGraphTop   := 60;          iGraphBottom := iImageHeight - 40;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  Graph.Picture.Graphic := Bitmap;
  // Clear the form
  FormClean(iImageWidth, iImageHeight);
  // Initialize variables
  sAction := 'charge';                                                                             // action on capacitor: charge or discharge
  sValue := 'u';                                                                                   // curve(s) to draw: u, i or both
end;

{ Menu item "Calcul > Tension u(t)": Voltage curve }

procedure TFormRC.MenuCalcUClick(Sender: TObject);

begin
  sValue := 'u';
  ButtonCalc.Click;
end;

{ Menu item "Calcul > Courant i(t)": Current curve }

procedure TFormRC.MenuCalcIClick(Sender: TObject);

begin
  sValue := 'i';
  ButtonCalc.Click;
end;

{ Menu item "Calcul > u(i) et i(t)": Both curves }

procedure TFormRC.MenuCalcBothClick(Sender: TObject);

begin
  if sAction = 'charge' then begin                                                                 // do for capacitor charge only...
    sValue := 'both';
    ButtonCalc.Click;
  end
  else
    MessageDlg('Sélection inadéquate', 'Cette fonction n''est disponible que pour la charge!', mtError, [mbOK], 0);
end;

{ Menu item "Calcul > Quitter": Exit program }

procedure TFormRC.MenuCalcExitClick(Sender: TObject);

begin
  FormRC.Close;
end;

{ Menu item "Aide > Physique": Display physics help }

procedure TFormRC.MenuHelpPhysicsClick(Sender: TObject);

begin
  FormHelp.Title.Caption := 'Circuits RC - Lois de physique.';
  FormHelp.HelpText.Lines.LoadFromFile('help1.txt');                                               // load help text from file
  FormHelp.Show;                                                                                   // display help (as a form)
end;

{ Menu item "Aide > Programme": Display program help }

procedure TFormRC.MenuHelpProgramClick(Sender: TObject);

begin
  FormHelp.Title.Caption := 'Circuits RC - Aide programme.';
  FormHelp.HelpText.Lines.LoadFromFile('help2.txt');                                               // load help text from file
  FormHelp.Show;                                                                                   // display help (as a form)
end;

{ Menu item "Aide > Info": Display program info }

procedure TFormRC.MenuHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if PopupAbout.Visible then
    PopupAbout.Hide
  else begin
    S := 'Physique/électricité: Chargement/déchargement d''un condensateur.' + Chr(13) + Chr(13);
    S += '© allu, Janvier, 2018.';
    PopupAbout.Text := S;
    PopupAbout.Show;                                                                               // display info as message popup
  end;
end;

{ Action on capacitor chosen: charge }

procedure TFormRC.RadioButtonChargeChange(Sender: TObject);

begin
  if RadioButtonCharge.Checked then
    sAction := 'charge';
end;

{ Action on capacitor chosen: discharge }

procedure TFormRC.RadioButtonDischargeChange(Sender: TObject);

begin
  if RadioButtonDischarge.Checked then
    sAction := 'Discharge';
end;

{ Result computation and graph display }

procedure TFormRC.ButtonCalcClick(Sender: TObject);

var
  E, R, C, U0, I0, T, U0R, I0R, TR, MU, MI, MT: Real;
  UU, UI, UT, LegendX, Legend1Y, Legend2Y: string;

begin
  // Clear the form
  FormClean(iImageWidth, iImageHeight);
  // Read user values for E, R and C
  if FormRC.Voltage.Text = '' then
    E := 0
  else
    E := StrToFloat(FormRC.Voltage.Text);
  if FormRC.Resistance.Text = '' then
    R := 0
  else
    R := StrToFloat(FormRC.Resistance.Text);
  if FormRC.Capacity.Text = '' then
    C := 0
  else
    C := StrToFloat(FormRC.Capacity.Text);
  // All values must be (strictly) positive
  if (E > 0) and (R > 0) and (C > 0) then begin
    // Convert voltage to V
    if FormRC.UnitVoltage.Text = 'mV' then
      E /= 1000;
    // Convert resistance to Ω
    if FormRC.UnitResistance.Text = 'kΩ' then
      R *= 1000
    else if FormRC.UnitResistance.Text = 'MΩ' then
      R *= 1000 * 1000;
    // Convert capacity to F
    if FormRC.UnitCapacity.Text = 'µF' then
      C /= (1000 * 1000)
    else if FormRC.UnitCapacity.Text = 'nF' then
      C /= (1000 * 1000 * 1000)
    else if FormRC.UnitCapacity.Text = 'pF' then
      C /= (1000 * 1000 * 1000 * 1000);
    // Compute maximum voltage, maximum current and time constant
    U0 := E; I0 := U0 / R; T  := R * C;
    // Find voltage unit (for best display)
    if FormRC.UnitVoltage.Text = 'V' then begin
      MU := 1; UU := 'V';
      if U0 < 0.1 then begin
        MU := 1000; UU := 'mV';
      end;
    end
    else begin
      MU := 1000; UU := 'mV';
    end;
    U0R := Round(1000 * MU * U0) / 1000;
    // Find current unit (for best display)
    MI := 1; UI := 'A';
    if I0 < 0.0001 then begin
      MI := 1000 * 1000; UI := 'µA';
    end
    else if I0 < 0.1 then begin
      MI := 1000; UI := 'mA';
    end;
    I0R := Round(1000 * MI * I0) / 1000;
    // Find time unit (for best display)
    MT := 1; UT := 's';
    if T < 0.001 then begin
      MT := 1000 * 1000; UT := 'µs';
    end
    else if T < 1 then begin
      MT := 1000; UT := 'ms';
    end;
    TR  := Round(1000 * MT * T) / 1000;
    // Display result values and draw graphs (if all values fit as real with 3 decimal digits)
    if (U0R <> 0) and (I0R <> 0) and (TR <> 0) then begin
      // Discharge current has to be negative
      if (sAction = 'Discharge') and (sValue = 'i') then
        I0R := -I0R;
      // Display result values (U0, I0 and τ)
      FormRC.CVoltageMax.Text := FloatToStr(U0R) + ' ' + UU;
      FormRC.CCurrentMax.Text := FloatToStr(I0R) + ' ' + UI;
      FormRC.Tau.Text := FloatToStr(TR) + ' ' + UT;
      // Determine legend text for both axises
      LegendX := 't [' + UT + ']';
      if sValue = 'u' then begin
        Legend1Y := 'u(t) [' + UU + ']'; Legend2Y := '';
      end
      else if sValue = 'i' then begin
        Legend1Y := 'i(t) [' + UI + ']'; Legend2Y := '';
      end
      else begin
        Legend1Y := 'u(t) [' + UU + ']';
        Legend2Y := 'i(t) [' + UI + ']';
      end;
      // Draw the axises
      DrawAxis(iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, LegendX, Legend1Y, Legend2Y);
      // Draw the graphs
      DrawGraphs(U0, I0, T, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, UU, UI, UT);
    end
    // Some value to small to fit as real with 3 decimal digits
    else begin
      if U0R <= 0 then
        MessageDlg('Données inadéquates', 'Tension < 0,001 mV!', mtError, [mbOK], 0)
      else if I0R <= 0 then
        MessageDlg('Données inadéquates', 'Courant maximal < 0,001 µA!', mtError, [mbOK], 0)
      else if TR <= 0 then
        MessageDlg('Données inadéquates', 'Constante de temps < 0,001 µs!', mtError, [mbOK], 0);
    end;
  end
  // Negative or zero values entered
  else
    MessageDlg('Données invalides', 'E, R et C doivent tous avoir une valeur positive!', mtError, [mbOK], 0);
end;

end.

