{***************************************}
{* Main unit for Christmas application *}
{***************************************}

unit xmas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LazUTF8;

type
  TXMasText = record
    Language, Txt: string;
  end;
  TBallShapes = array[1..20] of TShape;
  {*************}
  { TfChristmas }
  {*************}
  TfChristmas = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsText, mSettingsTextDefault, mSettingsTextDefaultCH, mSettingsTextDefaultEN: TMenuItem;
    mSettingsTextDefaultFR, mSettingsTextDefaultDE, mSettingsTextDefaultIT, mSettingsTextDefaultJP: TMenuItem;
    mSettingsTextDefaultLB, mSettingsTextDefaultPT, mSettingsTextDefaultRU, mSettingsTextDefaultSP, mSettingsTextCustom: TMenuItem;
    mSettingsColor, mSettingsColorCyan, mSettingsColorLime, mSettingsColorMagenta, mSettingsColorPink: TMenuItem;
    mSettingsColorOrange, mSettingsColorRed, mSettingsColorYellow: TMenuItem;
    mSettingsSize, mSettingsSize10, mSettingsSize12, mSettingsSize14, mSettingsSize15, mSettingsSize16, mSettingsSize18: TMenuItem;
    mSettingsDisplay, mSettingsDisplayStatic, mSettingsDisplayDynamic, mSettingsDisplayBlinking: TMenuItem;
    mSettingsMess2, mSettingsMess2Text, mSettingsMess2SameColor, mSettingsMess2Blinking, MenuItem4, mSettingsMess2None: TMenuItem;
    mSettingsShape, mSettingsShape1, mSettingsShape2, mSettingsShape3, mSettingsShape4: TMenuItem;
    MenuItem1, MenuItem2, MenuItem3, MenuItem5, mSettingsShapeMulti, mSettingsShapeRandom: TMenuItem;
    mSettingsBalls, mSettingsBallsOrange, mSettingsBallsRed, mSettingsBallsYellow: TMenuItem;
    mSettingsBallsWhite, mSettingsBallsMulti, mSettingsBallsRandom: TMenuItem;
    mSettingsBlinking, mSettingsBlinkingAll, mSettingsBlinkingG2, mSettingsBlinkingG3: TMenuItem;
    mSettingsBlinkingG4, mSettingsBlinkingNone, mSettingsBlinkingRandom: TMenuItem;
    mSettingsFrequency, mSettingsFrequency10, mSettingsFrequency5, mSettingsFrequency4, mSettingsFrequency2, mSettingsFrequency1: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    Image1: TImage;
    edXMasText, edXMasText2: TEdit;
    shXBall1, shXBall2, shXBall3, shXBall13, shXBall4, shXBall5: TShape;
    shXBall6, shXBall7, shXBall8, shXBall9, shXBall10: TShape;
    shXBall11, shXBall12, shXBall14, shXBall15: TShape;
    shXBall16, shXBall17, shXBall18, shXBall19, shXBall20: TShape;
    tiXMas: TTimer;
    tiXMas2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsTextDefaultCHClick(Sender: TObject);
    procedure mSettingsTextDefaultENClick(Sender: TObject);
    procedure mSettingsTextDefaultFRClick(Sender: TObject);
    procedure mSettingsTextDefaultDEClick(Sender: TObject);
    procedure mSettingsTextDefaultITClick(Sender: TObject);
    procedure mSettingsTextDefaultJPClick(Sender: TObject);
    procedure mSettingsTextDefaultLBClick(Sender: TObject);
    procedure mSettingsTextDefaultPTClick(Sender: TObject);
    procedure mSettingsTextDefaultRUClick(Sender: TObject);
    procedure mSettingsTextDefaultSPClick(Sender: TObject);
    procedure mSettingsTextCustomClick(Sender: TObject);
    procedure mSettingsColorCyanClick(Sender: TObject);
    procedure mSettingsColorLimeClick(Sender: TObject);
    procedure mSettingsColorMagentaClick(Sender: TObject);
    procedure mSettingsColorOrangeClick(Sender: TObject);
    procedure mSettingsColorPinkClick(Sender: TObject);
    procedure mSettingsColorRedClick(Sender: TObject);
    procedure mSettingsColorYellowClick(Sender: TObject);
    procedure mSettingsSize10Click(Sender: TObject);
    procedure mSettingsSize12Click(Sender: TObject);
    procedure mSettingsSize14Click(Sender: TObject);
    procedure mSettingsSize15Click(Sender: TObject);
    procedure mSettingsSize16Click(Sender: TObject);
    procedure mSettingsSize18Click(Sender: TObject);
    procedure mSettingsDisplayStaticClick(Sender: TObject);
    procedure mSettingsDisplayDynamicClick(Sender: TObject);
    procedure mSettingsDisplayBlinkingClick(Sender: TObject);
    procedure mSettingsMess2TextClick(Sender: TObject);
    procedure mSettingsMess2SameColorClick(Sender: TObject);
    procedure mSettingsMess2BlinkingClick(Sender: TObject);
    procedure mSettingsMess2NoneClick(Sender: TObject);
    procedure mSettingsShape1Click(Sender: TObject);
    procedure mSettingsShape2Click(Sender: TObject);
    procedure mSettingsShape3Click(Sender: TObject);
    procedure mSettingsShape4Click(Sender: TObject);
    procedure mSettingsShapeMultiClick(Sender: TObject);
    procedure mSettingsShapeRandomClick(Sender: TObject);
    procedure mSettingsBallsOrangeClick(Sender: TObject);
    procedure mSettingsBallsRedClick(Sender: TObject);
    procedure mSettingsBallsWhiteClick(Sender: TObject);
    procedure mSettingsBallsYellowClick(Sender: TObject);
    procedure mSettingsBallsMultiClick(Sender: TObject);
    procedure mSettingsBallsRandomClick(Sender: TObject);
    procedure mSettingsBlinkingAllClick(Sender: TObject);
    procedure mSettingsBlinkingG2Click(Sender: TObject);
    procedure mSettingsBlinkingG3Click(Sender: TObject);
    procedure mSettingsBlinkingG4Click(Sender: TObject);
    procedure mSettingsBlinkingNoneClick(Sender: TObject);
    procedure mSettingsBlinkingRandomClick(Sender: TObject);
    procedure mSettingsFrequency10Click(Sender: TObject);
    procedure mSettingsFrequency5Click(Sender: TObject);
    procedure mSettingsFrequency4Click(Sender: TObject);
    procedure mSettingsFrequency2Click(Sender: TObject);
    procedure mSettingsFrequency1Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure tiXMasTimer(Sender: TObject);
    procedure tiXMas2Timer(Sender: TObject);
  private
    iTextLetters, iTextSize, iBlinkingGroup: Integer;
    sTextDisplay, sText2Display, sTextLang, sCustomText, sCustomText2, sBallShape, sBallColor, sBallsBlinkingPattern: string;
    clBallColor: TColor;
    shBalls: TBallShapes;
    stBallShape: TShapeType;
  end;

const
  clOrange = $00A5FF;
  clPink   = $B469FF;
  Colours: array[0..3] of TColor = (
    clWhite, clYellow, clOrange, clRed
  );
  Shapes: array[0..3] of TShapeType = (
    stStar, stCircle, stDiamond, stTriangleDown
  );

var
  fChristmas: TfChristmas;

implementation

{$R *.lfm}

{ Set Christmas message text }

procedure SetText(TxtDisplay: string; var TxtLetters: Integer; Lang, Custom: string);

const
  XMasTexts: array[1..10] of TXMasText = (
    (Language: 'ch'; Txt: '圣诞节快乐'),
    (Language: 'en'; Txt: 'MERRY CHRISTMAS'),
    (Language: 'fr'; Txt: 'JOYEUX NOËL'),
    (Language: 'de'; Txt: 'FRÖHLICHE WEIHNACHTEN'),
    (Language: 'it'; Txt: 'BUON NATALE'),
    (Language: 'jp'; Txt: 'メリークリスマス'),
    (Language: 'lb'; Txt: 'EE SCHÉINE CHRËSCHTDAG'),
    (Language: 'pt'; Txt: 'FELIZ NATAL'),
    (Language: 'ru'; Txt: 'С РОЖДЕСТВОМ'),
    (Language: 'sp'; Txt: 'FELIZ NAVIDAD')
  );

var
  I: Integer;
  S, XMasText: string;

begin
  if Lang = 'xx' then
    // Custom Christmas text passed as argument
    S := Custom
  else begin
    // Default Christmas text for actual language
    for I := 1 to 10 do begin
      if Lang = XMasTexts[I].Language then
        S := XMasTexts[I].Txt;
    end;
  end;
  // What to display depends on actual display option
  if TxtDisplay = 'static' then begin
    // Static display: Show entire text
    XMasText := S;
  end
  else if TxtDisplay = 'dynamic' then begin
    // Dynamic display: Add one letter to actual text shown until entire text is done
    if TxtLetters = 0 then
      XMasText := ''
    else
      XMasText := UTF8Copy(S, 1, TxtLetters);
    if TxtLetters = UTF8Length(S) then                                         // entire text has now been displayed
      TxtLetters := 0;
  end
  else begin
    // Blinking display: Show entire text or nothing
    if TxtLetters = 0 then
      XMasText := ''
    else
      XMasText := S;
  end;
  fChristmas.edXMasText.Text := XMasText;
end;

{ Re-initialize Christmas message text }

procedure ResetText(TxtDisplay: string);

// This procedure, called after a new Christmas message has been chosen, ensures that the display of the message
// performs smoothly and properly for all text display settings. Re-initialisation is done, by simulating a click
// on the "Message display" menu item actually selected

begin
  if TxtDisplay = 'static' then
    fChristmas.mSettingsDisplayStatic.Click
  else if TxtDisplay = 'dynamic' then
    fChristmas.mSettingsDisplayDynamic.Click
  else
    fChristmas.mSettingsDisplayBlinking.Click;
end;

{ Set Christmas balls shape }

procedure SetBallShape(SShape: string; Shape: TShapeType; BlinkingPattern: string; var Balls: TBallShapes);

var
  I: Integer;

begin
  for I := 1 to 20 do begin
    // Unique: Same shape for all balls
    if SShape = 'unique' then begin
      Balls[I].Shape := Shape;
    end
    // Random: One of the 4 shapes, randomly for this ball
    else if SShape = 'random' then begin
      Balls[I].Shape := Shapes[Random(4)];
    end
    // Multi: Usage of "groups" depending on blinking pattern
    else begin
      if BlinkingPattern = 'g2' then begin
        // 2-groups blinking pattern: 2 shape groups
        if I <= 10 then
          Balls[I].Shape := Shapes[0]
        else
          Balls[I].Shape := Shapes[1];
      end
      else if BlinkingPattern = 'g3' then begin
        // 3-groups blinking pattern: 3 shape groups
        if I <= 7 then
          Balls[I].Shape := Shapes[0]
        else if I >= 14 then
          Balls[I].Shape := Shapes[2]
        else
          Balls[I].Shape := Shapes[1];
      end
      else begin
        // All other blinking pattern: 4 shape groups
        if I <= 5 then
          Balls[I].Shape := Shapes[0]
        else if I <= 10 then
          Balls[I].Shape := Shapes[1]
        else if I <= 15 then
          Balls[I].Shape := Shapes[2]
        else
          Balls[I].Shape := Shapes[3];
      end
    end;
  end;
end;

{ Set Christmas balls color }

procedure SetBallColor(SColour: string; Colour: TColor; BlinkingPattern: string; var Balls: TBallShapes);

var
  I: Integer;

begin
  for I := 1 to 20 do begin
    // Unique: Same color for all balls
    if SColour = 'unique' then begin
      Balls[I].Brush.Color := Colour;
    end
    // Random: One of the 4 colors, randomly for this ball
    else if SColour = 'random' then begin
      Balls[I].Brush.Color := Colours[Random(4)];
    end
    // Multi: Usage of "groups" depending on blinking pattern
    else begin
      if BlinkingPattern = 'g2' then begin
        // 2-groups blinking pattern: 2 color groups
        if I <= 10 then
          Balls[I].Brush.Color := Colours[0]
        else
          Balls[I].Brush.Color := Colours[1];
      end
      else if BlinkingPattern = 'g3' then begin
        // 3-groups blinking pattern: 3 color groups
        if I <= 7 then
          Balls[I].Brush.Color := Colours[0]
        else if I >= 14 then
          Balls[I].Brush.Color := Colours[2]
        else
          Balls[I].Brush.Color := Colours[1];
      end
      else begin
        // All other blinking pattern: 4 color groups
        if I <= 5 then
          Balls[I].Brush.Color := Colours[0]
        else if I <= 10 then
          Balls[I].Brush.Color := Colours[1]
        else if I <= 15 then
          Balls[I].Brush.Color := Colours[2]
        else
          Balls[I].Brush.Color := Colours[3];
      end
    end;
  end;
end;

{*************}
{ TfChristmas }
{*************}

{ Application start: Initialisation }

procedure TfChristmas.FormCreate(Sender: TObject);

begin
  // Create array with the ball shapes
  shBalls[1]  := shXBall1;  shBalls[2]  := shXBall2;  shBalls[3]  := shXBall3;  shBalls[4]  := shXBall4;  shBalls[5]  := shXBall5;
  shBalls[6]  := shXBall6;  shBalls[7]  := shXBall7;  shBalls[8]  := shXBall8;  shBalls[9]  := shXBall9;  shBalls[10] := shXBall10;
  shBalls[11] := shXBall11; shBalls[12] := shXBall12; shBalls[13] := shXBall13; shBalls[14] := shXBall14; shBalls[15] := shXBall15;
  shBalls[16] := shXBall16; shBalls[17] := shXBall17; shBalls[18] := shXBall18; shBalls[19] := shXBall19; shBalls[20] := shXBall20;
  // Initialize parameters (startup settings)
  sTextLang := 'en'; sCustomText := 'MERRY CHRISTMAS'; iTextSize := 15;
  sTextDisplay := 'blinking'; iTextLetters := 1;
  sBallShape := 'unique'; stBallShape := stCircle;
  sBallColor := 'unique'; clBallColor := clWhite;
  sBallsBlinkingPattern := 'none'; iBlinkingGroup := -1;
  sCustomText2 := 'A peace- and joyful Christmas to everyone!';
  sText2Display := 'static';
  // Start the message timer (as "blinking" is selected at startup)
  tiXMas2.Interval := 10; tiXMas2.Enabled := True;
  // Disable the balls timer (as "no blinking pattern" is selected at startup)
  tiXMas.Interval := 1000; tiXMas.Enabled := False;
  // Start random number generator
  Randomize;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfChristmas.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Message text > ...": Select default Christmas message in one of the available languages or enter custom text }

procedure TfChristmas.mSettingsTextDefaultCHClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := True;  mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'ch';                                                           // Chinese
  ResetText(sTextDisplay);                                                     // reset message text for proper display
end;

procedure TfChristmas.mSettingsTextDefaultENClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := True;  mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'en';                                                           // English
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultFRClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := True;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'fr';                                                           // French
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultDEClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := True;  mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'de';                                                           // German
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultITClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := True;  mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'it';                                                           // Italian
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultJPClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := True;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'jp';                                                           // Japanese
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultLBClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := True;  mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'lb';                                                           // Luxembourgish
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultPTClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := True;  mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'pt';                                                           // Portuguese
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultRUClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := True;
  mSettingsTextDefaultSP.Checked := False;
  sTextLang := 'ru';                                                           // Russian
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextDefaultSPClick(Sender: TObject);

begin
  mSettingsTextDefaultCH.Checked := False; mSettingsTextDefaultEN.Checked := False; mSettingsTextDefaultFR.Checked := False;
  mSettingsTextDefaultDE.Checked := False; mSettingsTextDefaultIT.Checked := False; mSettingsTextDefaultJP.Checked := False;
  mSettingsTextDefaultLB.Checked := False; mSettingsTextDefaultPT.Checked := False; mSettingsTextDefaultRU.Checked := False;
  mSettingsTextDefaultSP.Checked := True;
  sTextLang := 'sp';                                                           // Spanish
  ResetText(sTextDisplay);
end;

procedure TfChristmas.mSettingsTextCustomClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Christmas', 'Custom Christmas text', edXMasText.Text);
  if S <> '' then begin                                                        // get custom Christmas message, unless "Cancel" button has been pushed
    sCustomText := S;
    sTextLang := 'xx';                                                         // code for "custom message"
    ResetText(sTextDisplay);                                                   // reset message text for proper display
  end;
end;

{ Menu items "Settings > Message color > ...": Select Christmas message font color }

procedure TfChristmas.mSettingsColorCyanClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := True;    mSettingsColorLime.Checked := False; mSettingsColorMagenta.Checked := False;
  mSettingsColorOrange.Checked := False; mSettingsColorPink.Checked := False; mSettingsColorRed.Checked := False;
  mSettingsColorYellow.Checked := False;
  edXMasText.Font.Color := clAqua;
  if mSettingsMess2SameColor.Checked then                                      // if personal message has to have same color as main message...
    edXMasText2.Font.Color := clAqua;                                          // change personal message font color, too
end;

procedure TfChristmas.mSettingsColorLimeClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := False;   mSettingsColorLime.Checked := True;  mSettingsColorMagenta.Checked := False;
  mSettingsColorOrange.Checked := False; mSettingsColorPink.Checked := False; mSettingsColorRed.Checked := False;
  mSettingsColorYellow.Checked := False;
  edXMasText.Font.Color := clLime;
  if mSettingsMess2SameColor.Checked then
    edXMasText2.Font.Color := clLime;
end;

procedure TfChristmas.mSettingsColorMagentaClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := False;   mSettingsColorLime.Checked := False; mSettingsColorMagenta.Checked := True;
  mSettingsColorOrange.Checked := False; mSettingsColorPink.Checked := False; mSettingsColorRed.Checked := False;
  mSettingsColorYellow.Checked := False;
  edXMasText.Font.Color := clFuchsia;
  if mSettingsMess2SameColor.Checked then
    edXMasText2.Font.Color := clFuchsia;
end;

procedure TfChristmas.mSettingsColorOrangeClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := False;  mSettingsColorLime.Checked := False; mSettingsColorMagenta.Checked := False;
  mSettingsColorOrange.Checked := True; mSettingsColorPink.Checked := False; mSettingsColorRed.Checked := False;
  mSettingsColorYellow.Checked := False;
  edXMasText.Font.Color := clOrange;
  if mSettingsMess2SameColor.Checked then
    edXMasText2.Font.Color := clOrange;
end;

procedure TfChristmas.mSettingsColorPinkClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := False;   mSettingsColorLime.Checked := False; mSettingsColorMagenta.Checked := False;
  mSettingsColorOrange.Checked := False; mSettingsColorPink.Checked := True;  mSettingsColorRed.Checked := False;
  mSettingsColorYellow.Checked := False;
  edXMasText.Font.Color := clPink;
  if mSettingsMess2SameColor.Checked then
    edXMasText2.Font.Color := clPink;
end;

procedure TfChristmas.mSettingsColorRedClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := False;   mSettingsColorLime.Checked := False; mSettingsColorMagenta.Checked := False;
  mSettingsColorOrange.Checked := False; mSettingsColorPink.Checked := False; mSettingsColorRed.Checked := True;
  mSettingsColorYellow.Checked := False;
  edXMasText.Font.Color := clRed;
  if mSettingsMess2SameColor.Checked then
    edXMasText2.Font.Color := clRed;
end;

procedure TfChristmas.mSettingsColorYellowClick(Sender: TObject);

begin
  mSettingsColorCyan.Checked := False;   mSettingsColorLime.Checked := False; mSettingsColorMagenta.Checked := False;
  mSettingsColorOrange.Checked := False; mSettingsColorPink.Checked := False; mSettingsColorRed.Checked := False;
  mSettingsColorYellow.Checked := True;
  edXMasText.Font.Color := clYellow;
  if mSettingsMess2SameColor.Checked then
    edXMasText2.Font.Color := clYellow;
end;

{ Menu items "Settings > Message size > ...": Select Christmas message font size }

procedure TfChristmas.mSettingsSize10Click(Sender: TObject);

begin
  mSettingsSize10.Checked := True;  mSettingsSize12.Checked := False; mSettingsSize14.Checked := False;
  mSettingsSize15.Checked := False; mSettingsSize16.Checked := False; mSettingsSize18.Checked := False;
  edXMasText.Font.Size := 10;
end;

procedure TfChristmas.mSettingsSize12Click(Sender: TObject);

begin
  mSettingsSize10.Checked := False; mSettingsSize12.Checked := True;  mSettingsSize14.Checked := False;
  mSettingsSize15.Checked := False; mSettingsSize16.Checked := False; mSettingsSize18.Checked := False;
  edXMasText.Font.Size := 12;
end;

procedure TfChristmas.mSettingsSize14Click(Sender: TObject);

begin
  mSettingsSize10.Checked := False; mSettingsSize12.Checked := False; mSettingsSize14.Checked := True;
  mSettingsSize15.Checked := False; mSettingsSize16.Checked := False; mSettingsSize18.Checked := False;
  edXMasText.Font.Size := 14;
end;

procedure TfChristmas.mSettingsSize15Click(Sender: TObject);

begin
  mSettingsSize10.Checked := False; mSettingsSize12.Checked := False; mSettingsSize14.Checked := False;
  mSettingsSize15.Checked := True;  mSettingsSize16.Checked := False; mSettingsSize18.Checked := False;
  edXMasText.Font.Size := 15;
end;

procedure TfChristmas.mSettingsSize16Click(Sender: TObject);

begin
  mSettingsSize10.Checked := False; mSettingsSize12.Checked := False; mSettingsSize14.Checked := False;
  mSettingsSize15.Checked := False; mSettingsSize16.Checked := True;  mSettingsSize18.Checked := False;
  edXMasText.Font.Size := 16;
end;

procedure TfChristmas.mSettingsSize18Click(Sender: TObject);

begin
  mSettingsSize10.Checked := False; mSettingsSize12.Checked := False; mSettingsSize14.Checked := False;
  mSettingsSize15.Checked := False; mSettingsSize16.Checked := False; mSettingsSize18.Checked := True;
  edXMasText.Font.Size := 18;
end;

{ Menu items "Settings > Message display > ...": Toggle Christmas message display options }

procedure TfChristmas.mSettingsDisplayStaticClick(Sender: TObject);

begin
  mSettingsDisplayStatic.Checked := True; mSettingsDisplayDynamic.Checked := False; mSettingsDisplayBlinking.Checked := False;
  sTextDisplay := 'static';  iTextLetters := -1;
  SetText(sTextDisplay, iTextLetters, sTextLang, sCustomText);                 // be sure the full message is displayed...
  edXMasText.Visible := True;                                                  // ...and actually visible
  tiXMas2.Enabled := False;                                                    // disable the message timer for static display
end;

procedure TfChristmas.mSettingsDisplayDynamicClick(Sender: TObject);

begin
  mSettingsDisplayStatic.Checked := False; mSettingsDisplayDynamic.Checked := True; mSettingsDisplayBlinking.Checked := False;
  sTextDisplay := 'dynamic'; iTextLetters := 0;
  edXMasText.Text := ''; edXMasText.Visible := True;                           // clear message text
  tiXMas2.Interval := 10; tiXMas2.Enabled := True;                             // start the message timer
end;

procedure TfChristmas.mSettingsDisplayBlinkingClick(Sender: TObject);

begin
  mSettingsDisplayStatic.Checked := False; mSettingsDisplayDynamic.Checked := False; mSettingsDisplayBlinking.Checked := True;
  sTextDisplay := 'blinking'; iTextLetters := 1;
  SetText(sTextDisplay, iTextLetters, sTextLang, sCustomText);                 // start blinking with displaying the message
  edXMasText.Visible := True;                                                  // be sure the message is visible
  tiXMas2.Interval := 10; tiXMas2.Enabled := True;                             // start the message timer
end;

{ Menu item "Settings > Personal message > Personal message text": User entry of personal message text }

procedure TfChristmas.mSettingsMess2TextClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Christmas', 'Personal greeting message', edXMasText2.Text);
  if S <> '' then                                                              // get personal message, unless "Cancel" button has been pushed
    sCustomText2 := S;
  edXmasText2.Text := sCustomText2;
end;

{ Menu item "Settings > Personal message > Same color as main message": Toggle personal message font color same as Christmas message or yellow }

procedure TfChristmas.mSettingsMess2SameColorClick(Sender: TObject);

begin
  if mSettingsMess2SameColor.Checked then begin
    mSettingsMess2SameColor.Checked := False;
    edXmasText2.Font.Color := clYellow;
  end
  else begin
    mSettingsMess2SameColor.Checked := True;
    edXmasText2.Font.Color := edXmasText.Font.Color;
  end;
end;

{ Menu item "Settings > Personal message > Blinking with main message": Toggle personal message display blinking with Christmas message or static }

procedure TfChristmas.mSettingsMess2BlinkingClick(Sender: TObject);

begin
  if mSettingsMess2Blinking.Checked then begin
    mSettingsMess2Blinking.Checked := False;
    sText2Display := 'static';
    if not mSettingsMess2None.Checked then
      edXMasText2.Visible := True;                                             // be sure the text is visible
  end
  else begin
  mSettingsMess2Blinking.Checked := True;
  sText2Display := 'blinking';
  if not mSettingsMess2None.Checked then
    edXMasText2.Visible := True;                                               // start blinking with displaying the message
  end;
end;

{ Menu item "Settings > Personal message > No personal message": Toggle personal message displayed (visible) or not }

procedure TfChristmas.mSettingsMess2NoneClick(Sender: TObject);

begin
  if mSettingsMess2None.Checked then begin
    mSettingsMess2None.Checked := False;
    edXMasText2.Visible := True;
  end
  else begin
    mSettingsMess2None.Checked := True;
    edXMasText2.Visible := False;
  end;
end;

{ Menu items "Settings > Balls shape > ...": Select Christmas balls shape }

procedure TfChristmas.mSettingsShape1Click(Sender: TObject);

begin
  mSettingsShape1.Checked := True;  mSettingsShape2.Checked := False;     mSettingsShape3.Checked := False;
  mSettingsShape4.Checked := False; mSettingsShapeMulti.Checked := False; mSettingsShapeRandom.Checked := False;
  sBallShape := 'unique'; stBallShape := stCircle;                             // all balls are circles
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsShape2Click(Sender: TObject);

begin
  mSettingsShape1.Checked := False; mSettingsShape2.Checked := True;      mSettingsShape3.Checked := False;
  mSettingsShape4.Checked := False; mSettingsShapeMulti.Checked := False; mSettingsShapeRandom.Checked := False;
  sBallShape := 'unique'; stBallShape := stStar;                               // all balls are stars
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsShape3Click(Sender: TObject);

begin
  mSettingsShape1.Checked := False; mSettingsShape2.Checked := False;     mSettingsShape3.Checked := True;
  mSettingsShape4.Checked := False; mSettingsShapeMulti.Checked := False; mSettingsShapeRandom.Checked := False;
  sBallShape := 'unique'; stBallShape := stDiamond;                            // all balls are diamonds
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsShape4Click(Sender: TObject);

begin
  mSettingsShape1.Checked := False; mSettingsShape2.Checked := False;     mSettingsShape3.Checked := False;
  mSettingsShape4.Checked := True;  mSettingsShapeMulti.Checked := False; mSettingsShapeRandom.Checked := False;
  sBallShape := 'unique'; stBallShape := stTriangleDown;                       // all balls are triangles
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsShapeMultiClick(Sender: TObject);

begin
  mSettingsShape1.Checked := False; mSettingsShape2.Checked := False;     mSettingsShape3.Checked := False;
  mSettingsShape4.Checked := False; mSettingsShapeMulti.Checked := True; mSettingsShapeRandom.Checked := False;
  sBallShape := 'multi';                                                       // ball shape depending on "color group"
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsShapeRandomClick(Sender: TObject);

begin
  mSettingsShape1.Checked := False; mSettingsShape2.Checked := False;     mSettingsShape3.Checked := False;
  mSettingsShape4.Checked := False; mSettingsShapeMulti.Checked := False; mSettingsShapeRandom.Checked := True;
  sBallShape := 'random';                                                      // random ball shape
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
end;

{ Menu items "Settings > Balls color > ...": Select Christmas balls color }

procedure TfChristmas.mSettingsBallsOrangeClick(Sender: TObject);

begin
  mSettingsBallsOrange.Checked := True;  mSettingsBallsRed.Checked := False;   mSettingsBallsWhite.Checked := False;
  mSettingsBallsYellow.Checked := False; mSettingsBallsMulti.Checked := False; mSettingsBallsRandom.Checked := False;
  sBallColor := 'unique'; clBallColor := clOrange;                             // all balls colored in orange
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsBallsRedClick(Sender: TObject);

begin
  mSettingsBallsOrange.Checked := False; mSettingsBallsRed.Checked := True;    mSettingsBallsWhite.Checked := False;
  mSettingsBallsYellow.Checked := False; mSettingsBallsMulti.Checked := False; mSettingsBallsRandom.Checked := False;
  sBallColor := 'unique'; clBallColor := clRed;                                // all balls colored in red
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsBallsWhiteClick(Sender: TObject);

begin
  mSettingsBallsOrange.Checked := False; mSettingsBallsRed.Checked := False;   mSettingsBallsWhite.Checked := True;
  mSettingsBallsYellow.Checked := False; mSettingsBallsMulti.Checked := False; mSettingsBallsRandom.Checked := False;
  sBallColor := 'unique'; clBallColor := clWhite;                              // all balls colored in white
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsBallsYellowClick(Sender: TObject);

begin
  mSettingsBallsOrange.Checked := False; mSettingsBallsRed.Checked := False;   mSettingsBallsWhite.Checked := False;
  mSettingsBallsYellow.Checked := True;  mSettingsBallsMulti.Checked := False; mSettingsBallsRandom.Checked := False;
  sBallColor := 'unique'; clBallColor := clYellow;                             // all balls colored in yellow
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsBallsMultiClick(Sender: TObject);

begin
  mSettingsBallsOrange.Checked := False; mSettingsBallsRed.Checked := False;  mSettingsBallsWhite.Checked := False;
  mSettingsBallsYellow.Checked := False; mSettingsBallsMulti.Checked := True; mSettingsBallsRandom.Checked := False;
  sBallColor := 'multi';                                                       // balls color depending on "color-group"
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
end;

procedure TfChristmas.mSettingsBallsRandomClick(Sender: TObject);

begin
  mSettingsBallsOrange.Checked := False; mSettingsBallsRed.Checked := False;   mSettingsBallsWhite.Checked := False;
  mSettingsBallsYellow.Checked := False; mSettingsBallsMulti.Checked := False; mSettingsBallsRandom.Checked := True;
  sBallColor := 'random';                                                      // random ball color
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
end;

{ Menu items "Settings > Blinking pattern > ...": Select Christmas balls blinking pattern }

procedure TfChristmas.mSettingsBlinkingAllClick(Sender: TObject);

var
  I: Integer;

begin
  mSettingsBlinkingAll.Checked := True;  mSettingsBlinkingG2.Checked := False;   mSettingsBlinkingG3.Checked := False;
  mSettingsBlinkingG4.Checked := False;  mSettingsBlinkingNone.Checked := False; mSettingsBlinkingRandom.Checked := False;
  for I := 1 to 20 do begin
    shBalls[I].Visible := True;                                                // be sure are balls are visible
  end;
  sBallsBlinkingPattern := 'g1'; iBlinkingGroup := 0;                          // all balls will blink together
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);       // colors are dependent of blinking pattern
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);       // shapes are dependent of blinking pattern
  tiXMas.Enabled := True;                                                      // start balls blinking timer
  mSettingsFrequency.Enabled := True;                                          // enable user to change blinking frequency
end;

procedure TfChristmas.mSettingsBlinkingG2Click(Sender: TObject);

var
  I: Integer;

begin
  mSettingsBlinkingAll.Checked := False; mSettingsBlinkingG2.Checked := True;    mSettingsBlinkingG3.Checked := False;
  mSettingsBlinkingG4.Checked := False;  mSettingsBlinkingNone.Checked := False; mSettingsBlinkingRandom.Checked := False;
  for I := 1 to 20 do begin
    shBalls[I].Visible := True;
  end;
  sBallsBlinkingPattern := 'g2'; iBlinkingGroup := 0;                          // alternate "light on" of upper and lower ball group
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
  mSettingsFrequency.Enabled := True;
  tiXMas.Enabled := True;
end;

procedure TfChristmas.mSettingsBlinkingG3Click(Sender: TObject);

var
  I: Integer;

begin
  mSettingsBlinkingAll.Checked := False; mSettingsBlinkingG2.Checked := False;   mSettingsBlinkingG3.Checked := True;
  mSettingsBlinkingG4.Checked := False;  mSettingsBlinkingNone.Checked := False; mSettingsBlinkingRandom.Checked := False;
  for I := 1 to 20 do begin
    shBalls[I].Visible := True;
  end;
  sBallsBlinkingPattern := 'g3'; iBlinkingGroup := 0;                          // alternate "light on" of one of 3 ball groups
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
  tiXMas.Enabled := True;
  mSettingsFrequency.Enabled := True;
end;

procedure TfChristmas.mSettingsBlinkingG4Click(Sender: TObject);

var
  I: Integer;

begin
  mSettingsBlinkingAll.Checked := False; mSettingsBlinkingG2.Checked := False;   mSettingsBlinkingG3.Checked := False;
  mSettingsBlinkingG4.Checked := True;   mSettingsBlinkingNone.Checked := False; mSettingsBlinkingRandom.Checked := False;
  for I := 1 to 20 do begin
    shBalls[I].Visible := True;
  end;
  sBallsBlinkingPattern := 'g4'; iBlinkingGroup := 0;                          // alternate "light on" of one of 4 ball groups
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
  tiXMas.Enabled := True;
  mSettingsFrequency.Enabled := True;
end;

procedure TfChristmas.mSettingsBlinkingNoneClick(Sender: TObject);

var
  I: Integer;

begin
  mSettingsBlinkingAll.Checked := False; mSettingsBlinkingG2.Checked := False;  mSettingsBlinkingG3.Checked := False;
  mSettingsBlinkingG4.Checked := False;  mSettingsBlinkingNone.Checked := True; mSettingsBlinkingRandom.Checked := False;
  for I := 1 to 20 do begin
    shBalls[I].Visible := True;
  end;
  sBallsBlinkingPattern := 'g0'; iBlinkingGroup := -1;                         // static balls (no blinking)
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
  tiXMas.Enabled := False;                                                     // disable balls blinking timer
  mSettingsFrequency.Enabled := False;                                         // disable access to frequency settings (N/A)
end;

procedure TfChristmas.mSettingsBlinkingRandomClick(Sender: TObject);

var
  I: Integer;

begin
  mSettingsBlinkingAll.Checked := False; mSettingsBlinkingG2.Checked := False;   mSettingsBlinkingG3.Checked := False;
  mSettingsBlinkingG4.Checked := False;  mSettingsBlinkingNone.Checked := False; mSettingsBlinkingRandom.Checked := True;
  for I := 1 to 20 do begin
    shBalls[I].Visible := True;
  end;
  sBallsBlinkingPattern := 'gr'; iBlinkingGroup := -1;                         // all balls will blink randomly
  SetBallColor(sBallColor, clBallColor, sBallsBlinkingPattern, shBalls);
  SetBallShape(sBallShape, stBallShape, sBallsBlinkingPattern, shBalls);
  tiXMas.Enabled := True;
  mSettingsFrequency.Enabled := True;
end;

{ Menu items "Settings > Blinking frequencies > ...": Select balls blinking frequency }

procedure TfChristmas.mSettingsFrequency10Click(Sender: TObject);

begin
  mSettingsFrequency10.Checked := True;  mSettingsFrequency5.Checked := False; mSettingsFrequency4.Checked := False;
  mSettingsFrequency2.Checked := False;  mSettingsFrequency1.Checked := False;
  tiXMas.Interval := 100;
end;

procedure TfChristmas.mSettingsFrequency5Click(Sender: TObject);

begin
  mSettingsFrequency10.Checked := False; mSettingsFrequency5.Checked := True; mSettingsFrequency4.Checked := False;
  mSettingsFrequency2.Checked := False;  mSettingsFrequency1.Checked := False;
  tiXMas.Interval := 200;
end;

procedure TfChristmas.mSettingsFrequency4Click(Sender: TObject);

begin
  mSettingsFrequency10.Checked := False; mSettingsFrequency5.Checked := False; mSettingsFrequency4.Checked := True;
  mSettingsFrequency2.Checked := False;  mSettingsFrequency1.Checked := False;
  tiXMas.Interval := 250;
end;

procedure TfChristmas.mSettingsFrequency2Click(Sender: TObject);

begin
  mSettingsFrequency10.Checked := False; mSettingsFrequency5.Checked := False; mSettingsFrequency4.Checked := False;
  mSettingsFrequency2.Checked := True;   mSettingsFrequency1.Checked := False;
  tiXMas.Interval := 500;
end;

procedure TfChristmas.mSettingsFrequency1Click(Sender: TObject);

begin
  mSettingsFrequency10.Checked := False; mSettingsFrequency5.Checked := False; mSettingsFrequency4.Checked := False;
  mSettingsFrequency2.Checked := False;  mSettingsFrequency1.Checked := True;
  tiXMas.Interval := 1000;
end;

{ Menu item "Help > About": Display application about }

procedure TfChristmas.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Customizable Christmas tree.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, February-October 2020.';
  MessageDlg('About "Christmas"', S, mtInformation, [mbOK], 0);
end;

{ Balls blinking timer routine }
{ ---------------------------- }

procedure TfChristmas.tiXMasTimer(Sender: TObject);

// Blinking pattern depending on actual settings (menu item selections)

var
  BlinkingGroups, I: Integer;

begin
  for I := 1 to 20 do begin
    // All balls blinking together pattern: Alternately turn "light" on and off
    if sBallsBlinkingPattern = 'g1' then begin
      shBalls[I].Visible := not shBalls[I].Visible;
    end
    // 2-groups blinking pattern: Alternately turn "light" on for one or the other group
    else if sBallsBlinkingPattern = 'g2' then begin
      if iBlinkingGroup = 0 then begin
        if I <= 10 then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end
      else begin
        if I <= 10 then
          shBalls[I].Visible := False
        else
          shBalls[I].Visible := True;
      end;
    end
    // 3-groups blinking pattern: Alternately turn "light" on for one of the 3 groups
    else if sBallsBlinkingPattern = 'g3' then begin
      if iBlinkingGroup = 0 then begin
        if I in [1..7] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end
      else if iBlinkingGroup = 1 then begin
        if I in [8..13] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end
      else begin
        if I in [14..20] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end;
    end
    // 4-groups blinking pattern: Alternately turn "light" on for one of the 4 groups
    else if sBallsBlinkingPattern = 'g4' then begin
      if iBlinkingGroup = 0 then begin
        if I in [1..5] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end
      else if iBlinkingGroup = 1 then begin
        if I in [6..10] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end
      else if iBlinkingGroup = 2 then begin
        if I in [11..15] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end
      else begin
        if I in [16..20] then
          shBalls[I].Visible := True
        else
          shBalls[I].Visible := False;
      end;
    end
    // Random blinking pattern: Turn "light" of each ball randomly on or off
    else if sBallsBlinkingPattern = 'gr' then begin
      if Random(2) = 0 then
        shBalls[I].Visible := True
      else
        shBalls[I].Visible := False;
    end;
  end;
  // For multi-group blinking patterns, point to next group
  if (sBallsBlinkingPattern = 'g2') or (sBallsBlinkingPattern = 'g3') or (sBallsBlinkingPattern = 'g4') then begin
    BlinkingGroups := StrToInt(RightStr(sBallsBlinkingPattern, 1));
    Inc(iBlinkingGroup);
    if iBlinkingGroup = BlinkingGroups then                                    // if all groups done, point to first group again
      iBlinkingGroup := 0;
  end;
end;

{ Message text timer }
{ ------------------ }

procedure TfChristmas.tiXMas2Timer(Sender: TObject);

// Controls blinking resp. letter-by-letter display of the message text

begin
  // Dynamic message display: Message displayed by adding one letter at a time
  if sTextDisplay = 'dynamic' then begin
    // Letter counter = 0: Make message invisible
    if iTextLetters = 0 then begin
      edXMasText.Visible := False;
      tiXMas2.Interval := 2000;                                                // "nothing display" during 2 sec
      iTextLetters := 1;                                                       // 1st letter of message to show then
    end
    // Letter counter > 0: Show corresponding part of the message text
    else begin
      SetText(sTextDisplay, iTextLetters, sTextLang, sCustomText);             // display (first part of) message text
      edXMasText.Visible := True;                                              // be sure message is visible
      if iTextLetters = 0 then                                                 // letter counter set to 0 by "SetText" if entire message has been shown
        tiXMas2.Interval := 2000                                               // full message text display during 2 sec
      else begin
        Inc(iTextLetters);                                                     // increment letter counter
        tiXMas2.Interval := 500;                                               // interval before adding the next letter = 0.5 sec
      end;
    end;
  end
  // Blinking message display: Alternately show and hide the message text
  else begin
    // "1" indicates that message has to be shown
    if iTextLetters = 1 then begin
      SetText(sTextDisplay, iTextLetters, sTextLang, sCustomText);             // display (entire) message text
      edXMasText.Visible := True;                                              // be sure the message is visible
      if not mSettingsMess2None.Checked then begin
        // Blinking of personal message, too (if this option is selected)
        if mSettingsMess2Blinking.Checked then begin
          edXMasText2.Visible := True;
        end;
      end;
      tiXMas2.Interval := 2000;                                                // display the message during 2 sec
      iTextLetters := 0;
    end
    // "0" indicates that message has to be hidden
    else begin
      edXMasText.Visible := False;                                             // just make edit field invisible in this case
      if not mSettingsMess2None.Checked then begin
        // Blinking of personal message, too (if this option is selected)
        if mSettingsMess2Blinking.Checked then begin
          edXMasText2.Visible := False;
        end;
      end;
      tiXMas2.Interval := 500;                                                 // "nothing display" during 0.5 sec
      iTextLetters := 1;
    end;
  end;
end;

end.

