{****************************************}
{* Main unit for Spirometry application *}
{****************************************}

unit lv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, PopupNotifier, help;

type
  TLungVol = record
    TV, IRV, ERV, FRC: Real;
  end;
  { TfLV }
  TfLV = class(TForm)
    StaticText1: TStaticText;
    mMenu: TMainMenu;
    mFile, mFileNew, mFileAverage, mFileExit: TMenuItem;
    mSettings, mSettingsL: TMenuItem;
    mHelp, mHelpPhysiology, mHelpHelp, mHelpAbout: TMenuItem;
    Label1,  Label2,  Label3,  Label4,  Label5:  TLabel;
    Label6,  Label7,  Label8,  Label9,  Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15: TLabel;
    Label16, Label17, Label18: TLabel;
    edTV, edIRV, edERV: TEdit;
    edRV, edFRC, edIC, edVC, edTLC : TEdit;
    rgRV, rgSex: TRadioGroup;
    Shape1: TShape;
    rbHelium, rbNitrogen: TRadioButton;
    edHeV, edHeC1, edHeC2: TEdit;
    edNV, edNC1, edNC2: TEdit;
    btCalc: TButton;
    pnAbout: TPopupNotifier;
    procedure edHeC1Change(Sender: TObject);
    procedure edHeC2Change(Sender: TObject);
    procedure edHeVChange(Sender: TObject);
    procedure edNC1Change(Sender: TObject);
    procedure edNVChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileAverageClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsLClick(Sender: TObject);
    procedure mHelpPhysiologyClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure rgRVClick(Sender: TObject);
    procedure rgSexClick(Sender: TObject);
    procedure rbHeliumChange(Sender: TObject);
    procedure rbNitrogenChange(Sender: TObject);
  private
    iNormal, iL: Integer;
    rTV, rIRV, rERV, rRV, rFRC, rIC, rVC, rTLC: Real;
  end;

const
  // Normal average values for men and women
  Normal: array[0..1] of TLungVol = (
    (TV: 500; IRV: 3000; ERV: 1100; FRC: 2300),
    (TV: 500; IRV: 1900; ERV: 700;  FRC: 1800)
  );

var
  fLV: TfLV;

implementation

{$R *.lfm}

{ Display lung volumes and capacities }

procedure DisplayValues(TV, IRV, ERV, FRC: Real; L: Integer);

// L = 1000 if milliliters have to be displayed as liters, oterwise L = 1

begin
  if TV = 0 then
    fLV.edTV.Text := ''
  else
    fLV.edTV.Text := FloatToStr(TV / L);
  if IRV = 0 then
    fLV.edIRV.Text := ''
  else
    fLV.edIRV.Text := FloatToStr(IRV / L);
  if ERV = 0 then
    fLV.edERV.Text := ''
  else
    fLV.edERV.Text := FloatToStr(ERV / L);
  if FRC = 0 then
    fLV.edFRC.Text := ''
  else
    fLV.edFRC.Text := FloatToStr(FRC / L);
  fLV.edIC.Text  := '';
  fLV.edVC.Text  := '';
  fLV.edRV.Text  := '';
  fLV.edTLC.Text := '';
end;

{********}
{* TfLV *}
{********}

{ Application start: Display normal (male) average values }

procedure TfLV.FormCreate(Sender: TObject);

begin
  iL := 1;
  mFileAverage.Click;
end;

{ Menu item "File > New calculation": Clear input and result fields }

procedure TfLV.mFileNewClick(Sender: TObject);

begin
  rTV := 0; rIRV := 0; rERV := 0; rFRC := 0;
  DisplayValues(rTV, rIRV, rERV, rFRC, 1);
end;

{ Menu item "File > Average values": Display normal average values }

procedure TfLV.mFileAverageClick(Sender: TObject);

begin
  rTV := Normal[iNormal].TV; rIRV := Normal[iNormal].IRV;
  rERV := Normal[iNormal].ERV; rFRC := Normal[iNormal].FRC;
  DisplayValues(rTV, rIRV, rERV, rFRC, iL);
end;

{ Menu item "File > Exit application": Exit application }

procedure TfLV.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Volume in L": Choose volume unit (ml or L) }

procedure TfLV.mSettingsLClick(Sender: TObject);

begin
  if mSettingsL.Checked then begin
    mSettingsL.Checked := False;
    iL := 1;
  end
  else begin
    mSettingsL.Checked := True;
    iL := 1000;
  end;
end;

{ Menu item "Help > Physiology": Display physiology help text }

procedure TfLV.mHelpPhysiologyClick(Sender: TObject);

begin
  fHelp.Caption := 'Spirometry - Physiology Help.';
  fHelp.memoHelp.Clear;
  fHelp.memoHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program": Display program  help text }

procedure TfLV.mHelpHelpClick(Sender: TObject);

begin
  fHelp.Caption := 'Spirometry - Program Help.';
  fHelp.memoHelp.Clear;
  fHelp.memoHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program about text }

procedure TfLV.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Visible := False;
  S := 'Physiology: Spirometry.' + Chr(13);
  S += 'Measurement of lung volumes and capacities.' + Chr(13) + Chr(13);
  S += 'Version 1.0, © allu, June-July, 2018.';
  pnAbout.Text := S;
  pnAbout.Visible := True;
end;

{ Button "Calculate": Calculate lung volumes and capacities }

procedure TfLV.btCalcClick(Sender: TObject);

var
  V, C1, C2: Real;
  Mess: string;
  Err: Boolean;

begin
  Err := False;
  // Clear all calculation fields
  edIC.Text := ''; edVC.Text  := '';
  edRV.Text := ''; edTLC.Text := '';
  // Read user input values
  if edTV.Text = '' then
    rTV := 0
  else
    rTV := StrToFloat(edTV.Text);
  if edIRV.Text = '' then
    rIRV := 0
  else
    rIRV := StrToFloat(edIRV.Text);
  if edERV.Text = '' then
    rERV := 0
  else
    rERV := StrToFloat(edERV.Text);
  if (rTV <= 0) or (rIRV <= 0) or (rERV <= 0) then begin
    MessageDlg('Invalid user data', 'TV, IRV and ERV all be greater than zero!', mtError, [mbCancel], 0);
    Err := True;
  end;
  if not Err then begin
    // FRC input depending on actual settings:
    //   - if input field contains a value, this value is used
    //   - if either of the FRC measurement methods is checked, FRC value will be calculated using corr. vol/conc values
    //   - otherwise, FRC is supposed to be calculated from estimated RV
    if edFRC.Text = '' then begin
      // Helium dilution technique
      if rbHelium.Checked then begin
        if edHeV.Text = '' then
          V := 0
        else
          V := StrToFloat(edHeV.Text);
        if edHeC1.Text = '' then
          C1 := 0
        else begin
          if RightStr(edHeC1.Text, 1) = '%' then
            C1 := StrToFloat(Trim(LeftStr(edHeC1.Text, Length(edHeC1.Text) - 1)))
          else
            C1 := StrToFloat(edHeC1.Text);
        end;
        if edHeC2.Text = '' then
          C2 := 0
        else begin
          if RightStr(edHeC2.Text, 1) = '%' then
            C2 := StrToFloat(Trim(LeftStr(edHeC2.Text, Length(edHeC2.Text) - 1)))
          else
            C2 := StrToFloat(edHeC2.Text);
        end;
        // Basic vol/conc validity check
        if (V <= 0) or (C1 <= 0) or (C2 <= 0) then begin
          MessageDlg('Invalid user data', 'V, C1 and C2 must all be greater than zero!', mtError, [mbCancel], 0);
          Err := True;
        end
        else if C1 <= C2 then begin
          MessageDlg('Invalid user data', 'Initial He conc. must be greater than final one!', mtError, [mbCancel], 0);
          Err := True;
        end
        else begin
          if (C1 > 100) or (C2 > 100) then
             if MessageDlg('Questionable user data', 'C1 and C2 are supposed to be percent values!', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
               Err := True;
          if not Err then begin
            rFRC := V * (C1 - C2) / C2;
            edFRC.Text := FloatToStr(rFRC);
          end;
        end;
      end
      // Nitrogen washout method
      else if rbNitrogen.Checked then begin
        if edNV.Text = '' then
          V := 0
        else
          V := StrToFloat(edNV.Text);
        if edNC1.Text = '' then
          C1 := 0
        else begin
          if RightStr(edNC1.Text, 1) = '%' then
            C1 := StrToFloat(Trim(LeftStr(edNC1.Text, Length(edNC1.Text) - 1)))
          else
            C1 := StrToFloat(edNC1.Text);
        end;
        if edNC2.Text = '' then
          C2 := 0
        else begin
          if RightStr(edNC2.Text, 1) = '%' then
            C2 := StrToFloat(Trim(LeftStr(edNC2.Text, Length(edNC2.Text) - 1)))
          else
            C2 := StrToFloat(edNC2.Text);
        end;
        // Basic vol/conc validity check
        if (V <= 0) or (C1 <= 0) or (C2 <= 0) then begin
          MessageDlg('Invalid user data', 'V, C1 and C2 must all be greater than zero!', mtError, [mbCancel], 0);
          Err := True;
        end
        else if C1 >= C2 then begin
          MessageDlg('Invalid user data', 'Initial N conc. must be less than final one!', mtError, [mbCancel], 0);
          Err := True;
        end
        else begin
          if (C1 > 100) or (C2 > 100) then
            if MessageDlg('Questionable user data', 'C1 and C2 are supposed to be percent values!', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
              Err := True;
          if not Err then begin
            rFRC := C1 * V / C2;
            edFRC.Text := FloatToStr(rFRC);
          end;
        end;
      end
      // FRC is supposed to be calculated from RV
      else
        rFRC := 0;
    end
    // FRC read from corr. input field (always the case if this one contains a value)
    else begin
      rFRC := StrToFloat(edFRC.Text);
      if rFRC <= 0 then begin
        MessageDlg('Invalid user data', 'FRC must be greater than zero!', mtError, [mbCancel], 0);
        Err := True;
      end;
    end;
  end;
  // If input data validity check is ok, proceed with calculation
  // In case of warnings, user decides to proceed ("Ignore" button) or to abort ("Cancel" button)
  if not Err then begin
    // Transform all input values to ml
    rTV  *= iL; rIRV *= iL; rERV *= iL; rFRC *= iL;
    // Check input values against normal averages
    // If outside range 0.5 N .. 2 N, display warning message
    // Give user the possibility to continue ("Ignore" button) or to abort ("Cancel" button)
    if (rTV < 0.5 * Normal[iNormal].TV) or (rTV > 2 * Normal[iNormal].TV) then begin
      Mess := 'TV seems to be erroneous! Normal value = ' + FloatToStr(Normal[iNormal].TV / iL);
      if MessageDlg('Questionable user data', Mess + '.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
        Err := True;
    end;
    if not Err and ((rIRV < 0.5 * Normal[iNormal].IRV) or (rIRV > 2 * Normal[iNormal].IRV)) then begin
      Mess := 'IRV seems to be erroneous! Normal value = ' + FloatToStr(Normal[iNormal].IRV / iL);
      if MessageDlg('Questionable user data', Mess + '.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
        Err := True;
    end;
    if not Err and ((rERV < 0.5 * Normal[iNormal].ERV) or (rERV > 2 * Normal[iNormal].ERV)) then begin
      Mess := 'ERV seems to be erroneous! Normal value = ' + FloatToStr(Normal[iNormal].ERV / iL);
      if MessageDlg('Questionable user data', Mess + '.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
        Err := True;
    end;
    if not Err and ((rgRV.ItemIndex = 0) and ((rFRC < 0.5 * Normal[iNormal].FRC) or (rFRC > 2 * Normal[iNormal].FRC))) then begin
      Mess := 'FRC seems to be erroneous! Normal value = ' + FloatToStr(Normal[iNormal].FRC / iL);
      if MessageDlg('Questionable user data', Mess + '.', mtWarning, [mbCancel, mbIgnore], 0) = mrCancel then
        Err := True;
    end;
    if not Err then begin
      // Calculate lung volumes and capacities
      rIC := rTV + rIRV;
      rVC := rIRV + rTV + rERV;
      // Get RV from FRC (entered or calculated before) or by estimation formula
      if rgRV.ItemIndex = 0 then
        // RV from FRC
        rRV := rFRC - rERV
      else begin
        // RV from formula (sex dependent)
        if rgSex.ItemIndex = 0 then
          rRV := 0.24 * rVC
        else
          rRV := 0.28 * rVC;
        rFRC := rRV + rERV;                                                    // in this case, calculate FRC from RV
      end;
      // Display volumes/capacities (using volume unit selected)
      rTLC := rIRV + rTV + rERV + rRV;
      edIC.Text := FloatToStr(rIC / iL);
      edVC.Text := FloatToStr(rVC / iL);
      edFRC.Text := FloatToStr(rFRC / iL);
      edRV.Text := FloatToStr(rRV / iL);
      edTLC.Text := FloatToStr(rTLC / iL);
    end;
  end;
end;

{ "RV determination" has been changed by user }

procedure TfLV.rgRVClick(Sender: TObject);

begin
  if rgRV.ItemIndex = 1 then begin
    // "Estimated from VC" selected
    rbHelium.Checked := False;                                                 // FRC will be calculated from RV, not from measurement values
    rbNitrogen.Checked := False;
    edFRC.Text := '';                                                          // FRC input field must be cleared, otherwise its value will be used!
    edHeV.TabStop  := False; edHeC1.TabStop := False; edHeC2.TabStop := False;
    edNV.TabStop  := False;  edNC1.TabStop := False;

  end;
end;

{ "Individual's sex" has been changed by user }

procedure TfLV.rgSexClick(Sender: TObject);

begin
  iNormal := rgSex.ItemIndex;                                                  // index for normal average values
end;

{ "FRC measurement by He dilution technique" has been selected by user }

procedure TfLV.rbHeliumChange(Sender: TObject);

begin
  if rbHelium.Checked then begin
    edFRC.Text := '';                                                          // FRC input field must be cleared, otherwise its value will be used!
    if rgRV.ItemIndex = 1 then begin                                           // automatically correct RV determination selection
      MessageDlg('User input auto-update', 'RV determination will be set to "Calculate from FRC"!', mtInformation, [mbOK, mbCancel], 0);
      rgRV.ItemIndex := 0;
    end;
    edHeV.TabStop  := True;  edHeC1.TabStop := True; edHeC2.TabStop := True;
    edNV.TabStop   := False; edNC1.TabStop  := False;
    edHeV.SetFocus;
  end;
end;

{ "FRC measurement by N washout method" has been selected by user }

procedure TfLV.rbNitrogenChange(Sender: TObject);

begin
  if rbNitrogen.Checked then begin
    edFRC.Text := '';                                                          // FRC input field must be cleared, otherwise its value will be used!
    if rgRV.ItemIndex = 1 then begin                                           // automatically correct RV determination selection
      MessageDlg('User input auto-update', 'RV determination will be set to "Calculate from FRC"!', mtInformation, [mbOK, mbCancel], 0);
      rgRV.ItemIndex := 0;
    end;
    edNV.TabStop   := True;  edNC1.TabStop  := True; edHeV.TabStop  := False;
    edHeC1.TabStop := False; edHeC2.TabStop := False;
    edNV.SetFocus;
  end;
end;

{ FRC measurement vol/conc value has been changed by user }

// For convenience, auto-check corresponding radio-button
// Be sure FRC input field is empty (otherwise its content will be used instead of calculated value)!

procedure TfLV.edHeVChange(Sender: TObject);

begin
  if edHeV.Text <> '' then begin
    rbHelium.Checked := True;
    edFRC.Text := '';
  end;
end;

procedure TfLV.edHeC1Change(Sender: TObject);

begin
  if edHeC1.Text <> '' then begin
    rbHelium.Checked := True;
    edFRC.Text := '';
  end;
end;

procedure TfLV.edHeC2Change(Sender: TObject);

begin
  if edHeC2.Text <> '' then begin
    rbHelium.Checked := True;
    edFRC.Text := '';
  end;
end;

procedure TfLV.edNVChange(Sender: TObject);

begin
  if edNV.Text <> '' then begin
    rbNitrogen.Checked := True;
    edFRC.Text := '';
  end;
end;

procedure TfLV.edNC1Change(Sender: TObject);

begin
  if edNC1.Text <> '' then begin
    rbNitrogen.Checked := True;
    edFRC.Text := '';
  end;
end;

end.

