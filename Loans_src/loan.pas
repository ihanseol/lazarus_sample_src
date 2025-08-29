{***********************************}
{* Main unit for Loans application *}
{***********************************}

unit loan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, Math, addon, amortized, compound, help;

type
  {*********}
  { TfLoans }
  {*********}
  TfLoans = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsCalculation, mSettingsCalculation1, mSettingsCalculation2: TMenuItem;
    Help, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label5: TLabel;
    Label7, Label8, Label9, Label10: TLabel;
    rbAddon, rbDiscount, rbAmortized, rbCompound: TRadioButton;
    edDescription: TMemo;
    Shape1: TShape;
    laPeriods, laCompounds: TLabel;
    edInterest, edPrincipal, edPMT, edPeriods, edCompounds: TEdit;
    edNetAmount: TEdit;
    edTotalInterest: TEdit;
    edTotalPayback: TEdit;
    edInterestEff: TEdit;
    btCalc: TButton;
    btDetails: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsCalculation1Click(Sender: TObject);
    procedure mSettingsCalculation2Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btDetailsClick(Sender: TObject);
    procedure rbAddonChange(Sender: TObject);
    procedure rbDiscountChange(Sender: TObject);
    procedure rbAmortizedChange(Sender: TObject);
    procedure rbCompoundChange(Sender: TObject);
  private
    rR, rN, rP, rT, rPMT, rI: Real;
    sMethod, sCalculation: string;
  end;

var
  fLoans: TfLoans;

implementation

{$R *.lfm}

{*********}
{ TfLoans }
{*********}

{ Application start: Initializations }

procedure TfLoans.FormCreate(Sender: TObject);

begin
  sMethod := 'addon'; sCalculation := 'periods';
end;

{ Menu item "File > Exit: Exit application" }

procedure TfLoans.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Calculation > ...": Choose between calculation for given period resp. for given repayment }

procedure TfLoans.mSettingsCalculation1Click(Sender: TObject);

begin
  mSettingsCalculation1.Checked := True;
  mSettingsCalculation2.Checked := False;
  sCalculation := 'amount';
  edPMT.ReadOnly := False; edPMT.TabStop := True; edPMT.Color := clDefault;
  edPeriods.Text := ''; edPeriods.ReadOnly := True; edPeriods.Color := clMoneyGreen;
  laPeriods.Caption := 'Periods (months)';
end;

procedure TfLoans.mSettingsCalculation2Click(Sender: TObject);

begin
  mSettingsCalculation1.Checked := False;
  mSettingsCalculation2.Checked := True;
  sCalculation := 'periods';
  edPMT.Text := ''; edPMT.ReadOnly := True; edPMT.TabStop := False; edPMT.Color := clMoneyGreen;
  edPeriods.ReadOnly := False; edPeriods.Color := clDefault;
  laPeriods.Caption := 'Periods (years)';
end;

{ Menu item "Help > Help: Display application help" }

procedure TfLoans.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About: Display application about" }

procedure TfLoans.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Financial mathematics: Interests and loan repayment.' + LineEnding;
  S += 'Determination of the monthly payment resp. of the number of payment periods for a given amount borrowed at a given ';
  S += 'interest rate. The application supports the following interest methods: Simple add-on interest, simple discount interest, ';
  S += 'simple interest amortized loan, compound interest.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2021 - August 2023.';
  MessageDlg('About "Loans"', S, mtInformation, [mbOK], 0);
end;


{ Button "Calculate" pushed: Do calculations for actually selected interest method }


procedure TfLoans.btCalcClick(Sender: TObject);

var
  Repayment, NetAmount, EffInterest, Balance: Real;
  Mess: string;

begin
  // Read user values from form
  if edInterest.Text = '' then
    rR := 0
  else
    rR := StrToFloat(edInterest.Text) / 100;
  if sMethod = 'compound' then begin
    if edCompounds.Text = '' then
      rN := 0
    else
      rN := StrToFloat(edCompounds.Text);
  end;
  if edPrincipal.Text = '' then
    rP := 0
  else
    rP := StrToFloat(edPrincipal.Text);
  if sCalculation = 'amount' then begin
    if edPMT.Text = '' then
      rPMT := 0
    else
      rPMT := StrToFloat(edPMT.Text);
  end
  else begin
    if edPeriods.Text = '' then
      rT := 0
    else
      rT := StrToFloat(edPeriods.Text);;
  end;
  Mess := '';
  if rR <= 0 then begin
    Mess := 'Annual interest rate is invalid!';
    edInterest.SetFocus;
  end
  else if (sMethod = 'compound') and (rN <= 0) then begin
    Mess := 'Number of compounds is invalid!';
    edCompounds.SetFocus;
  end
  else if rP <= 0 then begin
    Mess := 'Principle is invalid!';
    edPrincipal.SetFocus;
  end
  else if (sCalculation = 'amount') and (rPMT <= 0) then begin
    Mess := 'Monthly payment is invalid!';
    edPMT.SetFocus;
  end
  else if (sCalculation = 'periods') and (rT <= 0) then begin
    Mess := 'Number of periods is invalid!';
    edPeriods.SetFocus;
  end;
  // If user data is ok, do the calculations
  if Mess = '' then begin
    edNetAmount.Text := ''; edTotalInterest.Text := ''; edTotalPayBack.Text := ''; edInterestEff.Text := '';
    if (sMethod = 'addon') or (sMethod = 'discount') then begin
      // Simple add-on and discount interest
      if sCalculation = 'periods' then begin
        // Determine number of periods for given repayment
        rI := rT * rR * rP; rI := Round(100 * rI) / 100;
        if sMethod = 'addon' then
          Repayment := rP + rI
        else
          Repayment := rP;
        rPMT := Repayment / (12 * rT); rPMT := Round(100 * rPMT) / 100;
        edPMT.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
      end
      else begin
        // Determine repayment for given number of periods
        Balance := rP; rT := 0;
        while Balance >= 0.01 do begin
          if sMethod = 'addon' then
            Balance -= (rPMT - (rR / 12) * rP)
          else
            Balance -= rPMT;
          rT += 1;
        end;
        edPeriods.Text := FloatToStrF(rT, ffFixed, 0, 0);
        rT /= 12;
        rI := rT * rR * rP; rI := Round(100 * rI) / 100;
        if sMethod = 'addon' then
          Repayment := rP + rI
        else
          Repayment := rP;
      end;
      if sMethod = 'addon' then
        NetAmount := rP
      else begin
        NetAmount := rP - rI;
        if NetAmount <= 0 then begin
          // Not possible to make a discount interest loan if the interests are greater than the principal!
          Mess := 'Invalid parameters for a discount interest loan!';
        end;
      end;
    end
    else if sMethod = 'amortized' then begin
      // Simple interest amortized loan
      if sCalculation = 'periods' then begin
        // Determine number of periods for given repayment
        rPMT := Round(100 * (rP * (rR / 12) * power(1 + rR / 12, 12 * rT)) / (power(1 + rR / 12, 12 * rT) - 1)) / 100;
        Repayment := rPMT * 12 * rT; Repayment := Round(100 * Repayment) / 100;
        rI := Repayment - rP;
        edPMT.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
      end
      else begin
        // Determine repayment for given number of periods
        rT := Round(100 * (-Log10(1 - ( rP * rR / (12 * rPMT))) / (12 * Log10(1 + (rR / 12))))) / 100;
        Repayment := 12 * rPMT * rT; Repayment := Round(100 * Repayment) / 100;
        if Repayment / (12 * rPMT) > Round(rT) then
          rT += 1 / 12;
        edPeriods.Text := FloatToStrF(12 * rT, ffFixed, 0, 0);
        rI := Repayment - rP;
      end;
      NetAmount := rP;
    end
    else begin
      // Compound interest
      if sCalculation = 'periods' then begin
        // Determine number of periods for given repayment
        Repayment := rP * power(1 + rR / rN, rT * rN); Repayment := Round(100 * Repayment) / 100;
        rI := Repayment - rP;
        rPMT := Repayment / (12 * rT); rPMT := Round(100 * rPMT) / 100;
        edPMT.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
      end
      else begin
        // Determine repayment for given number of periods: NOT implemented!
        Mess := 'Calculation for a given repayment amount is not implemented in this version!';
        MessageDlg('Compound interest', Mess, mtWarning, [mbOK], 0);
        btDetails.Enabled := False;
      end;
      NetAmount := rP;
    end;
    // If there was no input or calculation problem, display the calculated values
    if Mess = '' then begin
      EffInterest := rI / NetAmount / rT;
      edTotalInterest.Text := FloatToStrF(rI, ffFixed, 0, 2);
      edTotalPayback.Text := FloatToStrF(Repayment, ffFixed, 0, 2);
      edNetAmount.Text := FloatToStrF(NetAmount, ffFixed, 0, 2);
      edInterestEff.Text := FloatToStrF(100 * EffInterest, ffFixed, 0, 2);
      btDetails.Enabled := True;
    end;
  end
  // If user data is invalid, display error message
  else
    MessageDlg('Data error', Mess, mtError, [mbOK], 0);
end;

{ Button "Details" pushed: Display the form corresponding to the interest method actually selected }

procedure TfLoans.btDetailsClick(Sender: TObject);

begin
  if sMethod = 'amortized' then begin
    // Simple interest amortized loan
    fAmortized.rP := rP; fAmortized.edPrincipal.Text := FloatToStrF(rP, ffFixed, 0, 0);
    fAmortized.rR  := rR; fAmortized.edInterest.Text := FloatToStrF(rR * 100, ffFixed, 0, 2) + '%';
    fAmortized.rI := rI;
    fAmortized.rT := rT;
    fAmortized.rPMT := rPMT;
    if sCalculation = 'amount' then begin
      // Given monthly payment
      fAmortized.laCalculation.Caption := 'Monthly payment';
      fAmortized.edCalculation.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
      fAmortized.laCalculation2.Caption := 'Periods (months)';
      fAmortized.edCalculation2.Text := FloatToStrF(12 * rT, ffFixed, 0, 0);
    end
    else begin
      // Given number of periods
      fAmortized.laCalculation.Caption := 'Periods (years)';
      fAmortized.edCalculation.Text := FloatToStrF(rT, ffFixed, 0, 0);
      fAmortized.laCalculation2.Caption := 'Monthly payment';
      fAmortized.edCalculation2.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
    end;
    // Display the "Simple interest amortized loan" form
    fAmortized.ShowModal;
  end
  else if (sMethod = 'addon') or (sMethod = 'discount') then begin
    // Simple add-on or discount interest
    if sMethod = 'addon' then
      fAddon.Caption := 'Load repayment: Simple addon interest'
    else
      fAddon.Caption := 'Load repayment: Simple discount interest';
    fAddon.sMethod := sMethod;
    fAddon.rP := rP; fAddon.edPrincipal.Text := FloatToStrF(rP, ffFixed, 0, 0);
    fAddon.rR := rR;  fAddon.edInterest.Text  := FloatToStrF(rR * 100, ffFixed, 0, 2) + '%';
    fAddon.rI:= rI;
    fAddon.rT := rT;
    fAddon.rPMT := rPMT;
    if sCalculation = 'amount' then begin
      // Given monthly payment
      fAddon.laCalculation.Caption := 'Monthly payment';
      fAddon.edCalculation.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
      fAddon.laCalculation2.Caption := 'Periods (months)';
      fAddon.edCalculation2.Text := FloatToStrF(12 * rT, ffFixed, 0, 0);
    end
    else begin
      // Given number of periods
      fAddon.laCalculation.Caption := 'Periods (years)';
      fAddon.edCalculation.Text := FloatToStrF(rT, ffFixed, 0, 0);
      fAddon.laCalculation2.Caption := 'Monthly payment';
      fAddon.edCalculation2.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
    end;
    // Display the "Simple add-on/discount interest" form
    fAddon.ShowModal;
  end
  else begin
    // Compound interest
    fCompound.rP := rP; fCompound.edPrincipal.Text := FloatToStrF(rP, ffFixed, 0, 0);
    fCompound.rR  := rR; fCompound.edInterest.Text := FloatToStrF(rR * 100, ffFixed, 0, 2) + '%';
    fCompound.rT := rT;
    fCompound.laCalculation.Caption := 'Periods (years)';
    fCompound.edCalculation.Text := FloatToStrF(rT, ffFixed, 0, 0);
    fCompound.rN := rN; fCompound.edCompounds.Text := FloatToStrF(rN, ffFixed, 0, 0);
    fCompound.rPMT := rPMT;
    fCompound.laCalculation2.Caption := 'Monthly payment';
    fCompound.edCalculation2.Text := FloatToStrF(rPMT, ffFixed, 0, 2);
    fCompound.rI := rI;
    // Display the "Compound interest" form
    fCompound.ShowModal;
  end;
  btDetails.Enabled := False;
end;

{ Interest method selection (radiobutton checked by user) }

procedure TfLoans.rbAddonChange(Sender: TObject);

var
  S: AnsiString;

begin
  if rbAddon.Checked then begin
    laCompounds.Visible := False; edCompounds.Visible := False;
    sMethod := 'addon';
    S := 'The simple interest add-on rate method is widely used in consumer credit and financing, and the borrowing is repaid through ';
    S += 'monthly installments over a stated number of years. The computation is based on the simple formula: ' + LineEnding;
    S += '  Interest = Principle x Rate x Time' + LineEnding;
    S += 'The periodical apportioning of interest sum charged is calculated based on the "Rule 78" principle.';
    edDescription.Lines.Clear;
    edDescription.Lines.AddText(S);
  end;
end;

procedure TfLoans.rbDiscountChange(Sender: TObject);

var
  S: AnsiString;

begin
  if rbDiscount.Checked then begin
    laCompounds.Visible := False; edCompounds.Visible := False;
    sMethod := 'discount';
    S := 'The simple interest discount rate method is similar to the simple interest add-on rate method, except that instead of ';
    S += 'adding on the interest sum to the borrowing amount, it is deducted from it upfront. The computation of the interest and ';
    S += 'of the periodical apportioning of interest sum charged is the same as for simple add-on interest. ' + LineEnding;
    S += 'But, the net amount borrowed is equal to principal - interest. The effective interest rate will be higher than the rate stated.';
    edDescription.Lines.Clear;
    edDescription.Lines.AddText(S);
  end;
end;

procedure TfLoans.rbAmortizedChange(Sender: TObject);

var
  S: AnsiString;

begin
  if rbAmortized.Checked then begin
    laCompounds.Visible := False; edCompounds.Visible := False;
    sMethod := 'amortized';
    S := 'The simple interest amortized loan is the standard for larger purchases, such as in buying homes or new cars. Whereas ';
    S += 'add-on loans interest charged each payment period is based on the original amount borrowed, with simple amortized loans, ';
    S += 'interest is paid only on the outstanding balance. As the borrower makes payments on the loan, the outstanding principal ';
    S += 'decreases, and accordingly does the interest to pay. The effective interest rate will be lower than the rate stated.';
    edDescription.Lines.Clear;
    edDescription.Lines.AddText(S);
  end;
end;

procedure TfLoans.rbCompoundChange(Sender: TObject);

var
  S: AnsiString;

begin
  if rbCompound.Checked then begin
    laCompounds.Visible := True; edCompounds.Visible := True;
    sMethod := 'compound';
    S := 'With compound interest, the interest amount computed for each compounding period is capitalized to form a subsequent increasing ';
    S += 'principal sum, which is used to compute the next interest amount due (interest paid on interest; German: Zinseszins).';
    S += 'Compounding interest rate is commonly used in computing monthly loan repayment such as housing loan. The effective interest ';
    S += 'rate will be higher than the rate stated and increases with the number of yearly compound periods.';
    edDescription.Lines.Clear;
    edDescription.Lines.AddText(S);
  end;
end;

end.

