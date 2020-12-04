unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AxTaskDialog, Vcl.StdCtrls, System.StrUtils;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure TempButtonClick(Sender: TObject);
    procedure TempButton1Click(Sender: TObject);
  private
    { Private declarations }

    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons ; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string): Integer; overload; //Simple
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aVerifyText: string; aVerifyChecked: Boolean): Integer; overload; //With Verify Check
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aComboBoxItems: string; aIsComboEditable: Boolean; var ComboBoxSelectionResultIndex: Integer): Integer; overload; //With ComboBox
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aTextBoxDefaultText: string; var aTextBoxResultText: string): Integer; overload; //With TextBox
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aRadioButtonsText: string; var RadioButtonsResultIndex: Integer): Integer; overload; //With RadioButtons
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aCommandLinkButtons: array of TCommandLinkButton; aAddEmailButton: Boolean; aRemoveMainIcon: Boolean = True; aRemoveOsTitle: Boolean = True; aRemoveCommonButtons: Boolean = True): Integer; overload; //With CommandLinkButtons
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aExpandableText: string; aExpandedButtonTooltip: string; aCollapsedButtonTooltip: string; AAutoExpandFooter: Boolean): Integer; overload;// With Expandable Text
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons = []; aErrorNumber: LongInt = 0; aFlags: TAxTaskDialogFlags = []; aDialogCaption: string =''; aTitle: string = ''; aExpandableText: string = '';
                                  aExpandedButtonTooltip: string = 'See details'; aCollapsedButtonTooltip: string = 'Hide details'; aButtonsText: string = ''; aDialogFooterIcon: TAxTaskDialogFooterIcon = tfiInformation;
                                  aRadioButtonsText: string = ''; aVerifyText: string = ''; VerifyChecked: Boolean = True; aFooterText: string = ''; aComboBoxItems: string = ''; AAutoExpandFooter: Boolean = False): Integer; overload;
     // Full parameters

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetCommonButtons(aCommonButton: TMsgDlgButtons): TCommonButtons;
var
  LvDlgButton: TMsgDlgBtn;
begin
  Result := [];
  for LvDlgButton in aCommonButton do begin
    case Ord(LvDlgButton) of
      0: Result := Result + [cbYes];
      1: Result := Result + [cbNo];
      2: Result := Result + [cbOK];
      3: Result := Result + [cbCancel];
      5: Result := Result + [cbRetry];
      11: Result := Result + [cbClose];
    end;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  LvComboResult, LvRadioResult: Integer;
  MyButtonsArray : array of TCommandLinkButton;
  LvTextBoxResult: string;
begin
 ShowApplicationException('Test Message',mtError,[mbOk],1000,'Test Caption','Test Title');
 ShowApplicationException('Test Message',mtError,[mbOk],1000,'Test Caption','Test Title', DupeString('Test Expand Text \n',10),'See details', 'Hide details',True);
 ShowApplicationException('Test Message',mtError,[mbOk],1000,'Test Caption','Test Title','do not repeat again Verify', True);
 ShowApplicationException('Test Message',mtError,[mbOk],1000,'Test Caption','Test Title','Enter Text Here...', LvTextBoxResult);
 ShowMessage('Entered text: ' + LvTextBoxResult);

 ShowApplicationException('Test Message',mtError,[mbOk],1000,'Test Caption','Test Title','ComboItem1|ComboItem2|ComboItem3', False , LvComboResult);
 ShowMessage('Selected Item Index: ' + LvComboResult.ToString);

 ShowApplicationException('Test Message',mtError,[mbOk],1000,'Test Caption','Test Title','RadioItem1|RadioItem2|RadioItem3', LvRadioResult);
 ShowMessage('Selected Item Index: ' + LvRadioResult.ToString);

 SetLength(MyButtonsArray, 3);

 with MyButtonsArray[0] do
 begin
   TitleCaption := 'Save and Wait!';
   DetailCaption := 'This option will save items and do not close the dialog.';
   Hint := 'Hint for saving';
   CanCloseForm := False;
   ExtraBtnClick := TempButton1Click;
 end;

 with MyButtonsArray[1] do
 begin
   TitleCaption := 'Save and Exit';
   DetailCaption := 'This option will save items and exit dialog.';
   Hint := 'Hint for saving';
   CanCloseForm := True;
   ExtraBtnClick := TempButton1Click;
 end;

 with MyButtonsArray[2] do
 begin
   TitleCaption := 'Do not Save and Exit';
   DetailCaption := 'This option won''t save items and exit dialog.';
   Hint := 'Hint for No saving';
   CanCloseForm := True;
   ExtraBtnClick := TempButtonClick;
 end;

 ShowApplicationException('Test Message Without Email Button',mtError,[mbOk],1000,'Test Caption','', MyButtonsArray, False);
 ShowApplicationException('Test Message With Email Button',mtError,[mbOk],1000,'Test Caption','Test Title', MyButtonsArray, True);
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aFlags: TAxTaskDialogFlags; aDialogCaption: string; aTitle: string; aExpandableText: string;
                                  aExpandedButtonTooltip: string; aCollapsedButtonTooltip: string; aButtonsText: string; aDialogFooterIcon: TAxTaskDialogFooterIcon;
                                  aRadioButtonsText: string; aVerifyText: string; VerifyChecked: Boolean; aFooterText: string; aComboBoxItems: string; AAutoExpandFooter: Boolean): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
  function GetFlags: AxTaskDialog.TAxTaskDialogFlags;
  var
    LvFlag: TAxTaskDialogFlag;
  begin
    Result := [];
    if aFlags = [] then
      aFlags := [tdfEnableHyperLinks];
    for LvFlag in aFlags do
      case Ord(LvFlag) of
        0: Result := Result + [tdfEnableHyperLinks];
        1: Result := Result + [tdfUseCommandLinks];
        2: Result := Result + [tdfUseCommandLinksNoIcon];
        3: Result := Result + [tdfExpandFooterArea];
        4: Result := Result + [tdfExpandByDefault];
        5: Result := Result + [tdfShowProgressBar];
        6: Result := Result + [tdfShowMarqueeProgressBar];
        7: Result := Result + [tdfCallbackTimer];
        8: Result := Result + [tdfPositionRelativeToWindow];
        9: Result := Result + [tdfRtlLayout];
        10: Result := Result + [tdfNoDefaultRadioButton];
        11: Result := Result + [tdfCanBeMinimized];
        12: Result := Result + [tdfUserInputeTextBox];
        13: Result := Result + [tdfExpandButton];
        14: Result := Result + [tdfEmailButton];
        15: Result := Result + [tdfRemoveMainIcon];
      end;
  end;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.RadioButtonsText := aRadioButtonsText;
  if not aVerifyText.IsEmpty then begin
    Task.VerifyText := '';
    Task.VerifyChecked := True;
  end;
    Task.FooterText := aFooterText;
  Task.ComboBoxItems := aComboBoxItems;
  Task.ExpandableText := aExpandableText;
  Task.ExpandedButtonTooltip := aExpandedButtonTooltip;
  Task.CollapsedButtonTooltip := aCollapsedButtonTooltip;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, GetFlags , LvDialogIcon, TAxTaskDialogFooterIcon(Ord(aDialogFooterIcon)), 200, 0);
end;

procedure TForm1.TempButtonClick(Sender: TObject);
begin
  ShowMessage('Hello!');
end;

procedure TForm1.TempButton1Click(Sender: TObject);
begin
  ShowMessage('Saved successfully');
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow] , LvDialogIcon);
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string;
                                  aExpandableText: string; aExpandedButtonTooltip: string; aCollapsedButtonTooltip: string; AAutoExpandFooter: Boolean): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
  LvFlags: AxTaskDialog.TAxTaskDialogFlags;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.ExpandableText := aExpandableText;
  Task.ExpandedButtonTooltip := aExpandedButtonTooltip;
  Task.CollapsedButtonTooltip := aCollapsedButtonTooltip;
  if AAutoExpandFooter then
    LvFlags := LvFlags + [tdfExpandByDefault];
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  LvFlags := [tdfExpandFooterArea, tdfExpandButton, tdfPositionRelativeToWindow];
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, LvFlags, LvDialogIcon);
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aVerifyText: string; aVerifyChecked: Boolean): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.VerifyText := aVerifyText;
  Task.VerifyChecked := aVerifyChecked;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow] , LvDialogIcon);
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aComboBoxItems: string; aIsComboEditable: Boolean; var ComboBoxSelectionResultIndex: Integer): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
  LvFlags: AxTaskDialog.TAxTaskDialogFlags;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.ComboBoxItems := aComboBoxItems;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  LvFlags := [tdfPositionRelativeToWindow];
  if aIsComboEditable then
    LvFlags := LvFlags + [tdfUserInputeTextBox];
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, LvFlags , LvDialogIcon);
  ComboBoxSelectionResultIndex := Task.ComboBoxSelectionResultIndex;
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aTextBoxDefaultText: string; var aTextBoxResultText: string): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow, tdfUserInputeTextBox] , LvDialogIcon);
  aTextBoxResultText := Task.UserInputedText;
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aRadioButtonsText: string; var RadioButtonsResultIndex: Integer): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.RadioButtonsText := aRadioButtonsText;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow] , LvDialogIcon);
  RadioButtonsResultIndex := Task.RadioResult;
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aCommandLinkButtons: array of TCommandLinkButton; aAddEmailButton: Boolean; aRemoveMainIcon: Boolean; aRemoveOsTitle: Boolean; aRemoveCommonButtons: Boolean): Integer;
var
  Task: TAxTaskDialog;
  LvDialogIcon : TAxTaskDialogIcon;
  LvFlags: AxTaskDialog.TAxTaskDialogFlags;
  I: Integer;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  if Length(aCommandLinkButtons) > 0 then begin
    SetLength(Task.CommandLinkButtons, length(aCommandLinkButtons));
    for I := Low(aCommandLinkButtons) to High(aCommandLinkButtons) do begin
      Task.CommandLinkButtons[I].TitleCaption := aCommandLinkButtons[I].TitleCaption;
      Task.CommandLinkButtons[I].DetailCaption := aCommandLinkButtons[I].DetailCaption;
      Task.CommandLinkButtons[I].Hint := aCommandLinkButtons[I].Hint;
      Task.CommandLinkButtons[I].CanCloseForm := aCommandLinkButtons[I].CanCloseForm;
      Task.CommandLinkButtons[I].ExtraBtnClick := aCommandLinkButtons[I].ExtraBtnClick;
    end;
  end;
  case aDlgType of
    mtError: LvDialogIcon := tiError;
    mtWarning: LvDialogIcon := tiWarning;
    mtInformation: LvDialogIcon := tiInformation;
    mtConfirmation: LvDialogIcon := tiQuestion;
    mtCustom: LvDialogIcon := tiShield;
  end;
  LvFlags := [tdfPositionRelativeToWindow, tdfUseCommandLinks];
  if aAddEmailButton then
    LvFlags := LvFlags + [tdfEmailButton];
  if aRemoveMainIcon then
    LvFlags := LvFlags + [tdfRemoveMainIcon];
  if aRemoveOsTitle then
    LvFlags := LvFlags + [tdfDoNotUseDefaultTitle];
  if aRemoveCommonButtons then
    LvFlags := LvFlags + [tdfRemoveCommonButtons];
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, LvFlags , LvDialogIcon);
end;
end.
