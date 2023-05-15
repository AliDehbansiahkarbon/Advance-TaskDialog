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
    function CreateVistaStyleButtons: TArray<TCommandLinkButton>;
    function GetDialogIcon(AMsgDlgType: TMsgDlgType): TAxTaskDialogIcon;


    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons ; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string): Integer; overload; //Simple
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; AFooterText: string; AFooterIcon: TAxTaskDialogFooterIcon): Integer; overload;// With Footer
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aVerifyText: string; aVerifyChecked: Boolean): Integer; overload; //With Verify Check
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aComboBoxItems: string; aIsComboEditable: Boolean; var ComboBoxSelectionResultIndex: Integer): Integer; overload; //With ComboBox
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aTextBoxDefaultText: string; var aTextBoxResultText: string): Integer; overload; //With TextBox
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aRadioButtonsText: string; var RadioButtonsResultIndex: Integer): Integer; overload; //With RadioButtons
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aCommandLinkButtons: array of TCommandLinkButton; aAddEmailButton: Boolean; aRemoveMainIcon: Boolean = True; aRemoveOsTitle: Boolean = True; aRemoveCommonButtons: Boolean = True): Integer; overload; //With CommandLinkButtons
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aExpandableText: string; aExpandedButtonTooltip: string; aCollapsedButtonTooltip: string; AAutoExpandFooter: Boolean): Integer; overload;// With Expandable Text
    function ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons = []; aErrorNumber: LongInt = 0; aFlags: TAxTaskDialogFlags = []; aDialogCaption: string =''; aTitle: string = ''; aExpandableText: string = '';
                                  aExpandedButtonTooltip: string = 'See details'; aCollapsedButtonTooltip: string = 'Hide details'; aButtonsText: string = ''; aDialogFooterIcon: TAxTaskDialogFooterIcon = tfiInformation;
                                  aRadioButtonsText: string = ''; aVerifyText: string = ''; VerifyChecked: Boolean = True; aFooterText: string = ''; aComboBoxItems: string = ''; AAutoExpandFooter: Boolean = False): Integer; overload;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function GetCommonButtons(ACommonButton: TMsgDlgButtons): TCommonButtons;
var
  LvDlgButton: TMsgDlgBtn;
begin
  Result := [];
  for LvDlgButton in ACommonButton do
  begin
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
  LvTextBoxResult: string;
  LvShortMsg, LvLongMsg: string;
begin
  LvShortMsg := 'Test Message';
  LvLongMsg := 'A TLabel Delphi component has a WordWrap property you can set to true in order for the text in the Caption property appear wrapped (multi-lined) when it is too long for the width of the label.';


  // Simple Message
  ShowApplicationException(LvLongMsg, mtError, [mbOk], 1000, 'Test Caption', 'Test Title');

  // Simple Message with Footer Text and optional Icon.
  ShowApplicationException(LvLongMsg, mtError, [mbOk], 1000, 'Test Caption', 'Test Title', 'I am footer text', tfiShield);


  // Simple Message with hidable detail panel at the bottom.
  ShowApplicationException('Operation Failed', mtError, [mbOk], 1000, 'Test Caption', 'Test Title',
                           'Cannot insert the value NULL into column ''WalletID'', table ''Wallet''; column does not allow nulls. INSERT fails','See details', 'Hide details', True);

  // Simple Message with ChaeckBox
  ShowApplicationException(LvShortMsg, mtError, [mbOk], 1000, 'Test Caption', 'Test Title', 'do not repeat again Verify', True);

  // Simple Message with Input(Tedit) box.
  ShowApplicationException(LvShortMsg, mtError, [mbOk], 1000, 'Test Caption', 'Test Title', 'Enter Text Here...', LvTextBoxResult);
  ShowMessage('Entered text: ' + LvTextBoxResult);

  // Simple Message with ComboBox in the bottom.
  ShowApplicationException(LvShortMsg, mtError, [mbOk], 1000, 'Test Caption', 'Test Title', 'ComboItem1|ComboItem2|ComboItem3', False , LvComboResult);
  ShowMessage('Selected Item Index: ' + LvComboResult.ToString);

  // Simple Message with a number of Radio items.
  ShowApplicationException(LvShortMsg, mtError, [mbOk], 1000, 'Test Caption', 'Test Title', 'RadioItem1|RadioItem2|RadioItem3', LvRadioResult);
  ShowMessage('Selected Item Index: ' + LvRadioResult.ToString);

  // Vista(and above versions) style message with three buttons.
  ShowApplicationException('Test Message Without Email Button', mtError,[mbOk],1000, 'Test Caption', '', CreateVistaStyleButtons, False);

  // Vista(and above versions) style message with three buttons plus verify check function.
  ShowApplicationException('Test Message With Email Button', mtError, [mbOk], 1000, 'Test Caption', 'Test Title', CreateVistaStyleButtons, True);

  // All non Vista Objects + Progressbar
  ShowApplicationException('This is the main text', mtError, [mbok], 1000,
  [tdfShowProgressBar, tdfShowMarqueeProgressBar, tdfPositionRelativeToWindow], 'Dialog Test Caption', 'Test Title 1000', 'Expandable Text 100',
  'See Detail', 'Hide Detail', 'Button Text', tfiInformation, 'Radio 1|Radio 2', 'Verify Text', True, 'Footer Text', 'aaa|bbb|ccc', False);

  // All non-Vista objects + Progressbar + Hyperlink
  ShowApplicationException('This is the main text', mtError, [mbok], 1000,
  [tdfShowProgressBar, tdfShowMarqueeProgressBar, tdfEnableHyperLinks, tdfPositionRelativeToWindow], 'Dialog Test Caption', 'Test Title 1000', 'Expandable Text 100',
  'See Detail', 'Hide Detail', 'Button Text', tfiInformation, 'Radio 1|Radio 2', 'Verify Text', True, 'Support: <a href="www.google.com">(XCompany)</a>', 'aaa|bbb|ccc', False);
end;

function TForm1.CreateVistaStyleButtons: TArray<TCommandLinkButton>;
begin
  SetLength(Result, 3);

  with Result[0] do
  begin
    TitleCaption := 'Save and Wait!';
    DetailCaption := 'This option will save items and do not close the dialog.';
    Hint := 'Hint for saving';
    CanCloseForm := False;
    ExtraBtnClick := TempButton1Click;
  end;

  with Result[1] do
  begin
    TitleCaption := 'Save and Exit';
    DetailCaption := 'This option will save items and exit dialog.';
    Hint := 'Hint for saving';
    CanCloseForm := True;
    ExtraBtnClick := TempButton1Click;
  end;

  with Result[2] do
  begin
    TitleCaption := 'Do not Save and Exit';
    DetailCaption := 'This option won''t save items and exit dialog.';
    Hint := 'Hint for No saving';
    CanCloseForm := True;
    ExtraBtnClick := TempButtonClick;
  end;
end;

function TForm1.GetDialogIcon(AMsgDlgType: TMsgDlgType): TAxTaskDialogIcon;
begin
  case AMsgDlgType of
    mtError: Result := tiError;
    mtWarning: Result := tiWarning;
    mtInformation: Result := tiInformation;
    mtConfirmation: Result := tiQuestion;
    mtCustom: Result := tiShield;
  end;
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
  if not aVerifyText.IsEmpty then
  begin
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
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow], GetDialogIcon(aDlgType));
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt;
                                         aDialogCaption: string; aTitle: string; AFooterText: string; AFooterIcon: TAxTaskDialogFooterIcon): Integer;
var
  Task: TAxTaskDialog;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.FooterText := AFooterText;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow] , GetDialogIcon(aDlgType), AFooterIcon);
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string;
                                  aExpandableText: string; aExpandedButtonTooltip: string; aCollapsedButtonTooltip: string; AAutoExpandFooter: Boolean): Integer;
var
  Task: TAxTaskDialog;
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

  LvFlags := [tdfExpandFooterArea, tdfExpandButton, tdfPositionRelativeToWindow];
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, LvFlags, GetDialogIcon(aDlgType));
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aVerifyText: string; aVerifyChecked: Boolean): Integer;
var
  Task: TAxTaskDialog;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.VerifyText := aVerifyText;
  Task.VerifyChecked := aVerifyChecked;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow] , GetDialogIcon(aDlgType));
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aComboBoxItems: string; aIsComboEditable: Boolean; var ComboBoxSelectionResultIndex: Integer): Integer;
var
  Task: TAxTaskDialog;
  LvFlags: AxTaskDialog.TAxTaskDialogFlags;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.ComboBoxItems := aComboBoxItems;
  LvFlags := [tdfPositionRelativeToWindow];
  if aIsComboEditable then
    LvFlags := LvFlags + [tdfUserInputeTextBox];

  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, LvFlags , GetDialogIcon(aDlgType));
  ComboBoxSelectionResultIndex := Task.ComboBoxSelectionResultIndex;
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aTextBoxDefaultText: string; var aTextBoxResultText: string): Integer;
var
  Task: TAxTaskDialog;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow, tdfUserInputeTextBox] , GetDialogIcon(aDlgType));
  aTextBoxResultText := Task.UserInputedText;
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aRadioButtonsText: string; var RadioButtonsResultIndex: Integer): Integer;
var
  Task: TAxTaskDialog;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  Task.RadioButtonsText := aRadioButtonsText;
  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, [tdfPositionRelativeToWindow] , GetDialogIcon(aDlgType));
  RadioButtonsResultIndex := Task.RadioResult;
end;

function TForm1.ShowApplicationException(const aMsg: string; aDlgType: TMsgDlgType; aCommonButton: TMsgDlgButtons; aErrorNumber: LongInt; aDialogCaption: string; aTitle: string; aCommandLinkButtons: array of TCommandLinkButton; aAddEmailButton: Boolean; aRemoveMainIcon: Boolean; aRemoveOsTitle: Boolean; aRemoveCommonButtons: Boolean): Integer;
var
  Task: TAxTaskDialog;
  LvFlags: AxTaskDialog.TAxTaskDialogFlags;
  I: Integer;
begin
  Task.DialogCaption := aDialogCaption;
  Task.Title := aTitle;
  Task.MainText := aMsg;
  if Length(aCommandLinkButtons) > 0 then
  begin
    SetLength(Task.CommandLinkButtons, length(aCommandLinkButtons));
    for I := Low(aCommandLinkButtons) to High(aCommandLinkButtons) do
    begin
      Task.CommandLinkButtons[I].TitleCaption := aCommandLinkButtons[I].TitleCaption;
      Task.CommandLinkButtons[I].DetailCaption := aCommandLinkButtons[I].DetailCaption;
      Task.CommandLinkButtons[I].Hint := aCommandLinkButtons[I].Hint;
      Task.CommandLinkButtons[I].CanCloseForm := aCommandLinkButtons[I].CanCloseForm;
      Task.CommandLinkButtons[I].ExtraBtnClick := aCommandLinkButtons[I].ExtraBtnClick;
    end;
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

  Result := Task.Execute(GetCommonButtons(aCommonButton), 0, LvFlags , GetDialogIcon(aDlgType));
end;
end.
