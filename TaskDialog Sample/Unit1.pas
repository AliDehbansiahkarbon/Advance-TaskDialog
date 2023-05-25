unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AxTaskDialog, Vcl.StdCtrls, System.StrUtils;

type
  TMsgCategory = (mcSimple, mcFooter, mcVerifyCheckBox, mcComboBox, mcInputText, mcRadioButtons,
                  mcExpandableText, mcVistaStyleButtons, mcVistaStyleButtonsEmail, mcProgressBar, mcHyperLink, mcNone);

  TMsgCategories = set of TMsgCategory;

  TMsgInfo = record
    MsgCategory: TMsgCategory;
    Caption: string;
    CaptionPersian: string;
    Title: string;
    TitlePersian: string;
    Footer: string;
    FooterPersian: string;
    ShortMsg: string;
    ShortMsgPersian: string;
    LongMsg: string;
    LongMsgPersian: string;
    DialogFlags: TAxTaskDialogFlags;
    DlgType: TMsgDlgType;
    CommonButton: TMsgDlgButtons;
    ErrorNumber: Integer;
    FooterIcon: TAxTaskDialogFooterIcon;
    ExpandableText: string;
    ExpandableTextPersian: string;
    ExpandedButtonTooltip: string;
    CollapsedButtonTooltip: string;
    ExpandedButtonTooltipPersian: string;
    CollapsedButtonTooltipPersian: string;
    AutoExpandFooter: Boolean;
    VerifyText: string;
    VerifyTextPersian: string;
    VerifyChecked: Boolean;
    ComboBoxItems: string;
    ComboBoxItemsPersian: string;
    IsComboEditable: Boolean;
    RadioButtonsItems: string;
    RadioButtonsItemsPersian: string;
    CommandLinkButtons: TArray<TCommandLinkButton>;

    AddEmailButton: Boolean;
    RemoveMainIcon: Boolean;
    RemoveOsTitle: Boolean;
    RemoveCommonButtons: Boolean;
    Procedure Init;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    chk_RightToLeft: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure ExtraButtonClick(Sender: TObject);
    procedure TempButtonClickSave(Sender: TObject);
  private
    { Private declarations }
    procedure PrepareMsgInfo(var AMsgInfo: TMsgInfo; AMsgCategory: TMsgCategories; ARightToLeft: Boolean = False);
    function CreateVistaStyleButtons(ARightToLeft: Boolean): TArray<TCommandLinkButton>;
    function GetDialogIcon(AMsgDlgType: TMsgDlgType): TAxTaskDialogIcon;

    function ShowTaskDialog(AMsgInfo: TMsgInfo; AMsgCategory: TMsgCategory; ARightToLeft: Boolean = False): Integer;
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
  LvMsgInfo: TMsgInfo;
begin
  ShowTaskDialog(LvMsgInfo, mcSimple);// Simple Message
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcSimple, True);// Simple Message Right To Left
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcFooter);// Simple Message with Footer Text and optional Icon.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcFooter, True);// Simple Message with Footer Text and optional Icon. - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcExpandableText);// Simple Message with expandable detail panel at the bottom.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcExpandableText, True); // Simple Message with expandable detail panel at the bottom. -RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcVerifyCheckBox);// Simple Message with CheckBox
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcVerifyCheckBox, True);// Simple Message with CheckBox - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcInputText);// Simple Message with Input(Tedit) box.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcInputText, True);// Simple Message with Input(Tedit) box. - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcComboBox);// Simple Message with ComboBox in the bottom.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcComboBox, True);// Simple Message with ComboBox in the bottom. - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcRadioButtons);// Simple Message with a number of Radio items.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcRadioButtons, True);// Simple Message with a number of Radio items. - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcVistaStyleButtons);// Vista(and above versions) style message with three buttons and without email button.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcVistaStyleButtons, True);// Vista(and above versions) style message with three buttons and without email button. - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcVistaStyleButtonsEmail);// Vista(and above versions) style message with three buttons and wit email button.
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcVistaStyleButtonsEmail, True);// Vista(and above versions) style message with three buttons and wit email button. - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcProgressBar);// All non Vista Objects + Progressbar
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcProgressBar, True);// All non Vista Objects + Progressbar - RightToLeft
//==============================================================================

  ShowTaskDialog(LvMsgInfo, mcHyperLink);// All non-Vista objects + Progressbar + Hyperlink
  if chk_RightToLeft.Checked then
    ShowTaskDialog(LvMsgInfo, mcHyperLink, True);// All non-Vista objects + Progressbar + Hyperlink - RightToLeft
end;

function TForm1.CreateVistaStyleButtons(ARightToLeft: Boolean): TArray<TCommandLinkButton>;
begin
  SetLength(Result, 3);

  with Result[0] do
  begin
    TitleCaption := IfThen(ARightToLeft, 'ذخیره', 'Save and Wait!');
    DetailCaption := IfThen(ARightToLeft, 'این گزینه عملیات ذخیره را انجام میدهد  و منتظر می ماند', 'This option will save items and do not close the dialog.');
    Hint := IfThen(ARightToLeft, 'توضیحات دیگر', 'Hint for saving');
    CanCloseForm := False;
    ExtraBtnClick := TempButtonClickSave;
  end;

  with Result[1] do
  begin
    TitleCaption := IfThen(ARightToLeft, 'ذخیره و خروج', 'Save and Exit');
    DetailCaption := IfThen(ARightToLeft, 'این گزینه ذخیره را انجام میدهد و خارج میشود', 'This option will save items and exit dialog.');
    Hint := IfThen(ARightToLeft, 'توضیحات دیگر', 'Hint for saving');
    CanCloseForm := True;
    ExtraBtnClick := TempButtonClickSave;
  end;

  with Result[2] do
  begin
    TitleCaption := IfThen(ARightToLeft, 'خروج بدون ذخیره کردن', 'Do not Save and Exit');
    DetailCaption := IfThen(ARightToLeft, 'ذخیره نمیکند و خارج میشود', 'This option won''t save items and exit dialog.');
    Hint := IfThen(ARightToLeft, 'توضیحات دیگر', 'Hint for No saving');
    CanCloseForm := True;
    ExtraBtnClick := ExtraButtonClick;
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
    else
      Result := tiShield;
  end;
end;

procedure TForm1.PrepareMsgInfo(var AMsgInfo: TMsgInfo; AMsgCategory: TMsgCategories; ARightToLeft: Boolean);
begin
  AMsgInfo.Init;
  AMsgInfo.DialogFlags := [tdfPositionRelativeToWindow];
  AMsgInfo.DlgType := mtError;
  AMsgInfo.CommonButton := [mbOk];
  AMsgInfo.ErrorNumber := 1000;

  AMsgInfo.Caption := 'Test Caption';
  AMsgInfo.CaptionPersian := 'کپشن تست';
  AMsgInfo.Title := 'Test Title';
  AMsgInfo.TitlePersian := 'عنوان تست';
  AMsgInfo.Footer := 'I am footer text!';
  AMsgInfo.FooterPersian:= 'من فوتر هستم!';

  AMsgInfo.ShortMsg := 'Test short message Message';
  AMsgInfo.ShortMsgPersian := 'متن تستی';

  AMsgInfo.LongMsg := 'A TLabel Delphi component has a WordWrap property you can set to true in order for the text in the Caption property appear wrapped (multi-lined) when it is too long for the width of the label.';
  AMsgInfo.LongMsgPersian := 'یک جزء TLabel Delphi دارای یک ویژگی WordWrap است که می توانید آن را روی true تنظیم کنید تا متن موجود در ویژگی Caption به صورت پیچیده (چند خطی) در زمانی که برای عرض برچسب بیش از حد طولانی است ظاهر شود.';

  if mcExpandableText in AMsgCategory then
  begin
    AMsgInfo.ShortMsg := 'Operation Failed';
    AMsgInfo.ExpandableText := 'Cannot insert the value NULL into column ''WalletID'', table ''Wallet''; column does not allow nulls. INSERT fails';
    AMsgInfo.ExpandedButtonTooltip := 'See details';
    AMsgInfo.CollapsedButtonTooltip := 'Hide details';

    AMsgInfo.ExpandableTextPersian := 'عملیات ناموفق بود' + ' جزییات: ' + #13 + AMsgInfo.ExpandableText;
    AMsgInfo.ExpandedButtonTooltipPersian := 'مشاهده ی جزییات';
    AMsgInfo.CollapsedButtonTooltipPersian := 'مشاهده ی جزییات';

    AMsgInfo.AutoExpandFooter := True;
  end;

  if mcVerifyCheckBox in AMsgCategory then
  begin
    AMsgInfo.VerifyText := 'Are you sure?';
    AMsgInfo.VerifyTextPersian := 'آیا اطمینان دارید؟';
    AMsgInfo.VerifyChecked := True;
  end;

  if mcComboBox in AMsgCategory then
  begin
    AMsgInfo.ComboBoxItems := 'Combo1|Combo2|Combo3';
    AMsgInfo.ComboBoxItemsPersian := 'گزینه یک|گزینه دو|گزینه سه';
    AMsgInfo.IsComboEditable := True; // Or False
  end;

  if mcRadioButtons in AMsgCategory then
  begin
    AMsgInfo.RadioButtonsItems := 'Raio1|Radio2|Radio3';
    AMsgInfo.RadioButtonsItemsPersian := 'گزینه یک|گزینه دو|گزینه سه';
  end;

  if (mcVistaStyleButtons in AMsgCategory) or (mcVistaStyleButtonsEmail in AMsgCategory) then
  begin
    AMsgInfo.ShortMsg := IfThen(ARightToLeft, 'متن تستی به همراه دکمه ی ایمیل', 'Test Message With Email Button');
    AMsgInfo.CommandLinkButtons := CreateVistaStyleButtons(ARightToLeft);

    AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfUseCommandLinks];
    AMsgInfo.AddEmailButton := mcVistaStyleButtonsEmail in AMsgCategory;
    AMsgInfo.RemoveMainIcon := False;
    AMsgInfo.RemoveOsTitle := True;
    AMsgInfo.RemoveCommonButtons := True;
  end;

  if mcHyperLink in AMsgCategory then
  begin
    AMsgInfo.Footer := 'Support: <a href="www.google.com">(XCompany)</a>';
    AMsgInfo.FooterPersian := 'پشتیبانی : <a href="www.google.com">(شرکت ایکس)</a>';
  end;

  if mcFooter in AMsgCategory then
    AMsgInfo.FooterIcon := tfiShield;
end;

function TForm1.ShowTaskDialog(AMsgInfo: TMsgInfo; AMsgCategory: TMsgCategory; ARightToLeft: Boolean): Integer;
var
  I: Integer;
  Task: TAxTaskDialog;

  function GetFlags: AxTaskDialog.TAxTaskDialogFlags;
  var
    LvFlag: TAxTaskDialogFlag;
  begin
    Result := [];
    for LvFlag in AMsgInfo.DialogFlags do
    begin
      case Ord(LvFlag) of
        0: Result := Result + [tdfEnableHyperLinks];
        1: Result := Result + [tdfUseCommandLinks];
        2: Result := Result + [tdfUseCommandLinksNoIcon];
        3: Result := Result + [tdfExpandFooterArea];
        4: Result := Result + [tdfExpandByDefault];
        5: Result := Result + [tdfShowProgressBar];
        6: Result := Result + [tdfShowMarqueeProgressBar];
        7: Result := Result + [tdfPositionRelativeToWindow];
        8: Result := Result + [tdfComboEditable];
        9: Result := Result + [tdfRtlLayout];
        10: Result := Result + [tdfNoDefaultRadioButton];
        11: Result := Result + [tdfCanBeMinimized];
        12: Result := Result + [tdfUserInputeTextBox];
        13: Result := Result + [tdfExpandButton];
        14: Result := Result + [tdfEmailButton];
        15: Result := Result + [tdfRemoveMainIcon];
        16: Result := Result + [tdfDoNotUseDefaultTitle];
        17: Result := Result + [tdfRemoveCommonButtons];
      end;
    end;
  end;
begin
  PrepareMsgInfo(AMsgInfo, [AMsgCategory], ARightToLeft);

  if ARightToLeft then
  begin
    Task.MainText := AMsgInfo.LongMsgPersian;
    AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfRtlLayout];
    Task.DialogCaption := AMsgInfo.CaptionPersian;
    Task.Title := AMsgInfo.TitlePersian;
  end
  else
  begin
    Task.MainText := AMsgInfo.LongMsg;
    Task.DialogCaption := AMsgInfo.Caption;
    Task.Title := AMsgInfo.Title;
  end;

  case AMsgCategory of
    mcSimple: Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags, GetDialogIcon(AMsgInfo.DlgType));

    mcFooter:
    begin
      if ARightToLeft then
        Task.FooterText := AMsgInfo.FooterPersian
      else
        Task.FooterText := AMsgInfo.Footer;

      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags , GetDialogIcon(AMsgInfo.DlgType), AMsgInfo.FooterIcon);
    end;

    mcVerifyCheckBox:
    begin
      if ARightToLeft then
        Task.VerifyText := AMsgInfo.VerifyTextPersian
      else
        Task.VerifyText := AMsgInfo.VerifyText;

      Task.VerifyChecked := AMsgInfo.VerifyChecked;
      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags , GetDialogIcon(AMsgInfo.DlgType));
    end;

    mcComboBox:
    begin
      if ARightToLeft then
        Task.ComboBoxItems := AMsgInfo.ComboBoxItemsPersian
      else
        Task.ComboBoxItems := AMsgInfo.ComboBoxItems;

      if AMsgInfo.IsComboEditable then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfComboEditable];

      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags, GetDialogIcon(AMsgInfo.DlgType));
      ShowMessage('Selected Combo Item Index: ' + Task.ComboBoxSelectionResultIndex.ToString);
    end;

    mcInputText:
    begin
      AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfUserInputeTextBox];
      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags, GetDialogIcon(AMsgInfo.DlgType));
      ShowMessage('Entered text: ' + Task.UserInputedText);
    end;

    mcRadioButtons:
    begin
      if ARightToLeft then
        Task.RadioButtonsItems := AMsgInfo.RadioButtonsItemsPersian
      else
        Task.RadioButtonsItems := AMsgInfo.RadioButtonsItems;
      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags , GetDialogIcon(AMsgInfo.DlgType));
      ShowMessage('Selected Radio Item Index: ' + Task.RadioResult.ToString);
    end;

    mcExpandableText:
    begin
      if ARightToLeft then
      begin
        Task.ExpandableText := AMsgInfo.ExpandableTextPersian;
        Task.ExpandedButtonTooltip := AMsgInfo.ExpandedButtonTooltipPersian;
        Task.CollapsedButtonTooltip := AMsgInfo.CollapsedButtonTooltipPersian;
      end
      else
      begin
        Task.ExpandableText := AMsgInfo.ExpandableText;
        Task.ExpandedButtonTooltip := AMsgInfo.ExpandedButtonTooltip;
        Task.CollapsedButtonTooltip := AMsgInfo.CollapsedButtonTooltip;
      end;

      AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfExpandFooterArea, tdfExpandButton];

      if AMsgInfo.AutoExpandFooter then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfExpandByDefault];

      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags, GetDialogIcon(AMsgInfo.DlgType));
    end;

    mcVistaStyleButtons, mcVistaStyleButtonsEmail:
    begin
      Task.MainText := AMsgInfo.ShortMsg;

      if AMsgInfo.AddEmailButton then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfEmailButton];

      if AMsgInfo.RemoveMainIcon then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfRemoveMainIcon];

      if AMsgInfo.RemoveOsTitle then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfDoNotUseDefaultTitle];

      if AMsgInfo.RemoveCommonButtons then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfRemoveCommonButtons];

      if Length(AMsgInfo.CommandLinkButtons) > 0 then
      begin
        SetLength(Task.CommandLinkButtons, length(AMsgInfo.CommandLinkButtons));
        for I := Low(AMsgInfo.CommandLinkButtons) to High(AMsgInfo.CommandLinkButtons) do
        begin
          Task.CommandLinkButtons[I].TitleCaption := AMsgInfo.CommandLinkButtons[I].TitleCaption;
          Task.CommandLinkButtons[I].DetailCaption := AMsgInfo.CommandLinkButtons[I].DetailCaption;
          Task.CommandLinkButtons[I].Hint := AMsgInfo.CommandLinkButtons[I].Hint;
          Task.CommandLinkButtons[I].CanCloseForm := AMsgInfo.CommandLinkButtons[I].CanCloseForm;
          Task.CommandLinkButtons[I].ExtraBtnClick := AMsgInfo.CommandLinkButtons[I].ExtraBtnClick;
        end;
      end;
      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags , GetDialogIcon(AMsgInfo.DlgType));
    end;

    mcProgressBar, mcHyperLink:
    begin
      AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfShowProgressBar, tdfShowMarqueeProgressBar];

      if AMsgCategory = mcHyperLink then
        AMsgInfo.DialogFlags := AMsgInfo.DialogFlags + [tdfEnableHyperLinks];

      if not AMsgInfo.VerifyText.IsEmpty then
      begin
        Task.VerifyText := '';
        Task.VerifyChecked := True;
      end;

      Task.RadioButtonsItems := IfThen(ARightToLeft, AMsgInfo.RadioButtonsItemsPersian, AMsgInfo.RadioButtonsItems);
      Task.FooterText := IfThen(ARightToLeft, AMsgInfo.FooterPersian, AMsgInfo.Footer);
      Task.ComboBoxItems := IfThen(ARightToLeft, AMsgInfo.ComboBoxItemsPersian, AMsgInfo.ComboBoxItems);
      Task.ExpandableText := IfThen(ARightToLeft, AMsgInfo.ExpandableTextPersian, AMsgInfo.ExpandableText);
      Task.ExpandedButtonTooltip := IfThen(ARightToLeft, AMsgInfo.ExpandedButtonTooltipPersian, AMsgInfo.ExpandedButtonTooltip);
      Task.CollapsedButtonTooltip := IfThen(ARightToLeft, AMsgInfo.CollapsedButtonTooltipPersian,AMsgInfo.CollapsedButtonTooltip);

      Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, GetFlags , GetDialogIcon(AMsgInfo.DlgType), TAxTaskDialogFooterIcon(Ord(tfiInformation)), 200, 0);
    end;
  else
    Result := Task.Execute(GetCommonButtons(AMsgInfo.CommonButton), 0, AMsgInfo.DialogFlags, GetDialogIcon(AMsgInfo.DlgType)); //Simple
  end;
end;

procedure TForm1.ExtraButtonClick(Sender: TObject);
begin
  ShowMessage('Button has been clicked!');
end;

procedure TForm1.TempButtonClickSave(Sender: TObject);
begin
  ShowMessage('Saved successfully');
end;

{ TMsgInfo }

procedure TMsgInfo.Init;
begin
  MsgCategory := mcNone;
  Caption := '';
  CaptionPersian := '';
  Title := '';
  TitlePersian := '';
  Footer := '';
  FooterPersian := '';
  ShortMsg := '';
  ShortMsgPersian := '';
  LongMsg := '';
  LongMsgPersian := '';
  DialogFlags := [tdfPositionRelativeToWindow];
  DlgType := mtError;
  CommonButton := [mbOK];
  ErrorNumber := 0;
  FooterIcon := tfiInformation;
  ExpandableText := '';
  ExpandableTextPersian := '';
  ExpandedButtonTooltip := '';
  CollapsedButtonTooltip := '';
  AutoExpandFooter:= False;
  VerifyText := '';
  VerifyTextPersian := '';
  VerifyChecked := False;
  ComboBoxItems := '';
  ComboBoxItemsPersian := '';
  IsComboEditable := False;
  RadioButtonsItems := '';
  RadioButtonsItemsPersian := '';
  CommandLinkButtons := nil;

  AddEmailButton := False;
  RemoveMainIcon := False;
  RemoveOsTitle := False;
  RemoveCommonButtons := False;
end;

end.
