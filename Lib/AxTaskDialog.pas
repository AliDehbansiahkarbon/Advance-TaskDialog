unit AxTaskDialog;
{$R 'AxTaskDialogResource.RES'}
interface

uses
  Winapi.Windows,System.Classes, System.SysUtils,Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  RzPanel, RzRadChk, RzButton, RzEdit, RzCommon, Winapi.ShellAPI, RzLabel, System.StrUtils, System.TypInfo, System.Generics.Collections,
  Vcl.Themes, System.IOUtils, Vcl.ComCtrls;
var
  BitmapOK: TBitmap;
  BitmapArrow: TBitmap;
  DefaultFont: TFont;

type
  TCommandLinkButton = record
    TitleCaption: string;
    DetailCaption: string;
    Hint: string;
    CanCloseForm: Boolean;
    ExtraBtnClick: procedure (Sender: TObject) of object;
  end;

  TExposedPanel = class(TRzPanel)
    procedure PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PanelMouseEnter(Sender: TObject);
    procedure PanelMouseLeave(Sender: TObject);

  private
    procedure DrawFrameOnPanel(aPanel: TExposedPanel; aColor: TColor; aStyle: TBrushStyle);
    procedure ClearFrameOnPanel(aPanel: TExposedPanel);
    procedure SetBitmap(aBmp: TBitmap);
  public
    CanCloseForm: Boolean;
    ExtraButtonClick: procedure (Sender: TObject) of object;
    constructor Create(aOwner: TComponent; aTitleCaption: string; aDetailCaption: string; aHint: string);
    procedure LabelOnClick(Sender: TObject);
  end;

  TTaskDialogForm = class(TForm)
  public
    procedure ButtonClick(Sender: TObject);
  protected
    procedure FormShow(Sender: TObject);
  end;

  TDialogFormType = (ftRtpForm, ftBasic);
  TCommonButton = (cbOK, cbYes, cbNo, cbCancel, cbRetry, cbClose);
  TCommonButtons = set of TCommonButton;
  TAxTaskDialogIcon = (tiBlank, tiWarning, tiQuestion, tiError, tiInformation, tiNotUsed, tiShield);
  TAxTaskDialogFooterIcon = (tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation, tfiShield);
  TAxTaskDialogFlag = (
    tdfEnableHyperLinks, tdfUseCommandLinks, tdfUseCommandLinksNoIcon,
    tdfExpandFooterArea, tdfExpandByDefault, tdfShowProgressBar,
    tdfShowMarqueeProgressBar, tdfCallbackTimer, tdfPositionRelativeToWindow,
    tdfRtlLayout, tdfNoDefaultRadioButton, tdfCanBeMinimized, tdfUserInputeTextBox,
    tdfExpandButton, tdfEmailButton, tdfRemoveMainIcon, tdfDoNotUseDefaultTitle, tdfRemoveCommonButtons);
  TAxTaskDialogFlags = set of TAxTaskDialogFlag;

  TAxTaskDialog = object
    procedure BtnExpandClick(Sender: TObject);
  public
    DialogCaption: string;
    Title: string;
    MainText: string;
    RadioButtonsText: string;
    ExpandableText: string;
    ExpandedButtonTooltip: string;
    CollapsedButtonTooltip: string;
    FooterText: string;
    VerifyText: string;
    ComboBoxItems: string;
    UserInputedText: string;
    RadioResult: Integer;
    ComboBoxSelectionResultIndex: Integer;
    VerifyChecked: BOOL;
    BottomPanel: TRzPanel;
    CommandLinkButtons: array of TCommandLinkButton;
    class function UnAmp(const aTempStr: string): string;
    function CR(const aText: string): string;
    function NoCR(const aText: string; var aHint: string): string;
    function GetFooterIconName(aFooterIcon: TAxTaskDialogFooterIcon): string;
    function Execute(aCommonButtons: TCommonButtons = []; aDefaultButton: Integer = 0; aFlags: TAxTaskDialogFlags = []; aDialogIcon: TAxTaskDialogIcon = tiInformation; aFooterIcon: TAxTaskDialogFooterIcon = tfiWarning; aDefaultRadio: Integer = 0; aWidth: Integer = 0): Integer;
  private
      FDialogForm: TTaskDialogForm;
      FTopPanel, FMiddlePanel: TRzPanel;
      FFlags: TAxTaskDialogFlags;
      FTempHint: string;
  end;

  TExtraBitButton = class(TRzBitBtn)
  public
    CanCloseForm: Boolean;
  end;

  TUrlLabelHandler = class(TComponent)
  private
    FLbl: TRzLabel;
    FUrl: string;
    procedure UrlOnClick(Sender: TObject);
  public
    constructor Create(aLabel: TRzLabel; const aURL: string); reintroduce;
  end;

implementation

resourcestring
  SMsgDlgWarning = 'Warning';
  SMsgDlgError = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Confirm';
  SMsgDlgYes = '&Yes';
  SMsgDlgNo = '&No';
  SMsgDlgOK = 'OK';
  SMsgDlgCancel = 'Cancel';
  SMsgDlgHelp = '&Help';
  SMsgDlgHelpNone = 'No help available';
  SMsgDlgHelpHelp = 'Help';
  SMsgDlgAbort = '&Abort';
  SMsgDlgRetry = '&Retry';
  SMsgDlgIgnore = '&Ignore';
  SMsgDlgAll = '&All';
  SMsgDlgNoToAll = 'N&o to All';
  SMsgDlgYesToAll = 'Yes to &All';
  SMsgDlgClose = '&Close';
  SCloseButton = '&Close';

const

  TD_BTNS: array[TCommonButton] of pointer = (@SMsgDlgOK, @SMsgDlgYes, @SMsgDlgNo, @SMsgDlgCancel, @SMsgDlgRetry, @SCloseButton);
  TD_BTNMOD: array[TCommonButton] of Integer = (mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);

  TD_ICONS_IDENT: array[TAxTaskDialogIcon] of pointer =(nil, @SMsgDlgWarning, @SMsgDlgConfirm, @SMsgDlgError, @SMsgDlgInformation, nil, @SMsgDlgInformation);
  WIN_ICONS: array[TAxTaskDialogIcon] of PChar = (nil, IDI_WARNING, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, nil, IDI_WINLOGO);
  WIN_FOOTERICONS: array[TAxTaskDialogFooterIcon] of PChar = (nil, IDI_WARNING, IDI_QUESTION, IDI_ERROR, IDI_INFORMATION, IDI_WINLOGO);

{ TTaskDialog }
procedure ActiveWindowScreenShot(aCapture: TBitMap);
var
  LvCanvas: TCanvas;
  LvTempRect, LvWindowRect: TRect;
  LvWindowHandle: THandle;
begin
  LvCanvas := TCanvas.Create;
  LvCanvas.Handle := GetWindowDC(GetDesktopWindow);
  LvWindowHandle := GetForeGroundWindow;
  if LvWindowHandle <> 0 then
    GetWindowRect(LvWindowHandle, LvWindowRect);
  try
    LvTempRect := Rect(0, 0, LvWindowRect.Right - LvWindowRect.Left, LvWindowRect.Bottom - LvWindowRect.Top);
    aCapture.Width  := LvWindowRect.Right - LvWindowRect.Left;
    aCapture.Height := LvWindowRect.Bottom - LvWindowRect.Top;
    aCapture.Canvas.CopyRect(LvTempRect, LvCanvas, LvWindowRect);
  finally
    ReleaseDC(0, LvCanvas.Handle);
    LvCanvas.Free;
  end;
end;

function ScreenShot(aCapture: TBitMap): string;
var
  LvCanvas: TCanvas;
  LvRect: TRect;
begin
  LvCanvas := TCanvas.Create;
  LvCanvas.Handle := GetWindowDC(GetDesktopWindow);
  try
    LvRect := Rect(0, 0, Screen.Width, Screen.Height);
    aCapture.Width := Screen.Width;
    aCapture.Height := Screen.Height;
    aCapture.Canvas.CopyRect(LvRect, LvCanvas, LvRect);
    Result := IncludeTrailingBackslash(TPath.GetTempPath) + 'RMPro'+ FormatDateTime('mmddyyyyhhnnss', Now) + '.bmp';
    aCapture.SaveToFile(Result);
  finally
    ReleaseDC(0, LvCanvas.Handle);
    LvCanvas.Free;
  end;
end;

procedure TTaskDialogForm.ButtonClick(Sender: TObject);
var
  LvAttachmentArray: array of string;
  LvRecipients: array of string;
  LvScreenCapture, LvDialogCapture: TBitmap;
begin
  inherited;
  if (Sender <> nil) then
  begin
    if Sender.InheritsFrom(TExposedPanel) then
    begin
      TExposedPanel(Sender).ClearFrameOnPanel(TExposedPanel(Sender));
      if TExposedPanel(Sender).HelpKeyword = 'EmailButton' then
      begin
        try
          SetLength(LvRecipients, 1);
          SetLength(LvAttachmentArray, 1);
          LvRecipients[0] := 'support@rminno.com';
          LvScreenCapture := TBitmap.Create;
          LvDialogCapture := TBitmap.Create;
          ActiveWindowScreenShot(LvDialogCapture);
          LvAttachmentArray[0] := ScreenShot(LvScreenCapture);
          if not FileExists(LvAttachmentArray[0]) then
            LvAttachmentArray[0] := '';
          //OpenRepEmail(LvRecipients, [], [], '', LvAttachmentArray, '', False, True, True, GetSMTPParams());
          TExposedPanel(Sender).ClearFrameOnPanel(TExposedPanel(Sender));
        finally
          LvScreenCapture.Free;
          LvDialogCapture.Free;
        end;
      end else begin
        with TExposedPanel(Sender) do
        begin
          if Assigned(ExtraButtonClick) then
            ExtraButtonClick(TExposedPanel(Sender));
          Self.Tag := Tag; // Self means TTaskDialogForm here.
          if CanCloseForm then
            Close;
        end;
      end;
    end;

    if Sender.InheritsFrom(TRzLabel) then
    begin
      with TExposedPanel(TRzLabel(Sender).Parent) do
      begin
        ClearFrameOnPanel(TExposedPanel(TRzLabel(Sender).Parent));
        if Assigned(TExposedPanel(TRzLabel(Sender).Parent).ExtraButtonClick) then
          TExposedPanel(TRzLabel(Sender).Parent).ExtraButtonClick(TExposedPanel(TRzLabel(Sender).Parent));
        Self.Tag := Tag; // Self means TTaskDialogForm here.
        if CanCloseForm then
          Close;
      end;
    end;

    if Sender.InheritsFrom(TExtraBitButton) then
    begin
      with TExtraBitButton(Sender) do
      begin
        Self.Tag := Tag; // Self means TTaskDialogForm here.
        if Tag in [mrOk..mrNo] then
        begin
          Self.ModalResult := Tag;
          Close;
        end
        else if CanCloseForm then
          Close;
      end;
    end;
  end;
end;

procedure TAxTaskDialog.BtnExpandClick(Sender: TObject);
var
  LvPnlHeight: Integer;
  LvTempPanel: TRzPanel;
begin
  LvPnlHeight := 0;
  LvTempPanel := TRzPanel(FDialogForm.FindComponent('AlignedExpandPanel'));
  if TSpeedButton(Sender).Tag = 1 then
  begin
    BottomPanel.Visible := True;
    if (LvTempPanel <> nil) and not (LvTempPanel.Visible) then begin
      LvPnlHeight := LvTempPanel.Height;
      LvTempPanel.Visible := True;
      BottomPanel.Height := BottomPanel.Height + LvPnlHeight + 2;
      FDialogForm.Height := FDialogForm.Height + LvPnlHeight + 2;
      TSpeedButton(Sender).Hint := CollapsedButtonTooltip;
      TSpeedButton(Sender).Tag := 2;
      TSpeedButton(Sender).Glyph := nil;
      TSpeedButton(Sender).Glyph.LoadFromResourceName(HInstance, 'BTNCOLLAPS');
      if (FMiddlePanel.BorderSides = [sdTop]) and (FDialogForm.FindComponent('AlignedBottomPanel') = nil) then
        FMiddlePanel.BorderSides := [sdTop, sdBottom];
    end;
  end
  else if TSpeedButton(Sender).Tag = 2 then
  begin
    if (LvTempPanel <> nil) and (LvTempPanel.Visible) then
    begin
      LvPnlHeight := LvTempPanel.Height;
      LvTempPanel.Visible := False;
      BottomPanel.Height := BottomPanel.Height - LvPnlHeight - 2;
      FDialogForm.Height := FDialogForm.Height - LvPnlHeight - 2;
      TSpeedButton(Sender).Hint := ExpandedButtonTooltip;
      TSpeedButton(Sender).Tag := 1;
      TSpeedButton(Sender).Glyph.LoadFromResourceName(HInstance, 'BTNEXPAND');
      if (FMiddlePanel.BorderSides = [sdTop, sdBottom]) and (FDialogForm.FindComponent('AlignedBottomPanel') = nil) then
        FMiddlePanel.BorderSides := [sdTop];
    end;
  end;
end;

function TAxTaskDialog.CR(const aText: string): string;
begin
  if pos('\n', aText)=0 then
    Result := aText
  else
    Result := StringReplace(aText, '\n', #10, [rfReplaceAll]);
end;

function TAxTaskDialog.Execute(aCommonButtons: TCommonButtons; aDefaultButton: Integer; aFlags: TAxTaskDialogFlags; aDialogIcon: TAxTaskDialogIcon; aFooterIcon: TAxTaskDialogFooterIcon; aDefaultRadio, aWidth: Integer): Integer;
var
  I, XB, LvLastLeftPosition, LvLastTopPosition, LvFontHeight, LvSurfaceOptimizer, LvMiddlePanelHeight, LvTopPanelHeight, LvBottomPanelHeight: Integer;
  LvTempImage: TImage;
  LvEditBox: TRzEdit;
  LvComboBox: TComboBox;
  LvTempList: TStrings;
  LvCButton: TCommonButton;
  LvRadioButtons: array of TRzRadioButton;
  LvVerifierCheckBox: TRzCheckBox;
  LvExpandButton: TSpeedButton;
  LvProgressBar: TProgressBar;
function AddFooterExpandedText(const aText: string; aBigFont: Boolean; aParent: TWinControl): TMemo;
begin
  Result := TMemo.Create(aParent);
  Result.Parent := aParent;
  Result.WordWrap := True;
  if aBigFont then
  begin
    Result.Font.Height := LvFontHeight - 4;
    Result.Font.Color := $B00000;
  end
  else
    Result.Font.Height:= LvFontHeight;
  Result.Left := LvLastLeftPosition;
  Result.Top := 5;
  Result.Width := aWidth - LvLastLeftPosition - 25;
  Result.Lines.Text := CR(aText);
  Result.ScrollBars := ssVertical;
  Result.ReadOnly := True;
  Result.BringToFront;
  Inc(LvLastTopPosition, Result.Height + 16);
  BottomPanel.Height := BottomPanel.Height + Result.Height;// + 16;
  aParent.Height := Result.Height + 5;
  Result.AlignWithMargins := True;
  Result.Margins.Left := 0;
  Result.Top := 2;
  Result.Margins.Right := 0;
  Result.Margins.Bottom := 2;
end;
function AddFooter(const aText: string): TRzLabel;
var
  LvUrlText, LvLeftText, LvRightText, LvTempText: string;
  LvStartPos, LvEndPos: Integer;
  LvLeftLabel, LvUrlLabel, LvRightLabel: TRzLabel;
  LvUrlHandler: TUrlLabelHandler;
  LvMustAddHeight: Boolean;
  LvTop: Integer;
  LvAlignedBottomPanel, LvAlignedExpandPanel: TRzPanel;
  LvTempIcon: TIcon;
  LvTempBmp: TBitmap;
begin
  LvMustAddHeight := False;
  LvLeftText := '';
  LvUrlText := '';
  LvRightText := '';
  LvTop := 5;
  BottomPanel.Margins.Top := 5;
{$REGION ' Add AddFooterExpandedText'}
  if (ExpandableText.Trim <> '') and (tdfExpandFooterArea in aFlags) then
  begin
    LvAlignedExpandPanel := TRzPanel.Create(FDialogForm);
    LvAlignedExpandPanel.Parent := BottomPanel;
    LvAlignedExpandPanel.Transparent := True;
    LvAlignedExpandPanel.BorderInner := fsNone;
    LvAlignedExpandPanel.BorderOuter := fsNone;
    LvAlignedExpandPanel.ParentBackground := False;
    LvAlignedExpandPanel.Align := alTop;
    LvAlignedExpandPanel.Margins.Top := 5;
    LvAlignedExpandPanel.Name := 'AlignedExpandPanel';
    AddFooterExpandedText(ExpandableText, False, LvAlignedExpandPanel);
  end;
{$ENDREGION}
  if (aText.IsEmpty) then
    Exit;
{$REGION ' Add AlignedBottomPanel as the FooterText holder'}
  LvAlignedBottomPanel := TRzPanel.Create(BottomPanel);
  LvAlignedBottomPanel.Parent := BottomPanel;
  LvAlignedBottomPanel.Align := alBottom;
  LvAlignedBottomPanel.Transparent := True;
  LvAlignedBottomPanel.BorderInner := fsNone;
  LvAlignedBottomPanel.BorderOuter := fsNone;
  LvAlignedBottomPanel.ParentBackground := False;
  LvAlignedBottomPanel.Margins.Top := 5;
  LvAlignedBottomPanel.Name := 'AlignedBottomPanel';
  LvAlignedBottomPanel.Height := 35;
{$ENDREGION}
{$REGION ' Add footer icon'}
  if WIN_FOOTERICONS[aFooterIcon] <> nil then
  begin
    LvTempImage := TImage.Create(LvAlignedBottomPanel);
    LvTempImage.Parent := LvAlignedBottomPanel;
    LvTempImage.Picture.Bitmap.LoadFromResourceName(hInstance, PWideChar(GetFooterIconName(aFooterIcon)));
    LvTempImage.SetBounds(24, 5, LvTempImage.Picture.Bitmap.Width, LvTempImage.Picture.Bitmap.Height);
    LvTempImage.AutoSize:= False;
    LvTempImage.Stretch := True;
    LvTempImage.Transparent:= True;
  end
  else
    LvLastLeftPosition := 24;
{$ENDREGION}
{$REGION ' Add footer Text'}
  if not (tdfEnableHyperLinks in FFlags) then
  begin
    Result := TRzLabel.Create(LvAlignedBottomPanel);
    Result.Parent := LvAlignedBottomPanel;
    Result.WordWrap := True;
    Result.Font.Height:= LvFontHeight;
    Result.Left := LvLastLeftPosition;
    Result.Top := LvTop;
    Result.Width := aWidth - LvLastLeftPosition - 8;
    Result.Caption := CR(aText);
    Result.Transparent := True;
    LvMustAddHeight := True;
    Inc(LvLastTopPosition, Result.Height + 16);
    Result.Parent.Height := Result.Height + 1;
  end
  else
  begin
    if pos('<a', aText) = 0 then
    begin
      Result := TRzLabel.Create(LvAlignedBottomPanel);
      Result.Parent := LvAlignedBottomPanel;
      Result.WordWrap := True;
      Result.Font.Height:= LvFontHeight;
      Result.Left := LvLastLeftPosition;
      Result.Top := LvTop;
      Result.Width := aWidth - LvLastLeftPosition - 8;
      Result.Caption := CR(aText);
      Result.Transparent := True;
      LvMustAddHeight := True;
      Inc(LvLastTopPosition, Result.Height + 16);
      Result.Parent.Height := Result.Height + 1;
    end
    else
    begin
      LvTempText := LowerCase(aText);
      LvStartPos := pos('"', LvTempText) + 1;
      LvEndPos := pos('">', LvTempText);
      LvLeftText := CR(Copy(LvTempText, 0 , pos('<a', LvTempText) -1 ));
      LvUrlText := CR(copy(LvTempText, LvStartPos, LvEndPos - LvStartPos ));
      LvRightText := CR(Copy(LvTempText, LvEndPos + 2, Pos('</a>', LvTempText) - LvEndPos - 4));

      if LvLeftText.Trim <> '' then
      begin
        LvLeftLabel := TRzLabel.Create(LvAlignedBottomPanel);
        LvLeftLabel.Parent := LvAlignedBottomPanel;
        LvLeftLabel.WordWrap := False;
        LvLeftLabel.Font.Height:= LvFontHeight;
        LvLeftLabel.Left := LvLastLeftPosition;
        LvLeftLabel.Top := LvTop;
        //LvLeftLabel.Width := aWidth - X - 8;
        LvLeftLabel.AutoSize:= True;
        LvLeftLabel.Caption := CR(LvLeftText);
        LvLeftLabel.Transparent := True;
        Inc(LvLastLeftPosition, LvLeftLabel.Width);
        if not LvMustAddHeight then
        begin
          Inc(LvLastTopPosition, LvLeftLabel.Height + 16);
          LvMustAddHeight := True;
        end;

        if LvUrlText.Trim <> '' then
        begin
          LvUrlLabel := TRzLabel.Create(LvAlignedBottomPanel);
          LvUrlLabel.Parent := LvAlignedBottomPanel;
          LvUrlLabel.WordWrap := False;
          LvUrlLabel.Font.Height:= LvFontHeight;
          LvUrlLabel.Left := LvLastLeftPosition;
          LvUrlLabel.Top := LvTop;
          LvUrlLabel.AutoSize := True;
          LvUrlLabel.Caption := CR(LvUrlText);
          LvUrlLabel.Transparent := True;
          LvUrlHandler := TUrlLabelHandler.Create(LvUrlLabel, LvUrlText);
          Inc(LvLastLeftPosition, LvUrlLabel.Width);
          if not LvMustAddHeight then
          begin
            Inc(LvLastTopPosition, LvUrlLabel.Height + 16);
            LvMustAddHeight := True;
          end;
        end;

        if LvRightText.Trim <> '' then
        begin
          LvRightLabel := TRzLabel.Create(LvAlignedBottomPanel);
          LvRightLabel.Parent := LvAlignedBottomPanel;
          LvRightLabel.WordWrap := False;
          LvRightLabel.Font.Height:= LvFontHeight;
          LvRightLabel.Left := LvLastLeftPosition;
          LvRightLabel.Top := LvTop;
          LvRightLabel.AutoSize := True;
          LvRightLabel.Caption := CR(LvRightText);
          LvRightLabel.Transparent := True;
          Inc(LvLastLeftPosition, LvRightLabel.Width);
        end;
      end;
    end;
  end;
{$ENDREGION}
end;
function AddButton(S: string; Tag: Integer; AParent: TWinControl): TExtraBitButton;
var
  LvBtnWith: Integer;
  LvHint: string;
begin
  S := UnAmp(S);
  LvBtnWith := FDialogForm.Canvas.TextWidth(S) + 60;
  Dec(XB, LvBtnWith);
  if XB < (LvLastLeftPosition shr 1) then
  begin
    XB := aWidth - LvBtnWith;
    Inc(LvLastTopPosition, 32);
    LvMiddlePanelHeight := LvMiddlePanelHeight + 28;
  end;
  case Tag of
   1..11 : XB := XB - 15;
  end;

  Result := TExtraBitButton.Create(FDialogForm);
  Result.CanCloseForm := True;
  Result.Parent := AParent;
  Result.SetBounds(XB, LvLastTopPosition, LvBtnWith, 25);
  Result.Caption := NoCR(S, LvHint);
  if not LvHint.IsEmpty then
  begin
    Result.Hint := LvHint;
    Result.ShowHint := True;
  end;
  Result.Tag := Tag;
  Result.OnClick := FDialogForm.ButtonClick;
  case Tag of
    mrOk:
    begin
      Result.Default := true;
      if aCommonButtons = [cbOk] then
        Result.Cancel := True;
    end;
    mrCancel: Result.Cancel := True;
  end;
  if Tag = aDefaultButton then
    FDialogForm.Tag := Integer(Result);
end;
function AddLabel(const Text: string; BigFont: Boolean; ChangeColor:Boolean; AParent: TWinControl; AName: string): TRzLabel;
begin
  Result := TRzLabel.Create(FDialogForm);
  Result.Parent := AParent;
  Result.WordWrap := True;
  if BigFont then
    Result.Font.Height := LvFontHeight - 6
  else
    Result.Font.Height:= LvFontHeight - 2;

  if ChangeColor then
    Result.Font.Color := $B00000;
  Result.Left := LvLastLeftPosition;
  Result.Top := LvLastTopPosition;
  Result.Width := AWidth - LvLastLeftPosition - 8;
  Result.Caption := CR(Text);
  Result.Transparent := True;
  Inc(LvLastTopPosition, Result.Height + 16);
  Result.Name := AName;
end;
function AddCommandLinkbuttons:TExposedPanel;
var
  I: Integer;
  LvTitleCaption, LvDetailCaption : string;
  LvHint: string;
begin
  if (tdfUseCommandLinks in aFlags) and (Length(CommandLinkButtons) > 0) then
  begin
    for I:= Low(CommandLinkButtons) to High(CommandLinkButtons) do
    begin
      LvTitleCaption := CommandLinkButtons[I].TitleCaption;
      LvDetailCaption := CommandLinkButtons[I].DetailCaption;
      LvHint := CommandLinkButtons[I].Hint;
      with TExposedPanel.Create(FDialogForm, LvTitleCaption, LvDetailCaption, LvHint) do
      begin
        Parent := FTopPanel;
        BorderInner := fsNone;
        BorderOuter := fsNone;
        Transparent := True;
        if TStyleManager.ActiveStyle.Name = 'Windows' then
          Color := clWhite
        else
          Color := TStyleManager.ActiveStyle.GetSystemColor(FDialogForm.Color);
        if not LvHint.IsEmpty then
        begin
          Hint := LvHint;
          ShowHint := True;
        end;
        ParentBackground := False;
        SetBounds(LvLastLeftPosition, LvLastTopPosition, aWidth - 20 - LvLastLeftPosition, Height);
        Caption := '';
        CanCloseForm := CommandLinkButtons[I].CanCloseForm;
        Inc(LvLastTopPosition, Height + 5);
        Tag := I + 101;
        OnClick := FDialogForm.ButtonClick;
        if not (tdfUseCommandLinksNoIcon in aFlags) then
          SetBitmap(BitmapArrow);
        if Assigned(CommandLinkButtons[I].ExtraBtnClick) then
          ExtraButtonClick := CommandLinkButtons[I].ExtraBtnClick
        else
          ExtraButtonClick := nil;
      end;
    end;
    Inc(LvLastTopPosition, 5);
  end;

  if (tdfEmailButton in aFlags) then
  begin
    LvTitleCaption := 'Send Mail';
    LvDetailCaption := 'Send Mail to support department.';
    LvHint := '';
    with TExposedPanel.Create(FDialogForm, LvTitleCaption, LvDetailCaption, LvHint) do
    begin
      Parent := FTopPanel;
      BorderInner := fsNone;
      BorderOuter := fsNone;
      Transparent := True;
      if TStyleManager.ActiveStyle.Name = 'Windows' then
        Color := clWhite
      else
        Color := TStyleManager.ActiveStyle.GetSystemColor(FDialogForm.Color);
      ParentBackground := False;
      SetBounds(LvLastLeftPosition, LvLastTopPosition, aWidth - 20 - LvLastLeftPosition, Height);
      Caption := '';
      CanCloseForm := False;
      Inc(LvLastTopPosition, Height + 2);
      Tag := 100;
      OnClick := FDialogForm.ButtonClick;
      if not (tdfUseCommandLinksNoIcon in aFlags) then
        SetBitmap(BitmapArrow);
      HelpKeyword := 'EmailButton';
    end;
    Inc(LvLastTopPosition, 7);
  end;
end;
begin
{$REGION ' Initialize parameters'}
  if (Byte(aCommonButtons) = 0) and (Length(CommandLinkButtons) = 0) then
  begin
    aCommonButtons := [cbOk];
    if aDefaultButton = 0 then
      aDefaultButton := mrOk;
  end;
  if DialogCaption='' then
    if Application.MainForm = nil then
      DialogCaption := Application.Title
    else
      DialogCaption := Application.MainForm.Caption;

  if (Title = '') and (TD_ICONS_IDENT[aDialogIcon] <> nil) and not (tdfDoNotUseDefaultTitle in aFlags) then
    Title := LoadResString(TD_ICONS_IDENT[aDialogIcon]);

  MainText := StringReplace(MainText, 'Error: ', '', [rfIgnoreCase]);
  LvVerifierCheckBox := nil;
  LvComboBox := nil;
  LvEditBox := nil;
  FFlags := aFlags; // Just for external use.
  I:= 0;
  XB:= 0;
  LvLastLeftPosition:= 0;
  LvLastTopPosition:= 0;
  LvFontHeight := 0;
  LvSurfaceOptimizer := 0;
  LvMiddlePanelHeight := 0;
  LvTopPanelHeight := 0;
  LvBottomPanelHeight := 0;
{$ENDREGION}
{$REGION ' Create TaskDialog Form and initialize form properties'}
  FDialogForm := TTaskDialogForm(TForm.CreateParented(Application.DialogHandle));
  try
    if (tdfCanBeMinimized in aFlags) then
    begin
      FDialogForm.BorderStyle := bsSizeable;
      FDialogForm.BorderIcons := [biMinimize];
      FDialogForm.Constraints.MinHeight := 150;
    end
    else
    begin
      FDialogForm.BorderStyle := bsDialog;
      FDialogForm.BorderIcons := [];
      FDialogForm.Constraints.MinHeight := 150;
    end;
    if (tdfPositionRelativeToWindow in aFlags) then
      FDialogForm.Position := poOwnerFormCenter;
    FDialogForm.Font := DefaultFont;
    LvFontHeight := DefaultFont.Height;
    if aWidth = 0 then
    begin
      aWidth := FDialogForm.Canvas.TextWidth(Title);
      if (aWidth > 300) or (FDialogForm.Canvas.TextWidth(MainText) > 300) {or ( (Length(CommandLinkButtons)) > 0 and (length(CommandLinkButtons[0].TitleCaption) > 40))} then
        aWidth := 480
      else
        aWidth := 420;
    end;
    FDialogForm.ClientWidth := aWidth;
    FDialogForm.Height := 130;
    FDialogForm.Caption := DialogCaption;
    FDialogForm.OnShow := FDialogForm.FormShow;
{$ENDREGION}
{$REGION ' Create 3 panels '}
    FTopPanel := TRzPanel.Create(FDialogForm);
    FTopPanel.Parent := FDialogForm;
    FTopPanel.Align := alTop;
    FTopPanel.ParentBackground := False; // clWhite not used otherwise
    if TStyleManager.ActiveStyle.Name = 'Windows' then
      FTopPanel.Color := clWhite
    else
      FTopPanel.Color := TStyleManager.ActiveStyle.GetSystemColor(FDialogForm.Color);
    FTopPanel.BorderInner := fsNone;
    FTopPanel.BorderOuter := fsNone;
    FTopPanel.Name := 'TopPanel';
    LvTopPanelHeight := FTopPanel.Height;
    FTopPanel.Transparent := False;
    FTopPanel.StyleElements := [seFont];
    
    FMiddlePanel := TRzPanel.Create(FDialogForm);
    FMiddlePanel.Parent := FDialogForm;
    FMiddlePanel.Align := alClient;
    FMiddlePanel.BorderInner := fsNone;
    FMiddlePanel.BorderOuter := fsGroove;
    FMiddlePanel.BorderSides := [sdTop, sdBottom];
    FMiddlePanel.Name := 'MiddlePanel';
    LvMiddlePanelHeight := 34;
    
    if (FooterText.Trim <> '') or ((tdfExpandFooterArea in FFlags) and (ExpandableText.Trim <> '')) then begin
      BottomPanel := TRzPanel.Create(FDialogForm);
      BottomPanel.Parent := FDialogForm;
      BottomPanel.Align := alBottom;
      BottomPanel.BorderInner := fsNone;
      BottomPanel.BorderOuter := fsNone;
      BottomPanel.Transparent := True;
      BottomPanel.ParentBackground := True;
      BottomPanel.Visible := True;
      BottomPanel.Name := 'BottomPanel';
      BottomPanel.Height := 35;
      LvBottomPanelHeight := BottomPanel.Height;
    end;
{$REGION ' Handle main Dialog icon'}    //
    if (WIN_ICONS[aDialogIcon] <> nil) and not (tdfRemoveMainIcon in aFlags) then  begin
      LvTempImage := TImage.Create(FDialogForm);
      LvTempImage.Parent := FTopPanel;
      LvTempImage.Picture.Icon.Handle := LoadIcon(0, WIN_ICONS[aDialogIcon]);
      LvTempImage.SetBounds(16, 16, LvTempImage.Picture.Icon.Width, LvTempImage.Picture.Icon.Height);
      LvLastLeftPosition := LvTempImage.Width + 48;
      LvLastTopPosition := LvTempImage.Top;
    end else begin
      LvTempImage := nil;
      LvLastLeftPosition := 24;
      LvLastTopPosition := 24;
    end;
{$ENDREGION}
{$ENDREGION}
{$REGION ' Add Primary Text, MainText, ExpandableText'}
    if not Title.IsEmpty then
      AddLabel(Title, True, True, FTopPanel, 'TitleLabel');

    if not MainText.IsEmpty then
      AddLabel(MainText, False, False, FTopPanel, 'MiainTextLabel');

    if (ExpandableText <> '') and not (tdfExpandFooterArea in aFlags) then
      AddLabel(ExpandableText, False, False, FTopPanel, 'ExpandLabel');
{$ENDREGION}
{$REGION ' Add command links buttons'}
    AddCommandLinkbuttons;
{$ENDREGION}
{$REGION ' Add Radio buttons'}
    if RadioButtonsText <> '' then
    begin
      with TStringList.Create do
      try
        Delimiter := '|';
        StrictDelimiter := True;
        DelimitedText := Trim(RadioButtonsText);
        SetLength(LvRadioButtons,Count);
        for I := 0 to Count-1 do
        begin
          LvRadioButtons[I] := TRzRadioButton.Create(FDialogForm);
          with LvRadioButtons[I] do
          begin
            Parent := FTopPanel;
            SetBounds(LvLastLeftPosition + 16, LvLastTopPosition, aWidth - 32 - LvLastLeftPosition, 6 - LvFontHeight);
            Caption := NoCR(Strings[I], FTempHint);
            if FTempHint <> '' then
            begin
              ShowHint := True;
              Hint := FTempHint; // note shown as Hint
            end;
            inc(LvLastTopPosition, Height);
            Tag := I + 200;
            if (I = 0) or (Tag = aDefaultRadio) then
              Checked := True;
            Transparent := True;
          end;
        end;
        inc(LvLastTopPosition,24);
      finally
        Free;
      end;
    end;
{$ENDREGION}
{$REGION ' Add ComboBox Or EditBox'}
    if ComboBoxItems <> '' then begin
      LvTempList := TStringList.Create;
      try
        LvTempList.Delimiter := '|';
        LvTempList.StrictDelimiter := True;
        LvComboBox := TComboBox.Create(FDialogForm);
        LvComboBox.Parent := FTopPanel;
        LvComboBox.SetBounds(LvLastLeftPosition, LvLastTopPosition, aWidth - 32 - LvLastLeftPosition, 22);
        if tdfUserInputeTextBox in aFlags then
          LvComboBox.Style := csDropDown
        else
          LvComboBox.Style := csDropDownList;
        LvTempList.DelimitedText := Trim(ComboBoxItems);
        LvComboBox.Items.Assign(LvTempList);
        LvComboBox.ItemIndex := LvTempList.IndexOf(UserInputedText);
        inc(LvLastTopPosition,42);
      finally
        LvTempList.Free;
      end;
    end else if tdfUserInputeTextBox in aFlags then begin
      LvEditBox := TRzEdit.Create(FDialogForm);
      LvEditBox.Parent := FTopPanel;
      LvEditBox.SetBounds(LvLastLeftPosition, LvLastTopPosition, aWidth - 16 - LvLastLeftPosition, 22);
      LvEditBox.Text := UserInputedText;
      inc(LvLastTopPosition, 42);
    end;
    LvTopPanelHeight := LvLastTopPosition;
    LvLastTopPosition := 16; // Reset
{$ENDREGION}
    FTopPanel.Height := LvTopPanelHeight;
    FDialogForm.ClientHeight := LvTopPanelHeight + LvMiddlePanelHeight + LvBottomPanelHeight ;
{$REGION ' Add buttons and verification checkbox'}
    if (Byte(aCommonButtons) <> 0) or (VerifyText <> '') or  ((Length(CommandLinkButtons) > 0) and not (tdfUseCommandLinks in aFlags)) then
    begin
      XB := aWidth;
      if not (tdfUseCommandLinks in aFlags) and (Length(CommandLinkButtons) > 0) then // if CommandLinks Buttons doesn't created before this line we create theme as standard buttons.
      begin
        for I := High(CommandLinkButtons) downto Low(CommandLinkButtons) do
          AddButton(CommandLinkButtons[I].TitleCaption, I + 101, FMiddlePanel);
      end;

      for LvCButton := high(LvCButton) downto low(LvCButton) do
        if LvCButton in aCommonButtons then
          AddButton(LoadResString(TD_BTNS[LvCButton]), TD_BTNMOD[LvCButton], FMiddlePanel);

      if VerifyText <> '' then
      begin
        LvVerifierCheckBox := TRzCheckBox.Create(FMiddlePanel);
        with LvVerifierCheckBox do
        begin
          Parent := FMiddlePanel;
          if LvLastLeftPosition + 16 + FDialogForm.Canvas.TextWidth(VerifyText) > XB then
          begin
            inc(LvLastTopPosition,32);
            XB := aWidth;
          end;
          SetBounds(LvLastLeftPosition, LvLastTopPosition, XB - LvLastLeftPosition, 24);
          Caption := VerifyText;
          Checked := VerifyChecked;
        end;
      end;
    end else
      XB := 0;     
{$ENDREGION}
{$REGION ' Add ProgressBar'}
    if tdfShowProgressBar in aFlags then
    begin
      LvProgressBar := TProgressBar.Create(FMiddlePanel);
      LvProgressBar.Parent := FMiddlePanel;

      if not (tdfShowMarqueeProgressBar in aFlags) then
        LvProgressBar.Style := pbstNormal
      else
        LvProgressBar.Style := pbstMarquee;

      inc(LvLastTopPosition, 32);
      if LvLastLeftPosition + 16 + FDialogForm.Canvas.TextWidth(VerifyText) > XB then
      begin
        inc(LvLastTopPosition, 32);
        XB := aWidth;
      end;
      LvProgressBar.SetBounds(LvLastLeftPosition, LvLastTopPosition - 28, XB - LvLastLeftPosition - 10, 17);
    end;
{$ENDREGION}
{$REGION ' Add ExpandButton'}
    if (ExpandableText.Trim <> '') and (tdfExpandFooterArea in aFlags) then
    begin
      LvExpandButton := TSpeedButton.Create(FMiddlePanel);
      LvExpandButton.Parent := FMiddlePanel;
      LvExpandButton.SetBounds(FMiddlePanel.Left + 2, 2, 24, 24);
      LvExpandButton.OnClick := BtnExpandClick;
      LvExpandButton.Tag := 1; // Do not change this tag!, its used for bottom expandable panel.
      LvExpandButton.ShowHint := True;
      LvExpandButton.Flat := True;
      LvExpandButton.Hint := ExpandedButtonTooltip;
      LvExpandButton.Glyph.LoadFromResourceName(HInstance, 'BTNEXPAND');
    end;
{$ENDREGION}
{$REGION ' Add footer text with optional icon'}
    LvLastTopPosition := 16; //Reset for Footer.
    if (FooterText.Trim <> '') or ((tdfExpandFooterArea in FFlags) and (ExpandableText.Trim <> '')) then
      AddFooter(FooterText);
{$ENDREGION}
{$REGION ' Display the form'}
    if FDialogForm.FindComponent('AlignedExpandPanel') <> nil then
    begin
      TRzPanel(FDialogForm.FindComponent('AlignedExpandPanel')).Visible := False;
      if FDialogForm.FindComponent('AlignedBottomPanel') = nil then
      begin
        BottomPanel.Height := 0;
        if FDialogForm.FindComponent('MiddlePanel') <> nil then
          FMiddlePanel.BorderSides := [sdTop];
      end
      else
        BottomPanel.Height := Max(35, (BottomPanel.Height - TRzPanel(FDialogForm.FindComponent('AlignedExpandPanel')).Height));
    end;

    if tdfRemoveCommonButtons in aFlags then
    begin
      if FDialogForm.FindComponent('MiddlePanel') <> nil then
      begin
        FMiddlePanel.Visible := False;
        LvMiddlePanelHeight := 0;
        LvSurfaceOptimizer := 0;
      end;
    end
    else LvSurfaceOptimizer := 15;

    if FDialogForm.FindComponent('BottomPanel') <> nil then
    begin
      LvBottomPanelHeight := TRzPanel(FDialogForm.FindComponent('BottomPanel')).Height;
      FDialogForm.ClientHeight := LvTopPanelHeight + LvMiddlePanelHeight  + LvBottomPanelHeight + LvSurfaceOptimizer
    end
    else
    begin
      FMiddlePanel.BorderOuter := fsNone;
      FDialogForm.ClientHeight := LvTopPanelHeight + LvMiddlePanelHeight  + LvSurfaceOptimizer;
    end;
    if (tdfExpandByDefault in aFlags) then
      LvExpandButton.Click;
    FDialogForm.FormStyle := fsNormal;
    FDialogForm.ShowModal;
{$ENDREGION}
{$REGION 'Retrieve the results'}
    Result := FDialogForm.Tag;
    if Result >= $10000 then
      Result := 0;
    if LvComboBox <> nil then
    begin
      ComboBoxSelectionResultIndex := LvComboBox.ItemIndex;
      UserInputedText := LvComboBox.Text;
    end
    else if LvEditBox <> nil then
      UserInputedText := LvEditBox.Text;

    if LvVerifierCheckBox <> nil then
      VerifyChecked := LvVerifierCheckBox.Checked;

    RadioResult := 0;
    for i := 0 to high(LvRadioButtons) do
      if LvRadioButtons[i].Checked then
        RadioResult := i + 200;
{$ENDREGION}
  finally
    FDialogForm.Free;
  end;
end;

function TAxTaskDialog.GetFooterIconName(aFooterIcon: TAxTaskDialogFooterIcon): string;
begin
  case aFooterIcon of
    tfiWarning: Result := 'WARNING';
    tfiQuestion: Result := 'QUESTION';
    tfiError: Result := 'ERROR';
  else
    Result := 'INFORMATION';
  end;
end;

function TAxTaskDialog.NoCR(const aText: string;var aHint: string): string;
var
  I: Integer;
begin
  Result := aText;
  aHint := '';
  I := pos('\n', Result);
  if I > 0 then
  begin
    aHint := CR(copy(Result, I + 2, maxInt));
    SetLength(Result, I - 1);
  end;
end;

class function TAxTaskDialog.UnAmp(const aTempStr: string): string;
var
  I: Integer;
begin
  Result := aTempStr;
  repeat
    I := pos('&',Result);
    if I = 0 then
      Exit;
    Delete(Result, I, 1);
  until False;
end;

{ TUrlLabelHandler }
constructor TUrlLabelHandler.Create(aLabel: TRzLabel; const aURL: string);
begin
  inherited Create(aLabel);
  FLbl := aLabel;
  FUrl := aURL;
  FLbl.OnClick := UrlOnClick;
  FLbl.Font.Style := FLbl.Font.Style + [fsUnderline];
  FLbl.Font.Color := clBlue;
  FLbl.Cursor := crHandPoint;
  if (FLbl.hint = '') then
  begin
    FLbl.hint := FUrl;
    FLbl.ShowHint := True;
  end;
end;

procedure TUrlLabelHandler.UrlOnClick(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'Open', PChar(FUrl), nil, nil, SW_SHOWNORMAL);
end;

{ TExposedPanel }
procedure TExposedPanel.LabelOnClick(Sender: TObject);
begin
  inherited;
  if (Sender <> nil) then
  begin
    if Sender.InheritsFrom(TRzLabel) then
    begin
      with TExposedPanel(TRzLabel(Sender).Parent) do
      begin
        ClearFrameOnPanel(TExposedPanel(TRzLabel(Sender).Parent));
        TExposedPanel(TRzLabel(Sender).Parent).Click;
      end;
    end;
  end;
end;

procedure TExposedPanel.ClearFrameOnPanel(aPanel: TExposedPanel);
var
  LvColor: TColor;
  LvRect : TRect;
begin
  try
    LvColor := TStyleManager.ActiveStyle.GetSystemColor(Application.MainForm.Color);
  except
    LvColor := -16777201
  end;
  LvRect := Rect(0, 0, aPanel.Width ,aPanel.Height);
  with TExposedPanel(aPanel).Canvas do
  begin
    Brush.Color := LvColor;
    Brush.Style := bsSolid;
    FrameRect(LvRect);
  end;
end;

constructor TExposedPanel.Create(aOwner: TComponent; aTitleCaption: string; aDetailCaption: string; aHint: string);
var
  LvLabelTitle, LvLabelDetail: TRzLabel;
begin
  inherited Create(aOwner);
  Height := 50;
  OnMouseDown := PanelMouseDown;
  OnMouseUp := PanelMouseUp;
  OnMouseEnter := PanelMouseEnter;
  OnMouseLeave := PanelMouseLeave;

  LvLabelTitle := TRzLabel.Create(Self.Parent);
  with LvLabelTitle do
  begin
    Transparent := True;
    Parent := Self;
    Align := alTop;
    AlignWithMargins := True;
    Margins.Left := 24;
    Margins.Bottom := 0;
    Margins.Top := 0;
    Margins.Right := 0;
    AutoSize := True;
    Caption := aTitleCaption;
    Font.Name := 'Tahoma';
    Font.Size := 11;
    Font.Color := clNavy;
    Font.Style := [fsBold];
    Layout := tlTop;
    TabStop := False;
    WordWrap:= True;
    OnMouseDown := Self.PanelMouseDown;
    OnMouseUp := Self.PanelMouseUp;
    OnMouseEnter := Self.PanelMouseEnter;
    OnMouseLeave := Self.PanelMouseLeave;
    OnClick := LabelOnClick;
    Self.Height := max(Self.Height,Height);
    if not aHint.IsEmpty then
    begin
      Hint := aHint;
      ShowHint := True;
    end;
  end;

  if not aDetailCaption.IsEmpty then
  begin
    LvLabelDetail := TRzLabel.Create(Self.Parent);
    with LvLabelDetail do
    begin
      Transparent := True;
      Parent := Self;
      Align := alClient;
      AlignWithMargins := True;
      Margins.Left := 24;
      Margins.Top := 0;
      Margins.Bottom := 0;
      Margins.Right := 0;
      AutoSize := True;
      Caption := aDetailCaption;
      Font.Name := 'Tahoma';
      Font.Size := 10;
      Font.Color := clNavy;
      Layout := tlTop;
      TabStop := False;
      WordWrap:= True;
      OnMouseDown := Self.PanelMouseDown;
      OnMouseUp := Self.PanelMouseUp;
      OnMouseEnter := Self.PanelMouseEnter;
      OnMouseLeave := Self.PanelMouseLeave;
      OnClick := LabelOnClick;

      if Self.Height < Height then
        Self.Height := Self.Height + (Height - Self.Height);

      if not aHint.IsEmpty then
      begin
        Hint := aHint;
        ShowHint := True;
      end;
    end;
  end
  else LvLabelTitle.Align := alClient;
end;

procedure TExposedPanel.DrawFrameOnPanel(aPanel: TExposedPanel; aColor: TColor; aStyle: TBrushStyle);
var
  LvRect : TRect;
begin
  if TStyleManager.ActiveStyle.Name = 'Windows' then
    aPanel.Color := clWhite
  else
    aPanel.Color := TStyleManager.ActiveStyle.GetSystemColor(Screen.ActiveForm.Color);
  LvRect := Rect(0, 0, aPanel.Width , aPanel.Height);
  with TExposedPanel(aPanel).Canvas do
  begin
    Pen.Width := 2;
    Brush.Color := aColor;
    Brush.Style := aStyle;
    FrameRect(LvRect);
  end;
end;

procedure TExposedPanel.PanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TExposedPanel) then
  begin
    Self.Repaint;
    Self.Transparent := True;
  end
  else if (Sender is TRzLabel) then
  begin
    TExposedPanel(TRzLabel(Sender).Parent).Repaint;
    TRzLabel(Sender).Repaint;
  end;
end;

procedure TExposedPanel.PanelMouseEnter(Sender: TObject);
var
  LvTempPanel: TExposedPanel;
begin
  LvTempPanel := nil;
  if (Sender is TRzLabel) then
    LvTempPanel := TExposedPanel(TRzLabel(Sender).Parent)
  else if (Sender is TExposedPanel) then
    LvTempPanel := TExposedPanel(Sender);
  if LvTempPanel <> nil then
    DrawFrameOnPanel(LvTempPanel , $00FFB720, bsSolid);
end;

procedure TExposedPanel.PanelMouseLeave(Sender: TObject);
var
  LvTempPanel: TExposedPanel;
begin
  LvTempPanel := nil;
  if (Sender is TRzLabel) then
    LvTempPanel := TExposedPanel(TRzLabel(Sender).Parent)
  else if (Sender is TExposedPanel) then
    LvTempPanel := TExposedPanel(Sender);
  if LvTempPanel <> nil then
    ClearFrameOnPanel(LvTempPanel);
end;

procedure TExposedPanel.PanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TExposedPanel) then
  begin
    Self.BevelOuter := bvNone;
    Self.Repaint;
  end
  else if (Sender is TRzLabel) then
  begin
    TExposedPanel(TRzLabel(Sender).Parent).BevelOuter := bvNone;
    TExposedPanel(TRzLabel(Sender).Parent).Repaint;
    TExposedPanel(TRzLabel(Sender).Parent).Refresh;
    TRzLabel(Sender).Repaint;
  end;
end;

procedure TExposedPanel.SetBitmap(aBmp: TBitmap);
var
  LvTempImage : TImage;
begin
  if aBmp <> nil then
  begin
    LvTempImage := TImage.Create(Self);
    LvTempImage.Parent := Self;
    LvTempImage.Picture.Bitmap.LoadFromResourceName(hInstance, PWideChar('BTNARROW'));
    LvTempImage.SetBounds(5, 5, LvTempImage.Picture.Bitmap.Width, LvTempImage.Picture.Bitmap.Height);
    LvTempImage.AutoSize:= False;
    LvTempImage.Stretch := True;
    LvTempImage.Transparent:= True;
  end;
end;

procedure TTaskDialogForm.FormShow(Sender: TObject);
begin
  if TRzPanel(FindComponent('TopPanel')) <> nil then
  begin
    TRzPanel(FindComponent('TopPanel')).StyleElements := [];
    TRzPanel(FindComponent('TopPanel')).Color := clWhite;
    TRzPanel(FindComponent('TopPanel')).ParentBackground := False;
  end;

  if TRzPanel(FindComponent('TopPanel')) <> nil then
  begin
    TRzPanel(FindComponent('TopPanel')).StyleElements := [];
    TRzPanel(FindComponent('TopPanel')).Color := clWhite;
    TRzPanel(FindComponent('TopPanel')).ParentBackground := False;
  end;
  Self.Font.Name := 'Calibri';
end;

initialization
  DefaultFont := TFont.Create;
  DefaultFont.Style := [];
  if Screen.Fonts.IndexOf('Calibri') >= 0 then
  begin
    DefaultFont.Height := -14;
    DefaultFont.Name := 'Calibri';
  end
  else
  begin
    if Screen.Fonts.IndexOf('Tahoma') >= 0 then
      DefaultFont.Name := 'Tahoma'
    else
      DefaultFont.Name := 'Arial';
    DefaultFont.Height := -13;
  end;
  BitmapOK := TBitmap.Create;
  BitmapOK.LoadFromResourceName(HInstance, 'btnOk');
  BitmapOK.Transparent := true;
  BitmapArrow := TBitmap.Create;
  BitmapArrow.LoadFromResourceName(HInstance, 'btnArrow');
  BitmapArrow.Transparent := true;

finalization
  DefaultFont.Free;
  BitmapArrow.Free;
  BitmapOK.Free;
end.
