object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 79
  ClientWidth = 206
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object Button1: TButton
    Left = 12
    Top = 10
    Width = 177
    Height = 41
    Caption = 'Show Message'
    TabOrder = 0
    OnClick = Button1Click
  end
  object chk_RightToLeft: TCheckBox
    Left = 21
    Top = 56
    Width = 160
    Height = 17
    Caption = 'Show RightToLeft messages'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
end
