object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  Caption = 'About AppPathEditor'
  ClientHeight = 203
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 162
    Width = 387
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 112
    ExplicitTop = 208
    ExplicitWidth = 185
    object Button1: TButton
      Left = 144
      Top = 6
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 0
    Width = 387
    Height = 162
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Application Path Editor'
      'Copyright '#169' 2016-2020 Chua Chee Wee')
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    Zoom = 100
  end
end
