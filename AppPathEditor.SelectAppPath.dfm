object FrameAppPathSelector: TFrameAppPathSelector
  Left = 0
  Top = 0
  Width = 522
  Height = 456
  Align = alClient
  TabOrder = 0
  object pnlAppPathsSelector: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 415
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object gbApplication: TGroupBox
      Left = 0
      Top = 0
      Width = 522
      Height = 415
      Align = alClient
      Caption = 'App Paths keys'
      TabOrder = 0
      object lbAppPaths: TListBox
        Left = 2
        Top = 15
        Width = 518
        Height = 398
        Align = alClient
        ItemHeight = 13
        MultiSelect = True
        PopupMenu = PopupMenu1
        TabOrder = 0
        OnClick = lbAppPathsClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 415
    Width = 522
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      522
      41)
    object btnLoadKey: TButton
      Left = 10
      Top = 8
      Width = 81
      Height = 25
      Caption = 'Load App Path'
      TabOrder = 0
      OnClick = btnLoadKeyClick
    end
    object btnCancel: TButton
      Left = 431
      Top = 8
      Width = 86
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnDeleteInvalid: TButton
      Left = 192
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Delete Invalid'
      TabOrder = 2
      Visible = False
      OnClick = btnDeleteInvalidClick
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 464
    Top = 112
    object miDeleteAppPath: TMenuItem
      Caption = 'Delete'
      OnClick = miDeleteAppPathClick
    end
  end
end
