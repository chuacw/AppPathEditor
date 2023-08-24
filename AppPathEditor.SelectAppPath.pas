unit AppPathEditor.SelectAppPath;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TFrameAppPathSelector = class(TFrame)
    pnlAppPathsSelector: TPanel;
    gbApplication: TGroupBox;
    PopupMenu1: TPopupMenu;
    miDeleteAppPath: TMenuItem;
    lbAppPaths: TListBox;
    pnlBottom: TPanel;
    btnLoadKey: TButton;
    btnCancel: TButton;
    btnDeleteInvalid: TButton;
    procedure miDeleteAppPathClick(Sender: TObject);
    procedure lbAppPathsClick(Sender: TObject);
    procedure btnLoadKeyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDeleteInvalidClick(Sender: TObject);
  private
    { Private declarations }
    FItemIndex: Integer;
  public
    { Public declarations }
  type
//    TOnDeleteEvent = procedure (const ADeleteKey: string) of object;
    TProcOnDelete = reference to procedure (const KeyNames: TArray<string>; var
      VDeletedKeys: TArray<string>);
  public
    { Public declarations }
    AppName: string;
//    CallOnComplete: TNotifyEvent;
//    procedure FixScene(const AScene: IScene);
    OnDelete: TProcOnDelete;
    procedure ShowDeleteInvalidButton;
    procedure HideDeleteInvalidButton;
  end;

implementation
uses
  System.Messaging, AppPathEditor.Messages;

{$R *.dfm}

procedure TFrameAppPathSelector.btnCancelClick(Sender: TObject);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TMessageGoInitialState.Create);
end;

procedure TFrameAppPathSelector.btnDeleteInvalidClick(Sender: TObject);
begin
  if Assigned(OnDelete) then
    begin
      var LKeyNames: TArray<string> := nil;
      for var I := 0 to lbAppPaths.Items.Count-1 do
        if lbAppPaths.Selected[I] then
          begin
            SetLength(LKeyNames, Length(LKeyNames)+1);
            LKeyNames[High(LKeyNames)] := lbAppPaths.Items[I];
          end;
      var VDeletedKeys: TArray<string> := nil;
      OnDelete(LKeyNames, VDeletedKeys);
      if Length(VDeletedKeys) > 0 then
        begin
          for var LDeletedKey in VDeletedKeys do
            begin
              var LIndex := lbAppPaths.Items.IndexOf(LDeletedKey);
              if LIndex >= 0 then
                begin
                  lbAppPaths.Selected[LIndex] := False; // clear indices first before deleting
                end;
            end;

          for var LDeletedKey in VDeletedKeys do
            begin
              var LIndex := lbAppPaths.Items.IndexOf(LDeletedKey);
              if LIndex >= 0 then
                begin
                  lbAppPaths.Items.Delete(LIndex);
                end;
            end;
        end;
    end;
end;

procedure TFrameAppPathSelector.btnLoadKeyClick(Sender: TObject);
var
  LMM: TMessageManager;
begin
  LMM := TMessageManager.DefaultManager;
  if lbAppPaths.ItemIndex <> -1 then
    begin
      AppName := lbAppPaths.Items[lbAppPaths.ItemIndex];
      LMM.SendMessage(nil, TMessageLog.Create(Format('%s selected.', [AppName])));
      LMM.SendMessage(nil, TMessageEnableDelete.Create);
    end;
  LMM.SendMessage(nil, TMessageLoadKeyComplete.Create);
end;

procedure TFrameAppPathSelector.HideDeleteInvalidButton;
begin
  btnDeleteInvalid.Visible := False;
end;

procedure TFrameAppPathSelector.lbAppPathsClick(Sender: TObject);
begin
  FItemIndex := lbAppPaths.ItemIndex;
end;

procedure TFrameAppPathSelector.miDeleteAppPathClick(Sender: TObject);
begin
//  if Assigned(OnDelete) then
    begin
      var LItemIndex := FItemIndex;
      var LItem := lbAppPaths.Items[LItemIndex];
//      OnDelete(LItem);
      TMessageManager.DefaultManager.SendMessage(Self,
        TDeleteRegKeyMsg.Create(LItem));

      lbAppPaths.Items.Delete(LItemIndex);
    end;
end;

procedure TFrameAppPathSelector.ShowDeleteInvalidButton;
begin
  btnDeleteInvalid.Visible := True;
end;

end.
