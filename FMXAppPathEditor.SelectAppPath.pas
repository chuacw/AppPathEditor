unit FMXAppPathEditor.SelectAppPath;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.Menus;

type
  TFrameAppPathSelector = class(TFrame)
    pnlAppPathsSelector: TPanel;
    gbApplication: TGroupBox;
    lbAppPaths: TListBox;
    pnlBottom: TPanel;
    btnLoadKey: TButton;
    btnCancel: TButton;
    PopupMenu1: TPopupMenu;
    miDeleteAppPath: TMenuItem;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadKeyClick(Sender: TObject);
    procedure lbAppPathsItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure miDeleteAppPathClick(Sender: TObject);
    procedure lbAppPathsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
    FTempItem: TListBoxItem;
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  public
//  type
//    TOnDeleteEvent = procedure (const ADeleteKey: string) of object;
//    TProcOnDelete = reference to procedure (const AKeyName: string);
    { Public declarations }
    AppName: string;
//    CallOnComplete: TNotifyEvent;
//    procedure FixScene(const AScene: IScene);
//    var OnDelete: TProcOnDelete;
  end;

implementation
uses
  System.Messaging, AppPathEditor.Messages;

{$R *.fmx}

procedure TFrameAppPathSelector.btnCancelClick(Sender: TObject);
begin
//  Parent := nil;

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

procedure TFrameAppPathSelector.lbAppPathsItemClick(
  const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  if not Assigned(Item.OnMouseUp) then
    begin
      Item.OnMouseUp := OnMouseUp;
      FTempItem := Item;
    end;
end;

procedure TFrameAppPathSelector.lbAppPathsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
    begin
      var LControl: IControl := FTempItem;
      var P1 := LControl.LocalToScreen(PointF(X, Y));
      PopupMenu1.Popup(P1.X, P1.Y);
    end;
end;

procedure TFrameAppPathSelector.miDeleteAppPathClick(Sender: TObject);
begin
  if not Assigned(FTempItem) then
    Exit;
//  if Assigned(OnDelete) then
    begin
      var LItem := FTempItem.Text;
      // OnDelete(LItem);

      TMessageManager.DefaultManager.SendMessage(Self,
        TDeleteRegKeyMsg.Create(LItem));

      var LItemIndex := FTempItem.Index;
      lbAppPaths.Items.Delete(LItemIndex);
      FTempItem := nil;
    end;
end;

procedure TFrameAppPathSelector.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
    begin
      var LControl: IControl := FTempItem;
      var P1 := LControl.LocalToScreen(PointF(X, Y));
      PopupMenu1.Popup(P1.X, P1.Y);
    end;
end;

end.
