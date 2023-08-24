unit FMXAppPathEditor.Editor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Controls.Presentation,
  System.ImageList, FMX.ImgList, FMX.ScrollBox, FMX.Memo, AppPathEditor.Types;

type
  TFrameAppPathEditor = class(TFrame)
    pnlAppPath: TPanel;
    edPath: TEdit;
    btnAdd: TButton;
    btnRemove: TButton;
    pnlSpeedButtons: TPanel;
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    ImageList1: TImageList;
    gbPaths: TGroupBox;
    Panel1: TPanel;
    lbPaths: TListBox;
    cbShrinkPath: TCheckBox;
    OpenDialog1: TOpenDialog;
    procedure lbPathsClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure lbPathsDblClick(Sender: TObject);
    procedure cbShrinkPathChange(Sender: TObject);
  private
    function GetShrinkPath: Boolean;
    { Private declarations }
  public
    { Public declarations }
    AddPath: TProcAddPath;
    OnShrinkPath: TFuncFixPath;
    OnExpandPath: TFuncFixPath;

    procedure Initialize;

    property ShrinkPath: Boolean read GetShrinkPath;
  end;

implementation
uses
  Winapi.Windows, System.Messaging, AppPathEditor.Messages;

{$R *.fmx}

/// <summary> Returns the proper case sensitive filename for APathName</summary>
function GetCaseFilename(const APathName: string): string;
var
  LShortPathName: string;
begin
  SetLength(LShortPathName, MAXSHORT);
  var LLen := GetShortPathName(PChar(APathName), PChar(LShortPathName), Length(LShortPathName));
  if LLen = 0 then
    Exit(APathName);
  SetLength(Result, MAXSHORT);
  LLen := GetLongPathName(PChar(LShortPathName), PChar(Result), Length(Result));
  if LLen = 0 then
    Exit(APathName);
  SetLength(Result, LLen);
end;

procedure TFrameAppPathEditor.btnAddClick(Sender: TObject);
var
  LPathName, LSelectDirectory: string;
begin
  if edPath.Text <> '' then
    begin
      LPathName := edPath.Text;
      if cbShrinkPath.IsChecked and Assigned(OnShrinkPath) then
        OnShrinkPath(LPathName);
      if Assigned(AddPath) then
        AddPath(LPathName) else
        lbPaths.Items.Add(LPathName);
      edPath.Text := '';
    end else
    begin
      LPathName := ExtractFileDir(ParamStr(0));
      if SelectDirectory('Select directory', LPathName, LSelectDirectory) then
        begin
          if cbShrinkPath.IsChecked and Assigned(OnShrinkPath) then
            OnShrinkPath(LSelectDirectory);
          lbPaths.Items.Add(LSelectDirectory);
        end;
    end;
end;

procedure TFrameAppPathEditor.btnRemoveClick(Sender: TObject);
var
  LItemIndex: Integer;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex <> -1 then
    begin
      lbPaths.Items.Delete(LItemIndex);
      if LItemIndex < lbPaths.Items.Count then
        lbPaths.ItemIndex := LItemIndex else
        lbPaths.ItemIndex := lbPaths.Items.Count-1;
    end;
end;

procedure TFrameAppPathEditor.cbShrinkPathChange(Sender: TObject);
var
  LChanged: Boolean;
begin
  if cbShrinkPath.IsChecked then
    begin
      if not Assigned(OnShrinkPath) then
        Exit;
    end else
  if not Assigned(OnExpandPath) then
    Exit;
  for var I := 0 to lbPaths.Items.Count-1 do
    begin
      var LOldPath: string := lbPaths.Items[I];
      if cbShrinkPath.IsChecked then
        begin
          LChanged := OnShrinkPath(LOldPath);
        end else
        begin
          LChanged := OnExpandPath(LOldPath);
        end;
      if LChanged then
        begin
          LOldPath := GetCaseFilename(LOldPath);
          lbPaths.Items[I] := LOldPath;
        end;
    end;
end;

function TFrameAppPathEditor.GetShrinkPath: Boolean;
begin
  Result := cbShrinkPath.IsChecked;
end;

procedure TFrameAppPathEditor.Initialize;
begin
  // OnIdle check if the Remove button can be disabled.
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageIdle,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      btnRemove.Enabled := (lbPaths.Items.Count > 0) and
        (lbPaths.ItemIndex <> -1);
    end
  );
end;

procedure TFrameAppPathEditor.lbPathsClick(Sender: TObject);
var
  LItemIndex: Integer;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex <> -1 then
    edPath.Text := lbPaths.Items[LItemIndex];
end;

procedure TFrameAppPathEditor.lbPathsDblClick(Sender: TObject);
var
  LItemIndex: Integer;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex <> -1 then
    edPath.Text := lbPaths.Items[LItemIndex];
end;

procedure TFrameAppPathEditor.sbDownClick(Sender: TObject);
var
  LItemIndex: Integer;
  LItem1, LItem2: TListBoxItem;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex < lbPaths.Count-1 then
    begin
      LItem1 := lbPaths.ListItems[LItemIndex];
      LItem2 := lbPaths.ListItems[LItemIndex+1];
      lbPaths.ItemsExchange(LItem1, LItem2);
      lbPaths.ItemIndex := LItemIndex+1;
    end;
end;

procedure TFrameAppPathEditor.sbUpClick(Sender: TObject);
var
  LItemIndex: Integer;
  LItem1, LItem2: TListBoxItem;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex > 0 then
    begin
      LItem1 := lbPaths.ListItems[LItemIndex];
      LItem2 := lbPaths.ListItems[LItemIndex-1];
      lbPaths.ItemsExchange(LItem1, LItem2);
      lbPaths.ItemIndex := LItemIndex-1;
    end;
end;

end.
