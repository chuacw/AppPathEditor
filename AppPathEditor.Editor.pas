unit AppPathEditor.Editor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, AppPathEditor.Types;

type
  TFrameAppPathEditor = class(TFrame)
    Panel1: TPanel;
    edPath: TEdit;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    gpPaths: TGroupBox;
    pnlAppPath: TPanel;
    Splitter1: TSplitter;
    pnlSpeedButtons: TPanel;
    lbPaths: TListBox;
    btnUp: TButton;
    btnDown: TButton;
    cbShrinkPath: TCheckBox;
    btnAdd: TButton;
    btnRemove: TButton;
    StaticText1: TStaticText;
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lbPathsDblClick(Sender: TObject);
  private
    { Private declarations }
    function GetShrinkPath: Boolean;
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
  Vcl.FileCtrl, System.Messaging, AppPathEditor.Messages;

{$R *.dfm}

procedure TFrameAppPathEditor.btnAddClick(Sender: TObject);
var
  LPathName: string;
  LSelectDirectory: TArray<string>;
  LShrinkPathMsg: TShrinkPathMsg;
begin
// If the edit is not empty, then add it
  if edPath.Text <> '' then
    begin
      LPathName := edPath.Text;
      if cbShrinkPath.Checked then
        begin
//          LShrinkPathMsg := TShrinkPathMsg.Create(LPathName);
          if Assigned(OnShrinkPath) then
            OnShrinkPath(LPathName);
        end;
      if Assigned(AddPath) then
        AddPath(LPathName) else
        lbPaths.Items.Add(LPathName);
      edPath.Text := '';
    end else
    begin
      // edit is empty, add is clicked, select a directory...
      LPathName := ExtractFileDir(ParamStr(0));
//function SelectDirectory(const StartDirectory: string; out Directories: TArray<string>; Options: TSelectDirFileDlgOpts = [];
//  const Title: string = ''; const FolderNameLabel: string = ''; const OkButtonLabel: string = ''): Boolean; overload;

      if SelectDirectory(LPathName, LSelectDirectory, [], 'Select directory') then
        begin
          if cbShrinkPath.Checked and Assigned(OnShrinkPath) then
            OnShrinkPath(LSelectDirectory[0]);
          lbPaths.Items.Add(LSelectDirectory[0]);
        end;
    end;
end;

procedure TFrameAppPathEditor.btnDownClick(Sender: TObject);
var
  LItemIndex, LItem1, LItem2: Integer;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex < lbPaths.Count-1 then
    begin
      LItem1 := LItemIndex;
      LItem2 := LItemIndex+1;
      lbPaths.Items.Exchange(LItem1, LItem2);
      lbPaths.ItemIndex := LItemIndex+1;
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

procedure TFrameAppPathEditor.btnUpClick(Sender: TObject);
var
  LItemIndex, LItem1, LItem2: Integer;
begin
  LItemIndex := lbPaths.ItemIndex;
  if LItemIndex > 0 then
    begin
      LItem1 := LItemIndex;
      LItem2 := LItemIndex-1;
      lbPaths.Items.Exchange(LItem1, LItem2);
      lbPaths.ItemIndex := LItemIndex-1;
    end;
end;

function TFrameAppPathEditor.GetShrinkPath: Boolean;
begin
  Result := cbShrinkPath.Checked;
end;

procedure TFrameAppPathEditor.Initialize;
begin
  // OnIdle check if the Remove button can be disabled.
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageIdle,
    procedure(const Sender: TObject; const M: TMessage)
    var
      LEnabled: Boolean;
    begin
      btnRemove.Enabled := (lbPaths.Items.Count > 0) and
        (lbPaths.ItemIndex <> -1);

      LEnabled := (lbPaths.Items.Count > 1);
      btnUp.Enabled := LEnabled;
      btnDown.Enabled := LEnabled;
    end
  );
end;

procedure TFrameAppPathEditor.lbPathsDblClick(Sender: TObject);
begin
  if lbPaths.ItemIndex <> -1 then
    begin
      var LPath := lbPaths.Items[lbPaths.ItemIndex];
      lbPaths.Items.Delete(lbPaths.ItemIndex);
      lbPaths.ItemIndex := -1;
      edPath.Text := LPath;
    end;
end;

end.
