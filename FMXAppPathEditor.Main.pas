unit FMXAppPathEditor.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.ImageList, FMX.ImgList, System.Win.Registry,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.Menus, System.Actions, FMX.ActnList,
  System.Generics.Collections, FMX.ScrollBox, FMX.Memo;

type
  TfrmAppPathEditor = class(TForm)
    ImageList1: TImageList;
    OpenDialog: TOpenDialog;
    MainMenu: TMainMenu;
    miAppPath: TMenuItem;
    miSave: TMenuItem;
    miExit: TMenuItem;
    miCancel: TMenuItem;
    pnlAppScope: TPanel;
    gbScope: TGroupBox;
    rbHKLM: TRadioButton;
    rbHKCU: TRadioButton;
    btnSelect: TButton;
    gpApplicationBrowser: TGroupBox;
    btnBrowse: TButton;
    pnlContainer: TPanel;
    lblAppPathName: TLabel;
    ActionList1: TActionList;
    acAppPathCancel: TAction;
    acAppPathSave: TAction;
    acExit: TAction;
    acSelectRegistry: TAction;
    acBrowseApplications: TAction;
    acAppPathSaveAs: TAction;
    miSaveAs: TMenuItem;
    Splitter1: TSplitter;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAppPathClick(Sender: TObject);
    procedure acAppPathCancelExecute(Sender: TObject);
    procedure acAppPathSaveExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acSelectRegistryExecute(Sender: TObject);
    procedure acBrowseApplicationsExecute(Sender: TObject);
    procedure acAppPathSaveAsExecute(Sender: TObject);
  private
    { Private declarations }
    Registry: TRegistry;
    FAppName, FAppPath, FBaseKey: string;
    FFrameRegistrySelector,
    FFrameAppPathEditor: TFrame;
    FEnvironmentMappings: TDictionary<string, string>;
    FLongestKeyLast: TArray<string>;
    procedure AddToListBox(const List: string);
    procedure BuildEnvironmentVarsDictionary;
    procedure CreateAppPath;
    procedure CreateFrame;
    procedure LoadRegistryKey(Sender: TObject);
    procedure ReopenRegistryMainKey;
    procedure OpenRegistryKey;
    procedure RegWriteString(const AName, AValue: string);
    procedure ShowAppPathEditor;
    procedure ShowAppPathSelector;
    procedure DeleteAppPathKey(const AKeyName: string);
    procedure EnableSave;
    procedure Log(const AMsg: string);
    procedure RegisterMessageListener;
    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean);
  private const
    CreateKeyIfMissing = True;
    DontCreateKey = False;

/// <summary>Given a full pathname without environment variables,
/// shrink it to one containing environment variables.</summary>
/// <returns>If the pathname doesn't contain environment variables, return false.</returns>
    function ShrinkPath(var VPathName: string): Boolean;

/// <summary>Given a pathname with environment variables,
/// expand it to the full pathname without the environment variables.</summary>
/// <returns>If the pathname doesn't contain environment variables, return false.</returns>
    function ExpandPath(var VPathName: string): Boolean;
  public
    { Public declarations }
  end;

var
  frmAppPathEditor: TfrmAppPathEditor;

implementation
uses
  Winapi.ShlwAPI, Winapi.Windows, System.IOUtils, FMXAppPathEditor.Editor,
  FMXAppPathEditor.SelectAppPath, System.StrUtils,
  System.Generics.Defaults, FMX.DialogService, System.Messaging,
  AppPathEditor.Messages, AppPathEditor.Types;

{$R *.fmx}

procedure TfrmAppPathEditor.acAppPathCancelExecute(Sender: TObject);
begin
  acSelectRegistry.Execute;
end;

procedure TfrmAppPathEditor.acAppPathSaveExecute(Sender: TObject);
begin
  CreateAppPath;
end;

function ExpandedEnvironmentString(const APathName: string): string;
var
  LLen: Cardinal;
begin
  SetLength(Result, MAXSHORT);
  LLen := ExpandEnvironmentStrings(PChar(APathName), PChar(Result), Length(Result));
  SetLength(Result, LLen-1);
end;

function PathCompact(const APath: string; var ANewPath: string): Boolean;
begin
//  Result := False;
//  ASize := Length(APath)*SizeOf(Char);
//  GetMem(LBuffer, ASize);
//  if PathUnExpandEnvStrings(PChar(APath), LBuffer, Length(APath)) then
//    begin
//      Result := True;
//      SetString(ANewPath, LBuffer, StrLen(LBuffer));
//    end;
//  FreeMem(LBuffer);
  ANewPath := APath;
  Result := frmAppPathEditor.ShrinkPath(ANewPath);
  if not Result then ANewPath := APath;
end;

procedure TfrmAppPathEditor.acBrowseApplicationsExecute(Sender: TObject);
var
  LKeyFound: Boolean;
  LFullPathName, LKeyName, LAppPath, LAppName: string;
  LKeyNames: TStringList;
begin
  if OpenDialog.Execute then
    begin
      LKeyFound := False;
      LFullPathName := OpenDialog.FileName;
      ShowAppPathEditor;
      OpenRegistryKey;
      lblAppPathName.Text := LFullPathName;
//      miSave.Enabled := True;
      acAppPathSave.Enabled := True;
      LKeyNames := TStringList.Create;
      try
        Registry.GetKeyNames(LKeyNames);
        for LKeyName in LKeyNames do
          begin
            Registry.OpenKeyReadOnly( FBaseKey + '\' + LKeyName);
            LAppPath := Registry.ReadString('');
            if (LFullPathName = LAppPath) or (ExpandedEnvironmentString(LAppPath) = LFullPathName) then
              begin
                LKeyFound := True;
                FAppName := LKeyName; // Use existing key name
                Break;
              end;
          end;
      finally
        LKeyNames.Free;
      end;
      LAppName := TPath.GetFileName(LFullPathName);
      ReopenRegistryMainKey;
      Registry.Access := KEY_ALL_ACCESS;
      if LKeyFound or Registry.OpenKey(LAppName, DontCreateKey) then
        begin
          FAppPath := Registry.ReadString('Path');
          if FAppPath = '' then
            FAppPath := TPath.GetDirectoryName(LFullPathName);
          // FAppName initialized above
        end else
      if Registry.OpenKey(LAppName, CreateKeyIfMissing) then
        begin
          Registry.WriteString('', LFullPathName);
          FAppPath := TPath.GetDirectoryName(LFullPathName);
          FAppName := TPath.GetFileName(LFullPathName);
        end;
      AddToListBox(FAppPath);
      acAppPathSave.Enabled := True;
      acAppPathSaveAs.Enabled := True;
      acAppPathCancel.Enabled := True;
    end;
end;

procedure TfrmAppPathEditor.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmAppPathEditor.DeleteAppPathKey(const AKeyName: string);
begin
  Registry.DeleteKey(AKeyName);
  Log(Format('%s deleted from %s', [AKeyName, FBaseKey]));
end;

procedure TfrmAppPathEditor.acSelectRegistryExecute(Sender: TObject);
var
  LKeyNames: TStringList;
  LFrameRegistrySelector: TFrameAppPathSelector;
  LKeyName: string;
begin
  ShowAppPathSelector;
  OpenRegistryKey;
  LKeyNames := TStringList.Create;
  try
    TFrame(LFrameRegistrySelector) := FFrameRegistrySelector;
    LFrameRegistrySelector.lbAppPaths.Clear;
//    LFrameRegistrySelector.CallOnComplete := LoadRegistryKey;
//    LFrameRegistrySelector.OnDelete := procedure (const AKeyName: string)
//      begin
//        Registry.DeleteKey(AKeyName);
//      end;
    Registry.GetKeyNames(LKeyNames);
    for LKeyName in LKeyNames do
      begin
        LFrameRegistrySelector.lbAppPaths.Items.Add(LKeyName);
      end;
  finally
    LKeyNames.Free;
  end;
end;

procedure TfrmAppPathEditor.acAppPathSaveAsExecute(Sender: TObject);
var
  LNewName: string;
begin
  LNewName := FAppName;
  if InputQuery('New Name', 'New Name?', LNewName) then
    begin
      if not EndsText('.exe', LNewName) then
        LNewName := LNewName + '.exe';
      FAppName := LNewName;
      acAppPathSave.Execute;
    end;
end;

procedure TfrmAppPathEditor.AddToListbox(const List: string);
var
  LFrameAppPathEditor: TFrameAppPathEditor;
  ListBox: TListBox;
  Items, Items2: TStringList;
  LItem: string;
begin
  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;
  LFrameAppPathEditor.OnShrinkPath := function (var VPathName: string): Boolean
    begin
      Result := ShrinkPath(VPathName);
      var LPath := lblAppPathName.Text;
      if ShrinkPath(LPath) then
        lblAppPathName.Text := LPath;
    end;
  LFrameAppPathEditor.OnExpandPath := function (var VPathName: string): Boolean
    begin
      Result := ExpandPath(VPathName);
      var LPath := lblAppPathName.Text;
      if ExpandPath(LPath) then
        lblAppPathName.Text := LPath;
    end;
  ListBox := LFrameAppPathEditor.lbPaths;
  Items := TStringList.Create;
  Items2 := TStringList.Create;
  try
    Items.Delimiter        := ';';
    Items.StrictDelimiter  := True;
    Items.AddStrings(ListBox.Items);
    Items2.Delimiter       := ';';
    Items2.StrictDelimiter := True;
    Items2.DelimitedText   := List;
    ListBox.Clear;
    ListBox.Items.AddStrings(Items);
    for LItem in Items2 do
      if Items.IndexOf(LItem)=-1 then
        ListBox.Items.Add(LItem);
  finally
    Items2.Free;
    Items.Free;
  end;
end;

// System environment is located here: HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment
// To programmatically add or modify system environment variables, add them to
// the HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment
// registry key, then broadcast a WM_SETTINGCHANGE message with lParam set to
// the string "Environment". This allows applications, such as the shell, to pick up your updates.

// Alternatively, use PathUnExpandEnvStrings

function TfrmAppPathEditor.ShrinkPath(var VPathName: string): Boolean;
var
  I: Integer; LMatched: Boolean;
  LEnvValue, LMatchValue, LMatchName, LEnvName: string;
const
  IgnoreCase = True;
begin
  LMatched := False; LMatchValue := '';
  for I := High(FLongestKeyLast) downto Low(FLongestKeyLast) do
    begin
      LEnvName := FLongestKeyLast[I];
      LEnvValue := FEnvironmentMappings[LEnvName];
      if VPathName.StartsWith(LEnvValue, IgnoreCase) and (Length(LEnvValue) > Length(LMatchValue)) then
        begin
          LMatched := True;
          LMatchValue := LEnvValue;
          LMatchName := LEnvName;
        end else
        begin
          // The last match is the shortest...
          if LMatched then
            begin
              LEnvName := '%' + LMatchName + '%';
              VPathName := StringReplace(VPathName, LMatchValue, LEnvName, [rfIgnoreCase]);
              Exit(True);
            end;
        end;
    end;
  Result := False;
end;

procedure TfrmAppPathEditor.EnableSave;
begin
  acAppPathSave.Enabled :=   FAppPath <> '';
  acAppPathSaveAs.Enabled := FAppPath <> '';
end;

function TfrmAppPathEditor.ExpandPath(var VPathName: string): Boolean;
var
  LPathName: string;
begin
  LPathName := ExpandedEnvironmentString(VPathName);
  Result := VPathName <> LPathName;
  if Result then
    VPathName := LPathName;
end;

//type
//  TLengthComparer = class(TInterfacedObject, IComparer<string>)
//  private
//    FDictionary: TDictionary<string, string>;
//  public
//    constructor Create(const ADictionary: TDictionary<string, string>);
//    function Compare(const Left, Right: string): Integer;
//  end;
//
//constructor TLengthComparer.Create(const ADictionary: TDictionary<string, string>);
//begin
//  inherited Create;
//  FDictionary := ADictionary;
//end;
//
//function TLengthComparer.Compare(const Left: string; const Right: string): Integer;
//var
//  LValLeft, LValRight: string;
//begin
//  LValLeft := FDictionary[Left];
//  LValRight := FDictionary[Right];
//  Result := Length(Left) + Length(LValLeft) - Length(Right) - Length(LValRight);
//end;

procedure TfrmAppPathEditor.BuildEnvironmentVarsDictionary;
var
  LEnvStr, LEnvKey, LEnvValue: string; PEnvEntry, PEnvBlock: PChar;
  LEnvMap: TArray<string>;
  LLongestKey: TList<string>;
begin
  if not Assigned(FEnvironmentMappings) then
    FEnvironmentMappings := TDictionary<string, string>.Create;
  LLongestKey := TList<string>.Create;

  // https://blogs.msdn.microsoft.com/oldnewthing/20100506-00/?p=14133
  // Skip the compatibility entries at the front, "fake per-drive current directories"
  PEnvBlock := GetEnvironmentStrings;
  PEnvEntry := PEnvBlock;

  try
    repeat
      // If there's a current directory set, PEnvBlock would look something like:
      // '=C:=C:\Users\Administrator'#0'=D:=D:\Downloads'#0'ALLUSERSPROFILE=C:\ProgramData'
      // If there's *NO* current directory set, PEnvBlock would look something like:
      // 'ALLUSERSPROFILE=C:\ProgramData'#0'APPDATA=C:\Users\Administrator\AppData\Roaming'
      LEnvStr := PEnvEntry;
      // Skip to the end
      PEnvEntry := StrEnd(PEnvEntry)+1;
      // and if it's an equal sign, continue
      if PEnvEntry^<>'=' then
        begin
          LEnvMap := SplitString(LEnvStr, '=');
          if Length(LEnvMap)>=2 then
            begin
              LEnvKey := LEnvMap[0];
              LEnvValue := LEnvMap[1];

              // Skip PATH and PATHEXT
              if SameText(LEnvKey, 'PATH') or SameText(LEnvKey, 'PATHEXT') or
              // if the value contains a semicolon (which is a path separator)
              // we don't need it
                 ContainsText(LEnvValue, PathSep) or
              // Skip keys defined by BCB/DELPHI or starts with BDS
                 SameText('DELPHI', LEnvKey) or
                 SameText('BCB', LEnvKey) or
                 StartsText('BDS', LEnvKey)  then
                Continue;
              // Ensure the value is a directory
              if not TDirectory.Exists(LEnvValue) then
                Continue;
        // same paths can be pointed to by multiple variables of different names
              FEnvironmentMappings.Add(LEnvKey, LEnvValue);
              LLongestKey.Add(LEnvKey);
            end;
        end;
    until PEnvEntry^ = #0;
    LLongestKey.Sort(TLengthComparer.Create(FEnvironmentMappings));
    FLongestKeyLast := LLongestKey.ToArray;
  finally
    FreeEnvironmentStrings(PEnvBlock);
    LLongestKey.Free;
  end;
end;

procedure TfrmAppPathEditor.CreateAppPath;
var
  LPath, LFullPathName, LAppName, LAppPath: string;
  LFrameAppPathEditor: TFrameAppPathEditor;
  ListBox: TListBox;
begin
  if (FFrameAppPathEditor = nil) then
    Exit;

  LFullPathName := lblAppPathName.Text;
  if FAppName = '' then
    FAppName := ExtractFileName(LFullPathName);
  LAppName := FAppName;
  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;

  ListBox := LFrameAppPathEditor.lbPaths;
  OpenRegistryKey;
  Registry.Access := KEY_ALL_ACCESS;
  LAppPath := '';
  for LPath in ListBox.Items do
    begin
      LAppPath := LAppPath + LPath + PathSep;
    end;
  Delete(LAppPath, Length(LAppPath), 1);
  try
    if Registry.OpenKey(LAppName, CreateKeyIfMissing) then
      begin
        if not Registry.ValueExists('') then
          begin
            RegWriteString('', LFullPathName);
            Log(Format('Created default for %s', [LFullPathName]));
          end;
        RegWriteString('Path', LAppPath);
        Log(Format('Saved %s to %s', [LAppPath, LAppName]));
      end;
  finally
    Registry.CloseKey;
  end;
end;

procedure TfrmAppPathEditor.CreateFrame;
var
  LFrameAppPathEditor: TFrameAppPathEditor;
begin
  if not Assigned(FFrameAppPathEditor) then
    begin
      LFrameAppPathEditor := TFrameAppPathEditor.Create(Self);
      LFrameAppPathEditor.AddPath := procedure (const APath: string) begin
        AddToListBox(APath);
      end;
      LFrameAppPathEditor.Initialize;
    end else
    begin
      LFrameAppPathEditor := TFrameAppPathEditor(FFrameAppPathEditor);
      LFrameAppPathEditor.edPath.Text := '';
    end;
  FFrameAppPathEditor := LFrameAppPathEditor;
end;

procedure TfrmAppPathEditor.FormCreate(Sender: TObject);
begin
  Registry := TRegistry.Create;
  BuildEnvironmentVarsDictionary;

  CreateFrame;

  Application.OnIdle := OnApplicationIdle;

  acAppPathCancel.Enabled := False;
  acAppPathSave.Enabled := False;
  acAppPathSaveAs.Enabled := False;

  RegisterMessageListener;
end;

procedure TfrmAppPathEditor.FormDestroy(Sender: TObject);
begin
  FEnvironmentMappings.Free;
  Registry.Free;
  FFrameRegistrySelector.Free;
  FFrameAppPathEditor.Free;
end;

procedure TfrmAppPathEditor.LoadRegistryKey(Sender: TObject);
var
  LKeyName, LFullPath, LAppPath: string;
  LFrameRegistrySelector: TFrameAppPathSelector;
  LFrameAppPathEditor: TFrameAppPathEditor;
begin
  TFrame(LFrameRegistrySelector) := FFrameRegistrySelector;
  LKeyName := LFrameRegistrySelector.AppName;
  FAppName := LKeyName;
  Registry.OpenKeyReadOnly( FBaseKey + '\' + LKeyName);
  LFullPath := Registry.ReadString('');
  LAppPath := Registry.ReadString('Path');
  FAppPath := LAppPath;
  lblAppPathName.Text := LFullPath;

  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;
  if Assigned(LFrameAppPathEditor) then
    LFrameAppPathEditor.lbPaths.Clear;

  TThread.ForceQueue(nil, procedure
    begin
      ShowAppPathEditor;
      AddToListBox(LAppPath);
    end);

  EnableSave;
end;

procedure TfrmAppPathEditor.Log(const AMsg: string);
var
  LTimedMsg: string;
begin
  LTimedMsg := Format('%s - %s', [FormatDateTime('hh:nn:ss', Now), AMsg]);
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Add(LTimedMsg);
    Memo1.GoToTextEnd;
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TfrmAppPathEditor.miAppPathClick(Sender: TObject);
begin
  EnableSave;
end;

procedure TfrmAppPathEditor.OnApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  TMessageManager.DefaultManager.SendMessage(nil, TMessageIdle.Create);
end;

procedure TfrmAppPathEditor.OpenRegistryKey;
const
  CKeySelected: array[HKEY_CLASSES_ROOT..HKEY_DYN_DATA] of string = (
    'HKEY_CLASSES_ROOT',
    'HKEY_CURRENT_USER',
    'HKEY_LOCAL_MACHINE',
    'HKEY_USERS',
    'HKEY_PERFORMANCE_DATA',
    'HKEY_CURRENT_CONFIG',
    'HKEY_DYN_DATA'
  );
var
  LRootKey: HKEY;
  LSelectedKey: string;
begin
  Registry.CloseKey;
  Registry.RootKey := HKEY_CURRENT_USER;
  if rbHKLM.IsChecked then
    begin
      Registry.RootKey := HKEY_LOCAL_MACHINE;
    end;

  LRootKey := Registry.RootKey;
  LSelectedKey := CKeySelected[LRootKey];
  Log(Format('%s selected.', [LSelectedKey]));
  ReopenRegistryMainKey;
end;

procedure TfrmAppPathEditor.RegisterMessageListener;
var
  LMM: TMessageManager;
begin
  LMM := TMessageManager.DefaultManager;

  LMM.SubscribeToMessage(TMessageLog,
    procedure(const Sender: TObject; const M: TMessage)
    var
      AMessageLog: TMessageLog absolute M;
    begin
      Log(AMessageLog.Msg);
    end
  );

  LMM.SubscribeToMessage(TMessageLoadKeyComplete,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      LoadRegistryKey(Sender);
    end
  );

  LMM.SubscribeToMessage(TDeleteRegKeyMsg,
    procedure(const Sender: TObject; const M: TMessage)
    var
      LMsg: TDeleteRegKeyMsg absolute M;
    begin
      DeleteAppPathKey(LMsg.Key);
    end
  );

end;

procedure TfrmAppPathEditor.RegWriteString(const AName, AValue: string);
begin
  var LOSSupported: Boolean := TOSVersion.Check(6, 1);
  if LOSSupported and (Pos('%', AValue)>0) then
    Registry.WriteExpandString(AName, AValue) else
    Registry.WriteString(AName, AValue);
end;

procedure TfrmAppPathEditor.ReopenRegistryMainKey;
begin
  if FBaseKey = '' then
    FBaseKey := '\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths';
  Registry.OpenKey(FBaseKey, CreateKeyIfMissing);
end;

procedure TfrmAppPathEditor.ShowAppPathEditor;
//var
//  LFrameAppPathEditor: TFrameAppPathEditor;
begin
  CreateFrame;

  if Assigned(FFrameRegistrySelector) then
    FFrameRegistrySelector.Parent := nil;

  FFrameAppPathEditor.Parent := pnlContainer;
end;

procedure TfrmAppPathEditor.ShowAppPathSelector;
var
  LFrameRegistrySelector: TFrameAppPathSelector;
begin
  if Assigned(FFrameAppPathEditor) then
    FFrameAppPathEditor.Parent := nil;
  if not Assigned(FFrameRegistrySelector) then
    begin
      LFrameRegistrySelector := TFrameAppPathSelector.Create(Self);
      FFrameRegistrySelector := LFrameRegistrySelector;
    end;

  FFrameRegistrySelector.Parent := pnlContainer;
end;

end.
