unit AppPathEditor.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus,
  System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Win.Registry, System.Generics.Collections;


type
  TfrmAppPathEditor = class(TForm)
    ActionList1: TActionList;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    OpenDialog: TOpenDialog;
    acAppPathCancel: TAction;
    acAppPathSave: TAction;
    acExit: TAction;
    acSelectRegistry: TAction;
    acBrowseApplications: TAction;
    acAppPathSaveAs: TAction;
    N11: TMenuItem;
    Panel1: TPanel;
    pnlContainer: TPanel;
    gbApplication: TGroupBox;
    Button1: TButton;
    gbScope: TGroupBox;
    rbHKLM: TRadioButton;
    rbHKCU: TRadioButton;
    btnSelectRegistry: TButton;
    lblAppPathName: TLabel;
    memInstructions: TMemo;
    miShowInstructions: TMenuItem;
    acAppPathDelete: TAction;
    N2: TMenuItem;
    N3: TMenuItem;
    miAppPathDelete: TMenuItem;
    GroupBoxLog: TGroupBox;
    Memo1: TMemo;
    Help1: TMenuItem;
    miShowLog: TMenuItem;
    N1: TMenuItem;
    miAbout: TMenuItem;
    procedure acAppPathSaveAsExecute(Sender: TObject);
    procedure acBrowseApplicationsExecute(Sender: TObject);
    procedure acSelectRegistryExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acAppPathSaveExecute(Sender: TObject);
    procedure acAppPathCancelExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miShowInstructionsClick(Sender: TObject);
    procedure acAppPathDeleteExecute(Sender: TObject);
    procedure miShowLogClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
  private
    { Private declarations }
    Registry: TRegistry;
    FRegKeyAccess: LongWord;
    FAppName, FAppPath, FBaseKey: string;
    FFrameRegistrySelector,
    FFrameAppPathEditor: TFrame;
    FEnvironmentMappings: TDictionary<string, string>;
    FLongestKeyLast: TArray<string>;


    procedure AddToListBox(const List: string);
    procedure ClearListBox;
    procedure AddPathToListBox;
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
    procedure CheckEnableSave;
    procedure CheckEnableDelete;
    procedure EnableDeleteIfAppPathExist;
    procedure Log(const AMsg: string);
    procedure RegisterMessageListener;
    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean);
  private
  type
    TRegistryKeyMode = (rkmNoChecks, rkmCheckInvalidPath);
  var
    FRegistryKeyMode: TRegistryKeyMode;
  const
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
  System.StrUtils, System.IOUtils, AppPathEditor.SelectAppPath,
  AppPathEditor.Editor, AppPathEditor.Types, System.Messaging,
  AppPathEditor.Messages, AppPathEditor.AboutDlg;

{$R *.dfm}

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
  ANewPath := APath;
  Result := frmAppPathEditor.ShrinkPath(ANewPath);
  if not Result then ANewPath := APath;
end;

procedure TfrmAppPathEditor.acAppPathCancelExecute(Sender: TObject);
begin
  acSelectRegistry.Execute;
end;

procedure TfrmAppPathEditor.acAppPathDeleteExecute(Sender: TObject);
begin
 //  DeleteAppPathKey
  Registry.CloseKey;
  Registry.OpenKey(FBaseKey, DontCreateKey);
  // Registry.DeleteKey(FAppName);
  DeleteAppPathKey(FAppName);
  FAppName := '';
  CheckEnableDelete;
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

procedure TfrmAppPathEditor.acAppPathSaveExecute(Sender: TObject);
begin
  CreateAppPath;
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
      lblAppPathName.Caption := LFullPathName;
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
      ClearListBox;
      AddToListBox(FAppPath);
      // AddPathToListBox;
      acAppPathSave.Enabled := True;
      acAppPathSaveAs.Enabled := True;
      Log(Format('"%s" and "%s" are now enabled.', [acAppPathSave.Caption, acAppPathSaveAs.Caption]));
      EnableDeleteIfAppPathExist;
    end;
end;

procedure TfrmAppPathEditor.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmAppPathEditor.acSelectRegistryExecute(Sender: TObject);
var
  LKeyNames: TStringList;
  LFrameRegistrySelector: TFrameAppPathSelector;
  LKeyName, LSavedCurrentPath: string;
begin
  ShowAppPathSelector;
  OpenRegistryKey;
  LKeyNames := TStringList.Create;
  try
    TFrame(LFrameRegistrySelector) := FFrameRegistrySelector;
    LFrameRegistrySelector.lbAppPaths.Clear;
    LFrameRegistrySelector.HideDeleteInvalidButton;
//    LFrameRegistrySelector.CallOnComplete := LoadRegistryKey;
    Registry.GetKeyNames(LKeyNames);
    if FRegistryKeyMode = rkmCheckInvalidPath then
      begin
        LSavedCurrentPath := Registry.CurrentPath;
        LFrameRegistrySelector.OnDelete := procedure (
          const KeyNames: TArray<string>; var VDeletedKeys: TArray<string>)
          begin
            Registry.OpenKey('\' + LSavedCurrentPath, False);
            for var LKeyName in KeyNames do
              begin
                Registry.DeleteKey(LKeyName);
                Log(Format('Key: %s deleted.', [LKeyName]));
                SetLength(VDeletedKeys, Length(VDeletedKeys)+1);
                VDeletedKeys[High(VDeletedKeys)] := LKeyName;
              end;
          end;
      end;
    var I := 0; var LInvalidKeysHeader := False;
    for LKeyName in LKeyNames do
      begin
        LFrameRegistrySelector.lbAppPaths.Items.Add(LKeyName);
        if FRegistryKeyMode = rkmCheckInvalidPath then
          begin
            Registry.OpenKey('\' + LSavedCurrentPath, False);
            Registry.OpenKeyReadOnly(LKeyName);
            var LPath := Registry.ReadString('Path');
            var LDefault := Registry.ReadString('');
            var LDirectory := ExtractFileDir(LDefault);
            if (LDirectory = '') or not TDirectory.Exists(LDirectory) then
              begin
                LFrameRegistrySelector.lbAppPaths.Selected[I] := True;
                if not LInvalidKeysHeader then
                  begin
                    LInvalidKeysHeader := True;
                    LFrameRegistrySelector.ShowDeleteInvalidButton;
                    Log(Format('Invalid keys found under %s\%s', ['HKLM', LSavedCurrentPath]));
                  end;
                Log(Format('  Invalid key %s', [LKeyName]));
              end;
          end;
        Inc(I);
      end;
  finally
    LKeyNames.Free;
  end;
end;

procedure TfrmAppPathEditor.AddPathToListBox;
var
  LPath: string;
begin
  LPath := GetEnvironmentVariable('PATH');
  AddToListBox(LPath);
end;

procedure TfrmAppPathEditor.AddToListBox(const List: string);
var
  LFrameAppPathEditor: TFrameAppPathEditor;
  ListBox: TListBox;
  ExistingItems, NewItems: TStringList;
  LItem: string;
begin
  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;
  LFrameAppPathEditor.OnShrinkPath := function (var VPathName: string): Boolean
    begin
      Result := ShrinkPath(VPathName);
      var LPath: string := lblAppPathName.Caption;
      if ShrinkPath(LPath) then
        lblAppPathName.Caption := LPath;
    end;
  LFrameAppPathEditor.OnExpandPath := function (var VPathName: string): Boolean
    begin
      Result := ExpandPath(VPathName);
      var LPath: string := lblAppPathName.Caption;
      if ExpandPath(LPath) then
        lblAppPathName.Caption := LPath;
    end;
  ListBox := LFrameAppPathEditor.lbPaths;
  ExistingItems := TStringList.Create;
  NewItems := TStringList.Create;
  try
    // Save existing listbox ExistingItems
    ExistingItems.Delimiter        := ';';
    ExistingItems.StrictDelimiter  := True;
    ExistingItems.Duplicates := TDuplicates.dupIgnore;
    ExistingItems.AddStrings(ListBox.Items);

    // Get the given list
    NewItems.Delimiter       := ';';
    NewItems.StrictDelimiter := True;
    NewItems.Duplicates := TDuplicates.dupIgnore;
    NewItems.DelimitedText   := List;

    ListBox.Clear;
    ListBox.Items.AddStrings(ExistingItems); // restore existing ExistingItems

    for LItem in NewItems do
      if ExistingItems.IndexOf(LItem)=-1 then
        ListBox.Items.Add(LItem);
  finally
    NewItems.Free;
    ExistingItems.Free;
  end;
end;

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

  LFullPathName := lblAppPathName.Caption;
  if FAppName = '' then
    FAppName := ExtractFileName(LFullPathName);
  LAppName := FAppName;
  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;

  ListBox := LFrameAppPathEditor.lbPaths;
  FRegKeyAccess := KEY_ALL_ACCESS
  {$IF DEFINED(WIN32)}
    or KEY_WOW64_64KEY;
  {$ELSEIF DEFINED(WIN64)}
    or KEY_WOW64_32KEY
  {$ENDIF}
  ;
  OpenRegistryKey;
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
        CheckEnableDelete;
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
      LFrameAppPathEditor.Initialize;
      LFrameAppPathEditor.AddPath := procedure (const APath: string) begin
        AddToListBox(APath);
      end;
    end else
    begin
      LFrameAppPathEditor := TFrameAppPathEditor(FFrameAppPathEditor);
      LFrameAppPathEditor.edPath.Text := '';
    end;
  FFrameAppPathEditor := LFrameAppPathEditor;
end;

procedure TfrmAppPathEditor.DeleteAppPathKey(const AKeyName: string);
begin
  Registry.DeleteKey(AKeyName);
  Log(Format('%s deleted from %s', [AKeyName, FBaseKey]));
end;

procedure TfrmAppPathEditor.CheckEnableDelete;
var
  LEnabled: Boolean;
begin
  LEnabled := FAppName <> '';
  acAppPathDelete.Enabled := LEnabled;
  if LEnabled then
    Log(Format('You can now delete %s if you want to.', [FAppName])) else
    Log('Delete is now disabled.');
end;

procedure TfrmAppPathEditor.EnableDeleteIfAppPathExist;
begin
  var LSavedKey := Registry.CurrentKey;
  var LSavedPath := Registry.CurrentPath;
  Registry.CloseKey;
  Registry.OpenKey(FBaseKey, False);
  if Registry.KeyExists(FAppName) then
    begin
      CheckEnableDelete;
    end;
  Registry.CloseKey;
  Registry.OpenKey(LSavedPath, False);
end;

procedure TfrmAppPathEditor.CheckEnableSave;
begin
  acAppPathSave.Enabled :=   FAppPath <> '';
  acAppPathSaveAs.Enabled := FAppPath <> '';
end;

procedure TfrmAppPathEditor.ClearListBox;
var
  LFrameAppPathEditor: TFrameAppPathEditor;
  ListBox: TListBox;
  ExistingItems, NewItems: TStringList;
  LItem: string;
begin
  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;
  ListBox := LFrameAppPathEditor.lbPaths;
  ListBox.Items.Clear;
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

procedure TfrmAppPathEditor.FormCreate(Sender: TObject);
begin
  FRegKeyAccess := KEY_ALL_ACCESS
  {$IF DEFINED(WIN32)}
    or KEY_WOW64_64KEY;
  {$ELSEIF DEFINED(WIN64)}
    or KEY_WOW64_32KEY
  {$ENDIF}
  ;
  Registry := TRegistry.Create;
  Registry.Access := FRegKeyAccess;
  BuildEnvironmentVarsDictionary;

  CreateFrame;

  acAppPathCancel.Enabled := False;
  acAppPathSave.Enabled := False;
  acAppPathSaveAs.Enabled := False;

  RegisterMessageListener;

  Application.OnIdle := OnApplicationIdle;
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
// Loads the App Path { LoadAppPath }
  TFrame(LFrameRegistrySelector) := FFrameRegistrySelector;
  LKeyName := LFrameRegistrySelector.AppName;
  FAppName := LKeyName;
  Registry.OpenKeyReadOnly( FBaseKey + '\' + LKeyName);
  LFullPath := Registry.ReadString('');
  LAppPath := Registry.ReadString('Path');
  FAppPath := LAppPath;
  lblAppPathName.Caption := LFullPath;

  TFrame(LFrameAppPathEditor) := FFrameAppPathEditor;
  if Assigned(LFrameAppPathEditor) then
    LFrameAppPathEditor.lbPaths.Clear;

  TThread.ForceQueue(nil, procedure
    begin
      ShowAppPathEditor;
      AddToListBox(LAppPath);
    end);

  CheckEnableSave;
  CheckEnableDelete;
end;

procedure TfrmAppPathEditor.Log(const AMsg: string);
var
  LTimedMsg: string;
begin
  LTimedMsg := Format('%s - %s', [FormatDateTime('hh:nn:ss', Now), AMsg]);
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Lines.Add(LTimedMsg);
    Memo1.Perform(EM_LINESCROLL, 0, Memo1.Lines.Count-1);
  finally
    Memo1.Lines.EndUpdate;
  end;
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
  if rbHKLM.Checked then
    begin
      Registry.RootKey := HKEY_LOCAL_MACHINE;
      FRegistryKeyMode := rkmCheckInvalidPath;
    end else
    begin
      Registry.RootKey := HKEY_CURRENT_USER;
      FRegistryKeyMode := rkmNoChecks;
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

  LMM.SubscribeToMessage(TMessageEnableDelete,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      CheckEnableDelete;
    end
  );

  LMM.SubscribeToMessage(TMessageEnableDeleteIfAppPathExist,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      EnableDeleteIfAppPathExist;
    end
  );

  LMM.SubscribeToMessage(TShrinkPathMsg,
    procedure(const Sender: TObject; const M: TMessage)
    var
      LShrinkPathMsg: TShrinkPathMsg absolute M;
      LPath: string;
    begin
      LPath := LShrinkPathMsg.Path;
      ShrinkPath(LPath);
      if LShrinkPathMsg.Path <> LPath then
        LShrinkPathMsg.Path := LPath;
      LPath := lblAppPathName.Caption;
      if ShrinkPath(LPath) then
        lblAppPathName.Caption := LPath;
    end
  );

  LMM.SubscribeToMessage(TExpandPathMsg,
    procedure(const Sender: TObject; const M: TMessage)
    var
      LExpandPathMsg: TExpandPathMsg absolute M;
    begin
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

  LMM.SubscribeToMessage(TMessageIdle,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      CheckEnableSave;
    end
  );

  LMM.SubscribeToMessage(TMessageGoInitialState,
    procedure(const Sender: TObject; const M: TMessage)
    begin
      // Go to app's initial state.
      FAppName := '';
      FAppPath := '';
      FBaseKey := '';
      rbHKCU.Checked := True;
      lblAppPathName.Caption := '';
      CheckEnableDelete;
      Registry.CloseKey;
      for var I := 0 to pnlContainer.ControlCount-1 do
        if pnlContainer.Controls[I] is TFrame then
          pnlContainer.RemoveControl(pnlContainer.Controls[I]);
    end
  )

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

procedure TfrmAppPathEditor.miAboutClick(Sender: TObject);
begin
  ShowAbout;
end;

procedure TfrmAppPathEditor.miShowInstructionsClick(Sender: TObject);
begin
  memInstructions.Visible := not miShowInstructions.Checked;
  miShowInstructions.Checked := not miShowInstructions.Checked;
end;

procedure TfrmAppPathEditor.miShowLogClick(Sender: TObject);
begin
  miShowLog.Checked := not miShowLog.Checked;
  GroupBoxLog.Visible := miShowLog.Checked;
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

end.
