program FMXAppPathEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXAppPathEditor.Main in 'FMXAppPathEditor.Main.pas' {frmAppPathEditor},
  FMXAppPathEditor.SelectAppPath in 'FMXAppPathEditor.SelectAppPath.pas' {FrameAppPathSelector: TFrame},
  FMXAppPathEditor.Editor in 'FMXAppPathEditor.Editor.pas' {FrameAppPathEditor: TFrame},
  Winapi.ShLwApi,
  AppPathEditor.Messages in 'Common\AppPathEditor.Messages.pas',
  AppPathEditor.Types in 'Common\AppPathEditor.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAppPathEditor, frmAppPathEditor);
  Application.Run;
end.
