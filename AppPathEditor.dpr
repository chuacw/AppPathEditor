program AppPathEditor;

uses
  Vcl.Forms,
  AppPathEditor.Messages in 'Common\AppPathEditor.Messages.pas',
  AppPathEditor.Editor in 'AppPathEditor.Editor.pas' {FrameAppPathEditor: TFrame},
  AppPathEditor.Main in 'AppPathEditor.Main.pas' {frmAppPathEditor},
  AppPathEditor.SelectAppPath in 'AppPathEditor.SelectAppPath.pas' {FrameAppPathSelector: TFrame},
  AppPathEditor.Types in 'Common\AppPathEditor.Types.pas',
  AppPathEditor.AboutDlg in 'AppPathEditor.AboutDlg.pas' {frmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAppPathEditor, frmAppPathEditor);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
