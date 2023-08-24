unit AppPathEditor.AboutDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmAbout = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    RichEdit1: TRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAbout;

var
  frmAbout: TfrmAbout;

implementation

{$R *.dfm}

procedure ShowAbout;
begin
  frmAbout := TfrmAbout.Create(Application);
  frmAbout.ShowModal;
  frmAbout.Free;
end;

end.
