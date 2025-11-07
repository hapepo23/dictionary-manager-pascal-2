program Dictionary2;

{$mode objfpc}{$H+}

uses
  Forms,
  Interfaces,
  uMainForm,
  uViewEditForm,
  uMemoDialog;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TViewEditForm, ViewEditForm);
  Application.CreateForm(TMemoDialog, MemoDialog);
  Application.Run;
end.
