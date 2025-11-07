unit uMemoDialog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMemoDialog }

  TMemoDialog = class(TForm)
    butCancel: TButton;
    butOK: TButton;
    mmText: TMemo;
    procedure butCancelClick(Sender: TObject);
    procedure butOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    oldText: string;
    newText: string;
  public
    function Execute(const sTitle: string; const sText: string;
      const ReadOnly: boolean): string;
  end;

var
  MemoDialog: TMemoDialog;

implementation

{$R *.lfm}

procedure TMemoDialog.butOKClick(Sender: TObject);
begin
  MemoDialog.Visible := False;
  newText := mmText.Lines.Text;
end;

procedure TMemoDialog.butCancelClick(Sender: TObject);
begin
  MemoDialog.Visible := False;
  newText := oldText;
end;

procedure TMemoDialog.FormShow(Sender: TObject);
begin
  MemoDialog.Visible := True;
  mmText.SetFocus;
end;

function TMemoDialog.Execute(const sTitle: string; const sText: string;
  const ReadOnly: boolean): string;
begin
  Caption := sTitle;
  butOK.Enabled := not ReadOnly;
  oldText := sText;
  mmText.Lines.Text := sText;
  mmText.ReadOnly := ReadOnly;
  ShowModal;
  Result := newText;
end;

end.
