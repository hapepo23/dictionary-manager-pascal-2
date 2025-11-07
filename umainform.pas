unit uMainForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  Menus,
  uGlobalData,
  uDict;

type

  { TMainForm }

  TMainForm = class(TForm)
    labMessage: TLabel;
    menuMain: TMainMenu;
    menuItem1: TMenuItem;
    menuItem2: TMenuItem;
    menuItem3: TMenuItem;
    menuItem11: TMenuItem;
    menuItem12: TMenuItem;
    menuItem21: TMenuItem;
    menuItem22: TMenuItem;
    menuItem23: TMenuItem;
    menuItem24: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure menuItem11Click(Sender: TObject);
    procedure menuItem12Click(Sender: TObject);
    procedure menuItem21Click(Sender: TObject);
    procedure menuItem22Click(Sender: TObject);
    procedure menuItem23Click(Sender: TObject);
    procedure menuItem24Click(Sender: TObject);
    procedure menuItem3Click(Sender: TObject);
  private
    procedure ShowDictInfo;
  end;

var
  MainForm: TMainForm;

implementation

uses
  uApplication,
  uViewEditForm;

  {$R *.lfm}

procedure TMainForm.FormShow(Sender: TObject);
begin
  InitApp;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
  if Dict.IsDirty then
  begin
    CanClose := (MessageDlg('The application will quit and all changes will be lost.' +
      LineEnding + LineEnding + 'Are you sure?', mtConfirmation,
      [mbYes, mbNo], 0) = mrYes);
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ExitApp;
end;

procedure TMainForm.menuItem11Click(Sender: TObject);
begin
  ShowMessage('Dictionary2 Manager Version 1.0' + LineEnding +
    '(2025-11-07, ObjectPascal/Lazarus Version)');
end;

procedure TMainForm.menuItem12Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.menuItem21Click(Sender: TObject);
var
  ok: boolean;
begin
  ok := True;
  if Dict.IsDirty then
  begin
    ok := (MessageDlg('All changes will be lost.' + LineEnding +
      LineEnding + 'Are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;
  if ok then
  begin
    Dict.Clear;
    ShowDictInfo;
    menuItem23.Enabled := False;
  end;
end;

procedure TMainForm.menuItem22Click(Sender: TObject);
var
  ok: boolean;
begin
  ok := True;
  if Dict.IsDirty then
  begin
    ok := (MessageDlg('All changes will be lost.' + LineEnding +
      LineEnding + 'Are you sure?', mtConfirmation, [mbYes, mbNo], 0) = mrYes);
  end;
  if ok then
  begin
    if dlgOpen.Execute then
    begin
      if not Dict.FileImport(dlgOpen.FileName) then
        MessageDlg(dlgOpen.FileName + ' could not be loaded.', mtError, [mbOK], 0)
      else
        menuItem23.Enabled := True;
    end
    else
      MessageDlg('Aborted.',
        mtInformation, [mbOK], 0);
  end;
  ShowDictInfo;
end;

procedure TMainForm.menuItem23Click(Sender: TObject);
var
  ok: boolean;
begin
  ok := Dict.FileExportOld;
  if ok then
    MessageDlg(Dict.Filename + ' successfully saved.',
      mtInformation, [mbOK], 0)
  else
    MessageDlg(Dict.Filename + ' could not be saved.', mtError, [mbOK], 0);
  ShowDictInfo;
end;

procedure TMainForm.menuItem24Click(Sender: TObject);
var
  ok: boolean;
begin
  if dlgSave.Execute then
  begin
    if FileExists(dlgSave.FileName) then
    begin
      ok := (MessageDlg('The file ' + dlgSave.FileName + ' already exists.' +
        LineEnding + LineEnding + 'Overwrite this file?', mtConfirmation,
        [mbYes, mbNo], 0) = mrYes);
      if not ok then
        MessageDlg('File not overwritten.', mtInformation, [mbOK], 0);
    end
    else
      ok := True;
    if ok then
    begin
      ok := Dict.FileExportNew(dlgSave.FileName);
      if ok then
      begin
        MessageDlg(dlgSave.FileName + ' successfully saved.',
          mtInformation, [mbOK], 0);
        menuItem23.Enabled := True;
      end
      else
        MessageDlg(dlgSave.FileName + ' could not be saved.', mtError, [mbOK], 0);
    end;
  end
  else
    MessageDlg('Aborted.', mtInformation, [mbOK], 0);
  ShowDictInfo;
end;

procedure TMainForm.menuItem3Click(Sender: TObject);
begin
  ViewEditForm.ShowModal;
  ShowDictInfo;
end;

procedure TMainForm.ShowDictInfo;
var
  t: string;
begin
  if (Dict.Filename = '') and (Dict.Length = 0) and (not Dict.IsDirty) then
    t := 'New dictionary'
  else if (Dict.Filename = '') and (Dict.IsDirty) then
    t := 'New dictionary' + LineEnding + 'Number of entries: ' +
      IntToStr(Dict.Length) + LineEnding + '*** CHANGES MUST BE SAVED ***'
  else
  begin
    t := 'Dictionary path name: ' + ExtractFileDir(Dict.Filename) +
      LineEnding + 'Dictionary file name: ' + ExtractFileName(Dict.Filename) +
      LineEnding + 'Number of entries: ' + IntToStr(Dict.Length) + LineEnding;
    if Dict.IsDirty then
      t := t + '*** CHANGES MUST BE SAVED ***'
    else
      t := t + '(UNCHANGED)';
  end;
  labMessage.Caption := t;
end;

end.
