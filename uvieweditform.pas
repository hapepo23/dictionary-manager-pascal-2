unit uViewEditForm;

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
  ExtCtrls,
  ValEdit,
  Grids,
  uGlobalData,
  uDict,
  uDictEntry,
  uMemoDialog;

type
  TEditMode = (emNone, emAdd, emUpdate);

type

  { TViewEditForm }

  TViewEditForm = class(TForm)
    butAdd: TButton;
    butDelete: TButton;
    butEmptyFilter: TButton;
    butSave: TButton;
    butUpdate: TButton;
    edtFilter: TEdit;
    labData: TLabel;
    labFilter: TLabel;
    labKey: TLabel;
    lbxKeys: TListBox;
    vleData: TValueListEditor;
    mmKey: TMemo;
    pnLeft: TPanel;
    pnRight: TPanel;
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure butAddClick(Sender: TObject);
    procedure butDeleteClick(Sender: TObject);
    procedure butEmptyFilterClick(Sender: TObject);
    procedure butSaveClick(Sender: TObject);
    procedure butUpdateClick(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure lbxKeysSelectionChange(Sender: TObject; User: boolean);
    procedure mmKeyChange(Sender: TObject);
    procedure vleDataDblClick(Sender: TObject);
  private
    Mode: TEditMode;
    procedure SetWidgets(const KeysPos: integer; const AddBut: boolean;
      const UpdBut: boolean; const DelBut: boolean; const ClrKeyData: boolean;
      const SavBut: boolean);
    procedure DisplayDataAt(index: integer);
    procedure FillValueListEditorCellsFrom(const DE: TDictEntry);
    procedure FillValueListEditorCellsEmpty;
    procedure FillValueListEditorCellsForUpdate;
  end;

var
  ViewEditForm: TViewEditForm;

implementation

{$R *.lfm}

procedure TViewEditForm.FormShow(Sender: TObject);
begin
  edtFilter.Text := '';
  Mode := emNone;
  Dict.GetSortedKeysList(KeyList, '');
  lbxKeys.Items := KeyList;
  DisplayDataAt(0);
  SetWidgets(0, True, False, False, (lbxKeys.SelCount = 0), False);
  lbxKeys.SetFocus;
end;

procedure TViewEditForm.FormResize(Sender: TObject);
begin
  pnLeft.Width := ViewEditForm.Width div 3;
  pnRight.Width := ViewEditForm.Width - pnLeft.Width;
end;

procedure TViewEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  vleData.Clear;
  CloseAction := caHide;
end;

procedure TViewEditForm.edtFilterChange(Sender: TObject);
begin
  Dict.GetSortedKeysList(KeyList, Trim(edtFilter.Text));
  lbxKeys.Items := KeyList;
  Mode := emNone;
  SetWidgets(0, True, False, False, True, False);
end;

procedure TViewEditForm.butEmptyFilterClick(Sender: TObject);
begin
  edtFilter.Text := '';
  if Dict.Length > 0 then
    lbxKeys.ItemIndex := 0;
  lbxKeys.SetFocus;
end;

procedure TViewEditForm.butDeleteClick(Sender: TObject);
var
  LastSel, i: integer;
begin
  LastSel := 0;
  Mode := emNone;
  if lbxKeys.SelCount > 0 then
  begin
    for i := lbxKeys.Items.Count - 1 downto 0 do
    begin
      if lbxKeys.Selected[i] then
      begin
        LastSel := i;
        Dict.Remove(lbxKeys.Items[i]);
      end;
    end;
    Dict.GetSortedKeysList(KeyList, Trim(edtFilter.Text));
    lbxKeys.Items := KeyList;
    if LastSel > lbxKeys.Count - 1 then
      LastSel := lbxKeys.Count - 1;
    if lbxKeys.Count > 0 then
      lbxKeys.ItemIndex := LastSel
    else
    begin
      SetWidgets(-1, True, False, False, True, False);
    end;
    lbxKeys.SetFocus;
  end;
end;

procedure TViewEditForm.butAddClick(Sender: TObject);
var
  i: integer;
begin
  if lbxKeys.SelCount > 0 then
    for i := lbxKeys.Items.Count - 1 downto 0 do
      if lbxKeys.Selected[i] then
        lbxKeys.Selected[i] := False;
  Mode := emAdd;
  SetWidgets(-1, True, False, False, True, False);
  FillValueListEditorCellsEmpty;
  mmKey.SetFocus;
end;

procedure TViewEditForm.butSaveClick(Sender: TObject);
var
  s, k: string;
  i, pos: integer;
  ok: boolean;
begin
  k := Trim(mmKey.Lines.Text);
  s := k;
  for i := 1 to vleData.RowCount - 1 do
  begin
    s := s + #9 + vleData.Cells[1, i];
  end;
  s := TrimRight(s);
  ok := False;
  if Mode = emAdd then
  begin
    if not Dict.AddString(s) then
      MessageDlg('Key "' + k + '" already exists or is empty!', mtError, [mbOK], 0)
    else
      ok := True;
  end;
  if Mode = emUpdate then
  begin
    if Dict.Remove(k) then
    begin
      if Dict.AddString(s) then
        ok := True;
    end;
    if not ok then
      MessageDlg('Update unsuccessful!', mtError, [mbOK], 0);
  end;
  if ok then
  begin
    Dict.GetSortedKeysList(KeyList, Trim(edtFilter.Text));
    lbxKeys.Items := KeyList;
    pos := 0;
    for i := 0 to lbxKeys.Items.Count - 1 do
    begin
      if lbxKeys.Items[i] = k then
      begin
        pos := i;
        break;
      end;
    end;
    Mode := emNone;
    SetWidgets(pos, True, True, True, False, False);
    lbxKeys.SetFocus;
  end;
end;

procedure TViewEditForm.butUpdateClick(Sender: TObject);
begin
  Mode := emUpdate;
  SetWidgets(-1, True, False, False, False, True);
  FillValueListEditorCellsForUpdate;
  vleData.SetFocus;
end;

procedure TViewEditForm.lbxKeysSelectionChange(Sender: TObject; User: boolean);
var
  i, pos: integer;
begin
  Mode := emNone;
  if lbxKeys.SelCount = 1 then
  begin
    pos := 0;
    for i := lbxKeys.Items.Count - 1 downto 0 do
      if lbxKeys.Selected[i] then
        pos := i;
    DisplayDataAt(pos);
    SetWidgets(pos, True, True, True, False, False);
  end
  else
    SetWidgets(-1, True, False, (lbxKeys.SelCount > 0), True, False);
end;

procedure TViewEditForm.mmKeyChange(Sender: TObject);
var
  k: string;
begin
  if Mode = emAdd then
  begin
    k := Trim(mmKey.Lines.Text);
    if k = '' then
      butSave.Enabled := False
    else
      butSave.Enabled := (not Dict.KeyExists(k));
  end;
end;

procedure TViewEditForm.vleDataDblClick(Sender: TObject);
begin
  vleData.Cells[1, vleData.Row] :=
    MemoDialog.Execute(vleData.Cells[0, vleData.Row],
    vleData.Cells[1, vleData.Row], (Mode = emNone));
end;

procedure TViewEditForm.SetWidgets(const KeysPos: integer; const AddBut: boolean;
  const UpdBut: boolean; const DelBut: boolean; const ClrKeyData: boolean;
  const SavBut: boolean);
begin
  butAdd.Enabled := AddBut;
  butUpdate.Enabled := UpdBut;
  butDelete.Enabled := DelBut;
  butSave.Enabled := SavBut;
  if ClrKeyData then
  begin
    mmKey.Lines.Clear;
    vleData.Clear;
  end;
  mmKey.ReadOnly := not (Mode = emAdd);
  if Mode = emNone then
    vleData.Options := vleData.Options - [goEditing]
  else
    vleData.Options := vleData.Options + [goEditing];
  if ((lbxKeys.Count > 0) and (KeysPos < lbxKeys.Count) and (KeysPos >= 0)) then
  begin
    lbxKeys.ItemIndex := KeysPos;
  end;
  if ((Mode = emAdd) or (Mode = emUpdate)) then
  begin
    edtFilter.ReadOnly := True;
    butEmptyFilter.Enabled := False;
  end
  else
  begin
    edtFilter.ReadOnly := False;
    butEmptyFilter.Enabled := True;
  end;
end;

procedure TViewEditForm.DisplayDataAt(index: integer);
var
  DE: TDictEntry;
begin
  if ((lbxKeys.Count > 0) and (index < lbxKeys.Count) and (index >= 0)) then
  begin
    DE := Dict.GetDictEntry(lbxKeys.Items[index]);
    FillValueListEditorCellsFrom(DE);
  end;
end;

procedure TViewEditForm.FillValueListEditorCellsFrom(const DE: TDictEntry);
var
  i: integer;
begin
  if Dict.GetColHeader = nil then
    labKey.Caption := 'Key:'
  else
    labKey.Caption := Dict.GetColHeader.GetKey + ':';
  mmKey.Lines.Text := DE.GetKey;
  vleData.Clear;
  vleData.Cells[0, 0] := 'Name';
  vleData.Cells[1, 0] := 'Value';
  for i := 1 to (DE.DataLength) do
  begin
    if Dict.GetColHeader = nil then
      vleData.Cells[0, i] := IntToStr(i)
    else
    begin
      vleData.Cells[0, i] := Dict.GetColHeader.GetDataAt(i - 1);
      if vleData.Cells[0, i] = '' then
        vleData.Cells[0, i] := IntToStr(i);
    end;
    vleData.Cells[1, i] := DE.GetDataAt(i - 1);
  end;
  vleData.Row := 1;
end;

procedure TViewEditForm.FillValueListEditorCellsEmpty;
var
  i, l: integer;
begin
  if Dict.GetColHeader = nil then
    labKey.Caption := 'Key:'
  else
    labKey.Caption := Dict.GetColHeader.GetKey + ':';
  vleData.Cells[0, 0] := 'Name';
  vleData.Cells[1, 0] := 'Value';
  if Dict.GetColHeader = nil then
    l := 20
  else
    l := Dict.GetColHeader.DataLength;
  for i := 1 to l do
  begin
    if Dict.GetColHeader = nil then
      vleData.Cells[0, i] := IntToStr(i)
    else
    begin
      vleData.Cells[0, i] := Dict.GetColHeader.GetDataAt(i - 1);
      if vleData.Cells[0, i] = '' then
        vleData.Cells[0, i] := IntToStr(i);
    end;
    vleData.Cells[1, i] := '';
  end;
  vleData.Row := 1;
end;

procedure TViewEditForm.FillValueListEditorCellsForUpdate;
var
  i, l: integer;
begin
  if Dict.GetColHeader = nil then
    l := 20
  else
    l := Dict.GetColHeader.DataLength;
  for i := 1 to l do
  begin
    if vleData.Cells[0, i] = '' then
    begin
      if Dict.GetColHeader = nil then
        vleData.Cells[0, i] := IntToStr(i);
    end;
  end;
  vleData.Row := 1;
end;

end.
