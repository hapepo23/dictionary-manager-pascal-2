unit uDict;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  StrUtils,
  Generics.Collections,
  uDictEntry;

type
  TStrDEDictionary = specialize TDictionary<string, TDictEntry>;

type
  TDict = class(TObject)
  private
    sData: TStrDEDictionary;
    sFilename: string;
    sDirty: boolean;
    sColHeader: TDictEntry;
  public
    constructor Create;
    destructor Destroy; override;
    function AddDictEntry(var pDE: TDictEntry): boolean;
    function AddString(const pLine: string): boolean;
    function SetColHeaderString(const pLine: string): boolean;
    function GetDictEntry(const pKey: string): TDictEntry;
    function Remove(const pKey: string): boolean;
    function FileImport(const pFilePath: string): boolean;
    function FileExportNew(const pFilePath: string): boolean;
    function FileExportOld: boolean;
    function KeyExists(const pKey: string): boolean;
    function Length: integer;
    function ToString: string; override;
    procedure Clear;
    procedure GetKeysList(const pKeys: TStringList; const pFilter: string);
    procedure GetSortedKeysList(const pSortedKeys: TStringList; const pFilter: string);
    property IsDirty: boolean read sDirty;
    property Filename: string read sFilename;
    property GetColHeader: TDictEntry read sColHeader;
  end;

implementation

constructor TDict.Create;
begin
  inherited Create;
  sData := TStrDEDictionary.Create;
  sFilename := '';
  sDirty := False;
  sColHeader := nil;
end;

destructor TDict.Destroy;
begin
  Clear;
  FreeAndNil(sData);
  FreeAndNil(sColHeader);
  inherited Destroy;
end;

function TDict.AddDictEntry(var pDE: TDictEntry): boolean;
begin
  if pDE.GetKey = '' then
    Result := False
  else if sData.ContainsKey(pDE.GetKey) then
    Result := False
  else
  begin
    sData.Add(pDE.GetKey, pDE);
    sDirty := True;
    Result := True;
  end;
  if not Result then // pDE freed if not added
  begin
    //Writeln('FREE:',pDE.Display);
    FreeAndNil(pDE);
  end;
end;

function TDict.AddString(const pLine: string): boolean;
var
  DE: TDictEntry;
begin
  DE := TDictEntry.Create(pLine);
  Result := AddDictEntry(DE);
end;

function TDict.SetColHeaderString(const pLine: string): boolean;
var
  s: string;
begin
  if pLine[1] = '#' then
  begin
    s := Copy(pLine, 2);
    FreeAndNil(sColHeader);
    sColHeader := TDictEntry.Create(s);
    Result := True;
  end
  else
    Result := False;
end;

function TDict.GetDictEntry(const pKey: string): TDictEntry;
begin
  Result := nil;
  if sData.ContainsKey(pKey) then
    Result := sData.Items[pKey];
end;

function TDict.Remove(const pKey: string): boolean;
begin
  Result := False;
  if sData.ContainsKey(pKey) then
  begin
    sData.Items[pKey].Free;
    sData.Remove(pKey);
    sDirty := True;
    Result := True;
  end;
end;

function TDict.FileImport(const pFilePath: string): boolean;
var
  myFile: TextFile;
  line: string;
  start: boolean;
begin
  if not FileExists(pFilePath) then
    Result := False
  else
  begin
    try
      start := True;
      AssignFile(myFile, pFilePath);
      Reset(myFile);
      Clear;
      while not EOF(myFile) do
      begin
        ReadLn(myFile, line);
        if start then
        begin
          if not SetColHeaderString(line) then
            AddString(line);
          start := False;
        end
        else
          AddString(line);
      end;
      CloseFile(myFile);
      sFilename := pFilePath;
      sDirty := False;
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function TDict.FileExportNew(const pFilePath: string): boolean;
var
  myFile: TextFile;
  DE: TDictEntry;
begin
  try
    AssignFile(myFile, pFilePath);
    ReWrite(myFile);
    if sColHeader <> nil then
      WriteLn(myfile, '#' + sColHeader.ToString);
    for DE in sData.Values do
      WriteLn(myfile, DE.ToString);
    CloseFile(myFile);
    sFilename := pFilePath;
    sDirty := False;
    Result := True;
  except
    Result := False;
  end;
end;

function TDict.FileExportOld: boolean;
begin
  Result := FileExportNew(sFilename);
end;

function TDict.KeyExists(const pKey: string): boolean;
begin
  Result := sData.ContainsKey(pKey);
end;

function TDict.Length: integer;
begin
  Result := sData.Count;
end;

function TDict.ToString: string;
var
  line: string;
  DE: TDictEntry;
begin
  line := '';
  if sColHeader <> nil then
    line := line + '#' + sColHeader.ToString + LineEnding;
  for DE in sData.Values do
    line := line + DE.ToString + LineEnding;
  Result := line;
end;

procedure TDict.Clear;
var
  DE: TDictEntry;
begin
  for DE in sData.Values do
    DE.Free;
  sData.Clear;
  sFilename := '';
  sDirty := False;
  FreeAndNil(sColHeader);
end;

procedure TDict.GetKeysList(const pKeys: TStringList; const pFilter: string);
var
  Key: string;
  trimmedfilter: string;
begin
  pKeys.Clear;
  pKeys.CaseSensitive := False;
  pKeys.Sorted := False;
  trimmedfilter := Trim(pFilter);
  if trimmedfilter = '' then
  begin
    for Key in sData.Keys do
      pKeys.Add(Key);
  end
  else
  begin
    for Key in sData.Keys do
    begin
      if AnsiContainsText(Key, trimmedfilter) then
      begin
        pKeys.Add(Key);
      end
      else
      begin
        if AnsiContainsText(sData.Items[Key].GetData, trimmedfilter) then
          pKeys.Add(Key);
      end;
    end;
  end;
end;

procedure TDict.GetSortedKeysList(const pSortedKeys: TStringList; const pFilter: string);
begin
  GetKeysList(pSortedKeys, pFilter);
  pSortedKeys.Sort;
end;

end.
