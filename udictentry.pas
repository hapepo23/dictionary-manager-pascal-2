unit uDictEntry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Types;

type
  TDictEntry = class(TObject)
  private
    sKey: string;
    sData: string;                               // NL-delimited
  public
    constructor Create(const pLine: string);     // TAB-delimited
    function DataLength: integer;
    function GetDataAt(const index: integer): string;
    procedure SetDataAt(const index: integer; const s: string);
    procedure SetData(const s: string);          // NL-delimited
    function ToString: string; override;         // TAB-delimited
    function Display: string;
    function SplitData: TStringDynArray;
    procedure JoinData(const Parts: TStringDynArray; const From: integer);
    property GetKey: string read sKey;
    property GetData: string read sData;         // NL-delimited
  end;

implementation

constructor TDictEntry.Create(const pLine: string);
var
  Parts: TStringDynArray;
begin
  inherited Create;
  Parts := pLine.Split(#9);
  if Length(Parts) > 1 then
  begin
    sKey := Trim(Parts[0]);
    JoinData(Parts, 1);
  end
  else
  begin
    sKey := Trim(Parts[0]);
    sData := '';
  end;
end;

function TDictEntry.DataLength: integer;
begin
  Result := Length(SplitData);
end;

function TDictEntry.GetDataAt(const index: integer): string;
var
  Parts: TStringDynArray;
begin
  Parts := SplitData;
  if (index >= 0) and (index < Length(Parts)) then
    Result := Parts[index]
  else
    Result := '';
end;

procedure TDictEntry.SetDataAt(const index: integer; const s: string);
var
  Parts: TStringDynArray;
begin
  Parts := SplitData;
  if (index >= 0) and (index < Length(Parts)) then
  begin
    Parts[index] := s;
    JoinData(Parts, 0);
  end;
end;

procedure TDictEntry.SetData(const s: string);
begin
  sData := s;
end;

function TDictEntry.ToString: string;
begin
  Result := sKey + #9 + StringReplace(sData, LineEnding, #9, [rfReplaceAll]);
end;

function TDictEntry.Display: string;
begin
  Result := 'key="' + sKey + '", data="' + StringReplace(sData,
    LineEnding, '|', [rfReplaceAll]) + '"';
end;

function TDictEntry.SplitData: TStringDynArray;
begin
  Result := sData.Split([LineEnding]);
end;

procedure TDictEntry.JoinData(const Parts: TStringDynArray; const From: integer);
begin
  sData := string.Join(LineEnding, Parts, From, High(Parts));
end;

end.
