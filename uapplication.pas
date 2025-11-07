unit uApplication;

{$mode objfpc}{$H+}

interface

procedure InitApp;
procedure ExitApp;

implementation

uses
  Classes,
  SysUtils,
  uDict,
  uDictEntry,
  uGlobalData;

procedure Test;
var
  DE: TDictEntry;
  SL: TStringList;
  D: TDict;
  k: string;
  i: integer;
begin
  writeln('************************* TEST *************************');
  DE := TDictEntry.Create('Hallo'#9'Text1'#9'Text3'#9'äöüÄÖÜß'#9);
  writeln(DE.Display);
  writeln(DE.ToString);
  writeln(DE.DataLength);
  writeln(DE.GetKey);
  writeln(DE.GetData);
  writeln(DE.GetDataAt(0));
  writeln(DE.GetDataAt(1));
  writeln(DE.GetDataAt(2));
  writeln(DE.GetDataAt(3));
  writeln(DE.GetDataAt(4));
  writeln(DE.GetDataAt(5));
  writeln;
  DE.Free;
  DE := TDictEntry.Create('   Hallo  ');
  writeln(DE.Display);
  writeln(DE.ToString);
  writeln;
  DE.Free;
  DE := TDictEntry.Create('   Hello  '#9' A ');
  writeln(DE.Display);
  writeln(DE.ToString);
  writeln;
  DE.Free;
  DE := TDictEntry.Create(''#9''#9'');
  writeln(DE.Display);
  writeln(DE.ToString);
  writeln;
  DE.Free;
  writeln('---');
  D := TDict.Create;
  writeln('import:', D.FileImport('./data/jvocab.txt'));
  Writeln(D.Length);
  Writeln(D.KeyExists('hier'));
  Writeln(D.KeyExists('Key91'));
  Writeln(D.GetDictEntry('hier').ToString);
  Writeln(D.GetDictEntry('hier') = nil);
  Writeln(D.Remove('hier'));
  Writeln(D.GetDictEntry('hier') = nil);
  Writeln(D.Length);
  Writeln('import:', D.FileImport('./data/y.txt'));
  Writeln(D.ToString);
  Writeln(D.Length);
  Writeln('import:', D.FileImport('./data/wrong.txt'));
  Writeln('import:', D.FileImport('./data/jvocab.txt'));
  Writeln(D.Length);
  SL := TStringList.Create;
  D.GetSortedKeysList(SL, '');
  Writeln(SL.CommaText);
  D.Clear;
  D.AddString('Hallo'#9'Welt');
  D.AddString('hey'#9'World');
  D.AddString('hallo'#9'Mundo');
  D.GetSortedKeysList(SL, '');
  Writeln(SL.CommaText);
  Writeln(D.ToString);
  Writeln(D.Length);
  for k in SL do
    WriteLn(k, '=', D.GetDictEntry(k).GetData);
  writeln('import:', D.FileImport('./data/jvocab.txt'));
  D.GetSortedKeysList(SL, '');
  i := 1;
  for k in SL do
  begin
    WriteLn(i, ') ', D.GetDictEntry(k).ToString);
    Inc(i);
    if i = 21 then
      break;
  end;
  D.Free;
  SL.Free;
  writeln('********************** TEST ENDE ***********************');
end;

procedure InitApp;
begin
  // Test;
  Dict := TDict.Create;
  KeyList := TStringList.Create;
end;

procedure ExitApp;
begin
  FreeAndNil(KeyList);
  FreeAndNil(Dict);
end;

end.
