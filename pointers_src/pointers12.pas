program pointers12;

uses
  Strings;

var
  sName: string;
  pName, pMessage: PChar;

begin
  Write('Please, enter your name? '); Readln(sName);
  if sName <> '' then begin
    sName += #0;
    pName := @sName[1];
    pMessage := StrAlloc(StrLen(pName) + 7 + 1);
    pMessage := StrMove(pMessage, 'Hello ', 6 + 1);
    StrCat(pMessage, pName);
    StrCat(pMessage, '!');
    WriteLn(pMessage);
    Writeln; Write('ENTER to terminate '); Readln;
    end;
end.
