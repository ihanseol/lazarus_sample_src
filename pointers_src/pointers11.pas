program pointers11;

var
  sName, sMessage: string;
  pMessage: PChar;

begin
  repeat
    Write('Please, enter your name? '); Readln(sName);
    if sName = '' then
      sMessage := 'Hello World!' + #0
    else
      sMessage := 'Hello ' + sName + '!' + #0;
    pMessage := @sMessage[1];
    Writeln(pMessage);
  until sName = '';
  Writeln; Write('ENTER to terminate '); Readln;
end.
