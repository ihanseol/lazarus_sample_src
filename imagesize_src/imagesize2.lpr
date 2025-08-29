{ Determination of image size using "picslib" unit }

program imagesize2;

// File formats supported: BMP, GIF, JPG, PNG, TIFF, and others

uses
  SysUtils, PicsLib;

var
  FileName, Mess: string;
  Width, Height: LongWord;

begin
  Writeln; Writeln('Image size determination using "picslib" unit.'); Writeln;
  repeat
    Mess := '';
    Write('Filename (ENTER to terminate)? '); Readln(FileName);
    if FileName <> '' then begin
      if FileExists(FileName) then begin
        if GetImageSize(FileName, Width, Height) then begin
          Writeln('Image width  = ', Width);
          Writeln('Image height = ', Height);
        end
        else
          Mess := 'Cannot determine image size!'
      end
      else
        Mess := 'Cannot find file specified!';
    end;
    if Mess <> '' then
      Writeln('Error: ', Mess);
    Writeln;
  until FileName = '';
end.
