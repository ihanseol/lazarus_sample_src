program fclpdf5;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  fppdf, fpttf;

var
  Fonts: TFPFontCacheList;
  I: Integer;

procedure WriteFont(Family0, Name0, Filename0: string);

var
  I: Integer;
  Family, Name, Filename: string;

begin
  Family := Family0; Name := Name0; Filename := Filename0;
  for I := Length(Family0) to 35 do
    Family += ' ';
  for I := Length(Name0) to 40 do
    Name += ' ';
  Writeln(Family, Name, ExtractFilename(Filename));
end;

begin
  Fonts := TFPFontCacheList.Create;
  Fonts.SearchPath.Add('C:\Windows\Fonts');
  Fonts.BuildFontCache;
  for I := 0 to Fonts.Count - 1 do
    WriteFont(Fonts.Items[I].FamilyName, Fonts.Items[I].HumanFriendlyName, Fonts.Items[I].FileName);
  Fonts.Free;
end.

