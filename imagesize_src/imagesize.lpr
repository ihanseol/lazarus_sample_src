{ Determination of image size using LCL }

program imagesize;

// File formats supported: BMP, GIF, JPG, PNG, TIF, PCX

uses
  SysUtils, fpimage, fpreadbmp, fpreadgif, fpreadjpeg, fpreadpng, fpreadtiff, fpreadpcx;

var
  FileName, FileExt, Mess: string;
  Img: TFPCustomImage;
  Reader: TFPCustomImageReader;

begin
  Writeln; Writeln('Image size determination using LCL.'); Writeln;
  Img := TFPMemoryImage.Create(10, 10);
  repeat
    Mess := '';
    Write('Filename (ENTER to terminate)? '); Readln(FileName);
    if FileName <> '' then begin
      if FileExists(FileName) then begin
        FileExt := ExtractFileExt(FileName);
        if FileExt = '.bmp' then
          Reader := TFPReaderBMP.Create
        else if FileExt = '.gif' then
          Reader := TFPReaderGIF.Create
        else if (FileExt = '.jpg') or (FileExt = '.jpeg') then
          Reader := TFPReaderJPEG.Create
        else if FileExt = '.png' then
          Reader := TFPReaderPNG.Create
        else if (FileExt = '.tif') or (FileExt = '.tiff') then
          Reader := TFPReaderTIFF.Create
        else if FileExt = '.pcx' then
          Reader := TFPReaderPCX.Create
        else
          Mess := 'Unsupported file format!';
        if Mess = '' then begin
          Img.LoadFromFile(FileName, Reader);
          Writeln('Image width  = ', Img.Width);
          Writeln('Image height = ', Img.Height);
        end;
      end
      else
        Mess := 'File not found!';
    end;
    if Mess <> '' then
      Writeln('Error: ', Mess);
    Writeln;
  until FileName = '';
end.

