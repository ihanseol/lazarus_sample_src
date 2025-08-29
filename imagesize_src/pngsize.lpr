{********************************************************}
{* Determination of PNG size by reading the binary file *}
{* Version 1.0, (c) allu, December 2024                 *}
{********************************************************}

program pngsize;

// The program supports UTF-8 file names.
// However, it is unable to handle file names including spaces!

uses
  SysUtils, LazUTF8;

type
  TChars = array of Char;

const
  // Signature at the beginning of PNG files
  PNGSignature: TChars = (
    #$89, #$50, #$4E, #$47, #$0D, #$0A, #$1A, #$0A
  );
  // PNG file IHDR chunk type indicator
  // Image width and height may be found in IHDR chunk
  IHDR: TChars = (
    'I', 'H', 'D', 'R'
  );

var
  I, P: Integer;
  Offset, W, H, M: Int64;
  FileName, Mess: string;
  OK: Boolean;
  Signature, ChunkType, Width, Height: TChars;
  PNGFile: file of Char;

{ Check if two character arrays are equal }

function EqualArrays(A1, A2: TChars): Boolean;

var
  I: Integer;
  AreEqual: Boolean;

begin
  AreEqual := True;
  if Length(A1) <> Length(A2) then
    AreEqual := False
  else begin
    for I := 0 to Length(A1) - 1 do begin
      if A1[I] <> A2[I] then
        AreEqual := False;
    end;
  end;
  Result := AreEqual;
end;

{ Main program }

begin
  Writeln; Writeln('PNG size determination.'); Writeln;
  // Repeat the whole until an empty file name is entered
  repeat
    Mess := '';
    Write('FileName (ENTER to terminate)? '); Readln(FileName);
    if FileName <> '' then begin
    // User has entered a file name
      OK := True; P := UTF8Pos(' ', FileName);
      if P <> 0 then begin
        // File names including spaces are not supported
        // This seems to be a limitation of FPC (?)
        OK := False;
        Mess := 'Invalid filename!'
      end
      else begin
        // Check if file exists (consider file name input with or without .png extension)
        OK := FileExists(FileName);
        if not OK then begin
          FileName += '.png';
          OK := FileExists(FileName);
        end;
        if OK then begin
          // File found: Try to determine image size
          Assign(PNGFile, Filename); Reset(PNGFile);
          if FileSize(PNGFile) < 25 then
            Mess := 'File is not a PNG file!'
          else begin
            // Read the 8 first bytes of the file
            SetLength(Signature, 8);
            for I := 0 to 7 do
              Read(PNGFile, Signature[I]);
            if EqualArrays(PNGSignature, Signature) then begin
              // Proceed if the 8 first bytes of the file are a PNG signature
              // Find the IHDR chunk type indicator
              SetLength(ChunkType, 4);
              Offset := 8;                                                     // Start searching from offset +8
              Seek(PNGFile, Offset);
              for I := 0 to 3 do                                               // Read 4 bytes
                Read(PNGFile, ChunkType[I]);
              while (not EqualArrays(ChunkType, IHDR)) and (not EoF(PNGFile)) do begin
                // As long as the 4 bytes read aren't equal to the IHDR chunk type indicator,
                // read another 4 bytes, using an offset of +1 relative to the previous read operation
                Inc(Offset);
                Seek(PNGFile, Offset);
                for I := 0 to 3 do
                  Read(PNGFile, ChunkType[I]);
              end;
              if EoF(PNGFile) then
                Mess := 'IHDR chunk not found!'
              else begin
                // IHDR chunk found: Read the 4 bytes width and 4 bytes height
                // The width is stored immediately after the chunk indicator,
                // the height is stored immediately after the width
                SetLength(Width, 4); SetLength(Height, 4);
                Offset += 4;
                Seek(PNGFile, Offset);
                for I := 0 to 3 do
                  Read(PNGFile, Width[I]);
                for I := 0 to 3 do
                  Read(PNGFile, Height[I]);
                // Calculate the width and height from the binary values
                W := 0; H := 0; M := 1;
                for I := 3 downto 0 do begin
                  W += M * Ord(Width[I]);
                  H += M * Ord(Height[I]);
                  M *= 256;
                end;
                // Display the PNG image width and height
                Writeln('PNG image width  = ', W);
                Writeln('PNG image height = ', H);
              end;
            end
            else begin
              // No PNG signature found
              Mess := 'File is not a PNG file!';
            end;
          end;
          Close(PNGFile);
        end
        else begin
          // File with name entered not found
          Mess := 'File not found!';
        end;
      end;
    end;
    // If something went wrong, display error message
    if Mess <> '' then
      Writeln('Error: ', Mess);
    Writeln;
  until FileName = '';
end.

