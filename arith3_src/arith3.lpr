//
// Arithmetic3: "Die Kinder zählen die Autos" (command line version)
//

program arith3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Crt,
  arithmetics3_common;

const
  Outputfile = 'Arithmetic3.txt';

var
  NQuestions, Result, Answer, NCorrect, NFalse, Q, I, J: Integer;
  PSuccess: Real;
  Question, Txt: string;
  Option: Char;
  Output: Text;

//
// Write text file (80 characters per line format)
//

procedure Write80F(var F: Text; S: string; LF: Boolean);

var
  I, L: Integer;
  W: string;

begin
  I := 0; L := 0;
  while I < Length(S) do begin
    W := '';
    // Get next word
    repeat
      Inc(I);
      W += S[I]; Inc(L);
    until (S[I] = ' ') or (I = Length(S));
    if L >= 80 then begin
      // If word doesn't fit on line, write a CR/LF
      Writeln(F);
      L := Length(W);
    end;
    // Write the word
    Write(F, W);
  end;
  // Write a CR/LF at end of text if the calling program tells so
  if LF then
    Writeln(F);
end;

//
// Display text (80 characters per line format)
//

procedure Write80(S: string; LF: Boolean);

var
  I, L: Integer;
  W: string;

begin
  I := 0; L := 0;
  while I < Length(S) do begin
    W := '';
    // Get next word
    repeat
      Inc(I);
      W += S[I]; Inc(L);
    until (S[I] = ' ') or (I = Length(S));
    if L >= 80 then begin
      // If word doesn't fit on line, display a CR/LF
      Writeln;
      L := Length(W);
    end;
    // Display the word
    Write(W);
  end;
  // Display a CR/LF at end of text if the calling program tells so
  if LF then
    Writeln;
end;

//
// Main program
//

begin
  Randomize;
  ClrScr;
  TextColor(Yellow);
  Writeln('Arithmetische Spielereien.');
  Writeln('==========================');
  TextColor(LightGray); Writeln;
  Writeln('Arithmetik Aufgaben, mit denen die Schueler folgendes erlernen koennen:');
  Writeln(' 1. Eine Aufgabe aufmerksam lesen und verstehen.');
  Writeln(' 2. Logisch denken und dies auf die gestellte Frage anwenden.');
  Writeln(' 3. Die aritmethischen Grundrechenarten (mit Zahlen bis 100).');
  Writeln(' 4. Bruchrechnen (Halbe, Drittel, Viertel, Fuenftel).');
  Writeln;
  Write('B = Beispielfragen anzeigen, A = Test alle Fragen, Z = Test Zufallsfragen ? ');
  Readln(Option);
  // Create text file with question examples
  if Option in ['B', 'b'] then begin
    Assign(Output, Outputfile); Rewrite(Output);
    // Generate a question for each question type
    for I := 1 to NQuestionTypes do begin
      Writeln(Output, 'Frage ', I:2, ':');
      Writeln(Output, '---------');
      Result := 0; Txt := '';
      Question := 'Die Kinder zaehlen die Autos ' + RandomPlace + '.';
      QuestionText(I, Txt, Result);
      Question += ' ' + Txt + ' *' + IntToStr(Result) + '*';
      Write80F(Output, Question, True);
      Writeln(Output);
    end;
    Close(Output);
    Writeln; Writeln('Beispielsfragen Datei = ', Outputfile);
  end
  // Test (Generate questions to be answered by pupil)
  else if Option in ['A', 'a', 'Z', 'z'] then begin
    Writeln;
    if Option in ['Z', 'z'] then begin
      Write('Anzahl der Fragen ? '); Readln(NQuestions);                       // number of questions to be asked chosen by used
    end
    else
      NQuestions := NQuestionTypes;                                            // number of questions to be asked = number of question types
    NCorrect := 0; NFalse := 0; PSuccess := 0;
    // Generate the questions and check user answer
    for I := 1 to NQuestions do begin
      if Option in ['A', 'a'] then
        Q := I                                                                 // question = next question type
      else
        Q := Random(NQuestionTypes) + 1;                                       // question = randomly chosen (not keeping track of question types done)
      Txt := 'Frage ' + IntToStr(I) + ':';
      // Clear previous question text on screen
      for J := 0 to 9 do begin
        GotoXY(1, 12 + J);
        ClrEoL;
      end;
      // Display question number
      GotoXY(1, 12);
      Writeln(Txt);
      for J := 1 to Length(Txt) do
        Write('-');
      Writeln;
      // Generate the question
      Txt := ''; Result := 0; Answer := 0;
      Question := 'Die Kinder zaehlen die Autos ' + RandomPlace + '.';
      QuestionText(Q, Txt, Result);                                            // call question-generating-routine in arithmetics3_common
      Question += ' ' + Txt;
      // Display the question (80 chars per line formatted)
      Write80(Question, False);
      // Read user answer
      Write(' '); Readln(Answer); Writeln;
      // Correct answer
      if Answer = Result then begin
        TextColor(LightGreen); Writeln('Richtig!');
        Inc(NCorrect);
      end
      // False answer
      else begin
        TextColor(LightRed); Writeln('Falsch! Die richtige Antwort = ', Result);
        Inc(NFalse);
      end;
      // Continue with next question after user has pressed ENTER key
      TextColor(LightGray); Writeln;
      Write('Bitte, ENTER Taste druecken...'); Readln;
    end;
    // Clear last question text on screen
    for J := 0 to 9 do begin
      GotoXY(1, 12 + J);
      ClrEoL;
    end;
    // Display success (correct/false answers and percentage)
    GotoXY(1, 12);
    PSuccess := 100 * (NCorrect / NQuestions);
    Writeln('Anzahl der Fragen: ', NQuestions:3);
    Writeln('Richtige Antworten:', NCorrect:3);
    Writeln('Falsche Antworten: ', NFalse:3);
    Write('Erfolgsbewertung:  ');
    if PSuccess < 50 then
      TextColor(LightRed)
    else if PSuccess < 60 then
      TextColor(Yellow)
    else
      TextColor(LightGreen);
    Writeln(PSuccess:6:2, '%');
    TextColor(LightGray);
  end;
  Writeln; Writeln;
  Write('ENTER druecken um das Programm zu beenden...'); Readln;
end.

