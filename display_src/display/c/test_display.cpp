#include <iostream>
extern "C" {
  #include "display.h"
}

using namespace std;

int main(void) {
  int Black = 0; int Yellow = 14; int DarkGray = 8;
  char *Title = "Test DISPLAY unit.";
  char *EndOfProg = "Enter to terminate ";
  char *Name = "Aly";
  float Arr[10] = {
    1, 1.25, 1.50, 2, 2.25, 2.50, 3, 3.25, 3.50, 4
  };
  char Buffer[2];

  ClearScreen();
  SetColor(Yellow, Black);
  WritePos(Title, 1, 21);
  SetPos(2, 21);
  WriteChar('=', 18);
  ResetColor(); WriteEol(); WriteEol();
  WriteChar('-', 10); WriteEol();
  WriteString(Name, 'l', 0); WriteEol();
  WriteString(Name, 'r', 10); WriteEol();
  WriteString(Name, 'c', 10); WriteEol(); WriteEol();
  for (int I = 0; I < 10; I++) {
    WriteFloat(Arr[I], 0, 2);
    WriteChar(' ', 2);
  }
  WriteEol(); WriteEol();
  WriteColor(EndOfProg, DarkGray, Black); fgets(Buffer, 2, stdin);
  return EXIT_SUCCESS;
}
