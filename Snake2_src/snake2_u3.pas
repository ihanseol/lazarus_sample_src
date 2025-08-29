{**************************************************}
{* End of game unit (form) for Snake2 application *}
{**************************************************}

unit snake2_u3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  {************}
  { TfGameOver }
  {************}
  TfGameOver = class(TForm)
    imGameOver: TImage;
    stGameOver1, stGameOver2: TStaticText;
  end;

var
  fGameOver: TfGameOver;

implementation

{$R *.lfm}

end.

