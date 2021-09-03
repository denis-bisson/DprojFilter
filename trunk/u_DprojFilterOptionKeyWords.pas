//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* Unit to hold the options used in DprojFilter.                                 *
//* We will try to refer their specific name from here for consistancy.          *
//* This will avoid speeling the command differently from a place to another.    *
//* Written by Denis Bisson, Drummondville, Québec, 2021-08-27.                  *
//* -----------------------------------------------------------------------------*
//* Used in the project DprojFilter                                              *
//* Originally and mainly written by Thomas Mueller                              *
//*   https://osdn.net/projects/dprojfilter                                      *
//* This little adaptation written by Denis Bisson, Drummondville, Québec, Canada*
//*   https://github.com/denis-bisson/DprojFilter                                *
//*   2021-08-27                                                                 *
//* -----------------------------------------------------------------------------*
//* You should not remove these comments.                                        *
//********************************************************************************
//

unit u_DprojFilterOptionKeyWords;

interface

uses
  System.Classes;

var
  slKeyWords: TStringList = nil;
  KWD_ChangeFrom: integer = -1;
  KWD_ChangeTo: integer = -1;
  KWD_DeleteLine: integer = -1;
  KWD_InsertAfter: integer = -1;
  KWD_Insert: integer = -1;
  KWD_InsertAfterAll: integer = -1;
  KWD_InsertAfterAllowDuplicates: integer = -1;
  KWD_GroupOptions: integer = -1;
  KWD_RegExReplaceFrom: integer = -1;
  KWD_RegExReplaceTo: integer = -1;
  KWD_NoBackup: integer = -1;
  KWD_Test: integer = -1;

implementation

initialization
  slKeyWords := TStringList.Create;
  slKeyWords.Add('ChangeFrom');
  KWD_ChangeFrom := 0;
  slKeyWords.Add('ChangeTo');
  KWD_ChangeTo := 1;
  slKeyWords.Add('DeleteLine');
  KWD_DeleteLine := 2;
  slKeyWords.Add('InsertAfter');
  KWD_InsertAfter := 3;
  slKeyWords.Add('Insert');
  KWD_Insert := 4;
  slKeyWords.Add('InsertAfterAll');
  KWD_InsertAfterAll := 5;
  slKeyWords.Add('InsertAfterAllowDuplicates');
  KWD_InsertAfterAllowDuplicates := 6;
  slKeyWords.Add('GroupOptions');
  KWD_GroupOptions := 7;
  slKeyWords.Add('RegExReplaceFrom');
  KWD_RegExReplaceFrom := 8;
  slKeyWords.Add('RegExReplaceTo');
  KWD_RegExReplaceTo := 9;
  slKeyWords.Add('NoBackup');
  KWD_NoBackup := 10;
  slKeyWords.Add('Test');
  KWD_Test := 11;

end.

