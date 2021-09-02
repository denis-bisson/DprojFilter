//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* Unit to cumulate the file list when parsing  job.                            *
//* All this will allow us to read the eventual file list from @ListOffiles.     *
//* It allows to read them physically from their origin file JUST once.          *
//* It might appear stupid to re-read a file list we have already used, but let's*
//* not block that if there is no circular reference.                            *
//* Anyway, we will parse the file just once anyway.
//* If ever re-use for the same file of for subsequent files,  it will be fast   *
//* since it will be read from memory simple.                                    *
//* Finally, it will give us an easy way to detect circular references when a    *
//* @ListFilename would attempt to use a ListFilename that we have not           *
//* finished to parse.                                                           *
//* Written by Denis Bisson, Drummondville, Québec, 2021-09-02.                  *
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

unit u_FileList;

interface

uses
  System.Classes;

type
  TFilesList = class(TObject)
  private
    FOriginFilename: string;
    FListOffiles: TStringList;
    FCurrentlyParsing: boolean;
  public
    property OriginFilename: string read FOriginFilename;
    property ListOffiles: TStringList read FListOfFiles;
    property CurrentlyParsing: boolean read FCurrentlyParsing write FCurrentlyParsing;
    constructor Create(const paramOriginFilename: string);
    destructor Destroy; override;
  end;

  TFilesListList = class(TList)
  private
    function GetFilesList(Index: Integer): TFilesList;
  public
    property FilesList[Index: Integer]: TFilesList read GetFilesList;
    function FindByFilename(const paramFilename: string): integer;
    procedure Clear; override;
  end;

implementation

uses
  System.SysUtils;

{ TFilesList.Create }
constructor TFilesList.Create(const paramOriginFilename: string);
begin
  inherited Create;
  FOriginFilename := paramOriginFilename;
  FListOffiles := TStringList.Create;
  if FileExists(paramOriginFilename) then FListOfFiles.LoadFromFile(FOriginFilename);
  FCurrentlyParsing := False;
end;

{ TFilesList.Destroy }
destructor TFilesList.Destroy;
begin
  FreeAndNil(FListOffiles);
  inherited Destroy;
end;

{ TFilesListList.GetFilesList }
function TFilesListList.GetFilesList(Index: Integer): TFilesList;
begin
  result := TFilesList(Items[Index]);
end;

{ TFilesListList.FindByFilename }
function TFilesListList.FindByFilename(const paramFilename: string): integer;
var
  iSeekIndex: integer;
begin
  result := -1;
  iSeekIndex := 0;
  while (iSeekIndex < Count) and (result = -1) do
  begin
    if SameText(paramFilename, FilesList[iSeekIndex].OriginFilename) then
      result := iSeekIndex
    else
      inc(iSeekIndex);
  end;
end;

{ TFilesListList.Clear }
procedure TFilesListList.Clear;
var
  iIndex: integer;
begin
  for iIndex := pred(Count) downto 0 do
    Self.FilesList[iIndex].Destroy;
  inherited Clear;
end;

end.

