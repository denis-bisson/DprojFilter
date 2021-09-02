//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* Unit to hcumulate the GroupOptions when parsing  job.                        *
//* All this will allow us to read the eventual options from --GroupOptions file.*
//* It allows to read them physically from their origin file JUST once.          *
//* If ever re-use for the same file of for subsequent files,  it will be fast   *
//* since it will be read from memory simple.                                    *
//* Finally, it will give us an easy way to detect circular references when a    *
//* --GroupOptions would attempt to use a --GroupOptions that we have not        *
//* finished to parse.
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

unit u_GroupOptions;

interface

uses
  System.Classes;

type
  TGroupOptions = class(TObject)
  private
    FOriginFilename: string;
    FOptionsLines: TStringList;
    FCurrentlyParsing: boolean;
  public
    property OriginFilename: string read FOriginFilename;
    property OptionsLines: TStringList read FOptionsLines;
    property CurrentlyParsing: boolean read FCurrentlyParsing write FCurrentlyParsing;
    constructor Create(const paramOriginFilename: string);
    destructor Destroy; override;
  end;

  TGroupOptionsList = class(TList)
  private
    function GetGroupOptions(Index: Integer): TGroupOptions;
  public
    property GroupOptions[Index: Integer]: TGroupOptions read GetGroupOptions;
    function FindByFilename(const paramFilename: string): integer;
    procedure Clear; override;
  end;

implementation

uses
  System.SysUtils;

{ TGroupOptions.Create }
constructor TGroupOptions.Create(const paramOriginFilename: string);
begin
  inherited Create;
  FOriginFilename := paramOriginFilename;
  FOptionsLines := TStringList.Create;
  if FileExists(paramOriginFilename) then FOptionsLines.LoadFromFile(FOriginFilename);
  FCurrentlyParsing := False;
end;

{ TGroupOptions.Destroy }
destructor TGroupOptions.Destroy;
begin
  FreeAndNil(FOptionsLines);
  inherited Destroy;
end;

{ TGroupOptionsList.GetGroupOptions }
function TGroupOptionsList.GetGroupOptions(Index: Integer): TGroupOptions;
begin
  result := TGroupOptions(Items[Index]);
end;

{ TGroupOptionsList.FindByFilename }
function TGroupOptionsList.FindByFilename(const paramFilename: string): integer;
var
  iSeekIndex: integer;
begin
  result := -1;
  iSeekIndex := 0;
  while (iSeekIndex < Count) and (result = -1) do
  begin
    if SameText(paramFilename, GroupOptions[iSeekIndex].OriginFilename) then
      result := iSeekIndex
    else
      inc(iSeekIndex);
  end;
end;

{ TGroupOptionsList.Clear }
procedure TGroupOptionsList.Clear;
var
  iIndex: integer;
begin
  for iIndex := pred(Count) downto 0 do
    Self.GroupOptions[iIndex].Destroy;
  inherited Clear;
end;

end.

