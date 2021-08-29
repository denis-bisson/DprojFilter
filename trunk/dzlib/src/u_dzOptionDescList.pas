//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* Command line parser and other utilities from dzlib                           *
//* -----------------------------------------------------------------------------*
//* Used in the project DprojFilter                                              *
//* Originally and mainly written by Thomas Mueller                              *
//*   https://osdn.net/projects/dprojfilter                                      *
//* This little adaptation written by Denis Bisson, Drummondville, Québec, Canada*
//*   https://github.com/denis-bisson/DprojFilter                                *
//*   2021-08-27                                                                 *
//* -----------------------------------------------------------------------------*
//* See following notes for complement information.                              *
//* You should not remove these comments.                                        *
//********************************************************************************
//
//dummzeuch.de commandline parser
//copyright 2006-2011 by Thomas Mueller <http://www.dummzeuch.de>
//
//*****************************************************************************
//* Version: MPL 1.1
//*
//* The contents of this package are subject to the Mozilla Public License Version
//* 1.1 (the "License"); you may not use this file except in compliance with
//* the License. You may obtain a copy of the License at
//* http://www.mozilla.org/MPL/
//*
//* Software distributed under the License is distributed on an "AS IS" basis,
//* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
//* for the specific language governing rights and limitations under the
//* License.
//*
//* The Original package is dummzeuch.de commandline parser.
//*
//* The Initial Developer of the Original package is
//* Thomas Mueller <cmdlineparser@dummzeuch.de>.
//* Portions created by the Initial Developer are Copyright (C) 2006
//* the Initial Developer. All Rights Reserved.
//*
//* Contributor(s):
//* * Julian Bucknall ("Object-Oriented State Machines", The Delphi Magazine, issue 115)
//*****************************************************************************
//
//Documentation is supplied in the source code itself in a format suitable for
//PasDoc (http://pasdoc.sf.net), use ":" as marker.
//
//If you have any comments or want to contribute to dzCmdLineParser, feel free to contact
//me at the above address.
//
//Also I would like feedback if you use this library. So far I have received none
//at all which could mean everything between nobody even looked at it, or everybody
//who looked at it turned away in horror. ;-)
//
//Last updated 2011-10-16 by twm

unit u_dzOptionDescList;

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzQuicksort;

type
  EOptionDesc = class(Exception);
  EOptionName = class(EOptionDesc);

type
  TOptionDesc = class
  private
    FIsHidden: boolean;
    function CreateDescription(const _OptionName: string): string;
    procedure AssertValidOptionName(const _Name: ansistring);
  protected
    FPrimaryName: string;
    FDescription: string;
    FHasValue: boolean;
    FNames: TStringList;
  public
    constructor Create(const _Names: array of string; const _Description: string;
      _HasValue: boolean = false; _IsHidden: boolean = false);
    destructor Destroy; override;
    function GetDescription: string;
    property HasValue: boolean read FHasValue;
    property PrimaryName: string read FPrimaryName;
    property isHidden: boolean read FIsHidden write FIsHidden;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TOptionDesc;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  ///<summary> List for storing TOptionDesc items sorted by String </summary>
  TOptionDescList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    ///<summary> return the key of an item for comparison </summary>
    function KeyOf(const _Item: TOptionDesc): string; override;
    ///<summary> compare the keys of two items, must return a value
    ///          < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 </summary>
    function Compare(const _Key1, _Key2: string): integer; override;
  public
    function NonHiddenCount: integer;
  end;

implementation

uses
  StrUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TOptionDescList.KeyOf(const _Item: TOptionDesc): string;
begin
  Result := _Item.PrimaryName;
end;

function TOptionDescList.NonHiddenCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if not Items[i].isHidden then
      Inc(Result);
end;

function TOptionDescList.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

constructor TOptionDesc.Create(const _Names: array of string; const _Description: string;
  _HasValue: boolean = false; _IsHidden: boolean = false);
var
  i: integer;
  s: string;
begin
  inherited Create;
  FDescription := _Description;
  FHasValue := _HasValue;
  FIsHidden := _IsHidden;
  Assert(Length(_Names) > 0);
  FPrimaryName := _Names[0];
  AssertValidOptionName(AnsiString(FPrimaryName));
  FNames := TStringList.Create;
  for i := 0 to high(_Names) do begin
    s := _Names[i];
    AssertValidOptionName(AnsiString(s));
    fNames.Add(s);
  end;
end;

destructor TOptionDesc.Destroy;
begin
  FreeAndNil(FNames);
  inherited;
end;

procedure TOptionDesc.AssertValidOptionName(const _Name: ansistring);
var
  i: integer;
begin
  if _Name = '' then
    raise EOptionName.Create(_('Option name cannot be empty.'));
  { TODO -otwm : Maybe '$', '#' and some other special chars should be allowed }
  if not (_Name[1] in ['a'..'z', 'A'..'Z', '0'..'9', '?']) then
    raise EOptionName.Create(_('Option name must start with an alphanumeric character.'));
  for i := 2 to Length(_Name) do
    if not (_Name[i] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
      raise EOptionName.CreateFmt(_('Option name contains invalid character "%s" at position %d.'), [_Name[i], Ord(_Name[i])]);
end;

function TOptionDesc.CreateDescription(const _OptionName: string): string;
begin
  case Length(_OptionName) of
    0: Result := ''; // should never happen
    1: begin
        Result := '-' + _OptionName;
        if FHasValue then
          Result := Result + ' ' + _('value');
      end;
  else begin
      Result := '--' + _OptionName;
      if FHasValue then
        Result := Result + '=' + _('value');
    end
  end;
end;

function TOptionDesc.GetDescription: string;
var
  i: integer;
  iLenCurrent, iLonguest,iLastPosNewLine,iPosNewLine: integer;
  s,sLineHeader: string;

  procedure AddChunkToResult(sChunk:string);
  begin
    if sLineHeader='' then
    begin
       sLineHeader := StringOfChar(' ',(iLonguest+3));
       Result:=Result+sChunk;
    end
    else
    begin
      Result:=Result+#$0D#$0A+sLineHeader+sChunk;
    end;
  end;

begin
  Result := '';
  iLonguest:=0;
  iLenCurrent:=0;
  for i := 0 to fNames.Count - 1 do
  begin
    s := CreateDescription(FNames[i]);
    iLenCurrent :=length(s);
    if iLenCurrent>iLonguest then iLonguest:=iLenCurrent;
    if Result <> '' then Result := Result + #13#10;
    Result := Result + s;
  end;
  if iLonguest>length(s) then result:=result+StringOfChar(' ',(iLonguest-iLenCurrent));
  result:=result+' : ';

  sLineHeader := '';
  iLastPosNewLine:=1;
  repeat
    iPosNewLine := posex('\n',FDescription,iLastPosNewLine);
    if iPosNewLine=0 then
    begin
      AddChunkToResult(copy(FDescription, iLastPosNewLine));
    end
    else
    begin
      AddChunkToResult(copy(FDescription, iLastPosNewLine, iPosNewLine-iLastPosNewLine));
      iLastPosNewLine:=succ(succ(iPosNewLine));
     end;
  until iPosNewLine=0;
end;

end.

