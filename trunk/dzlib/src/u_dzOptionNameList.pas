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

unit u_dzOptionNameList;

interface

uses
  SysUtils,
  Classes,
  u_dzQuickSort,
  u_dzTranslator,
  u_dzOptionDescList;

type
  TOptionName = class
  protected
    FName: string;
    FOptionDesc: TOptionDesc;
  public
    constructor Create(const _Name: string; _OptionDesc: TOptionDesc);
    function GetPrimaryName: string;
    property Name: string read FName;
    property OptionDesc: TOptionDesc read FOptionDesc;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TOptionName;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  ///<summary> List for storing TOptionName items sorted by string </summary>
  TOptionNameList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    ///<summary> return the key of an item for comparison </summary>
    function KeyOf(const _Item: TOptionName): string; override;
    ///<summary> compare the keys of two items, must return a value
    ///          < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 </summary>
    function Compare(const _Key1, _Key2: string): Integer; override;
  end;

implementation

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TOptionNameList.KeyOf(const _Item: TOptionName): string;
begin
  Result := _Item.Name;
end;

function TOptionNameList.Compare(const _Key1, _Key2: string): Integer;
begin
  // Note: This compares 1 character options case sensitively ( eg. -A <> -a )
  //       but long options case insensitively (e.g. --hallo = --Hallo )
  Result := CompareText(_Key1, _Key2);
  if (Result = 0) and (Length(_Key1) = 1) then
    Result := CompareStr(_Key1, _Key2);
end;

constructor TOptionName.Create(const _Name: string; _OptionDesc: TOptionDesc);
begin
  inherited Create;
  FName := _Name;
  FOptionDesc := _OptionDesc;
end;

function TOptionName.GetPrimaryName: string;
begin
  Result := FOptionDesc.PrimaryName;
end;

end.

