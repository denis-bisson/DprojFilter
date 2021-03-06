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

unit u_dzParamFoundList;

interface

uses
  Classes,
  u_dzParamDescList;

type
  TParamFound = class
  private
    FValue: string;
    FParamDesc: TParamDesc;
  public
    constructor Create(_Paramdesc: TParamDesc; const _Value: string);
    property ParamDesc: TParamDesc read FParamDesc;
    property Value: string read fValue;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TParamFound;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  ///<summary> List for storing TParamFound items </summary>
  TParamFoundList = class(_DZ_OBJECT_LIST_TEMPLATE_)

  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

constructor TParamFound.Create(_Paramdesc: TParamDesc; const _Value: string);
begin
  inherited Create;
  FParamDesc := _Paramdesc;
  FValue := _Value;
end;

end.

