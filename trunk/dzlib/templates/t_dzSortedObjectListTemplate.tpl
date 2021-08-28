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

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
unit t_dzSortedObjectListTemplate;

interface

/// any class built on this template must add these units to the uses clause
uses
  Classes,
  u_dzQuicksort;

/// these types must be declared for any class built on this template
type
   /// This is the list's ancestor class, can be a user defined class if you
   /// want to inherit additional behaviour or TInterfacedObject if you
   /// want the list to implement an interface
  _LIST_PARENT_ = TObject;
  /// The type of items to be stored in the list, must be TObject or a descendant
  /// of TObject
  _ITEM_TYPE_ = TObject;

{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_SORTED_LIST_TEMPLATE__}
type
  _LIST_CONTAINER_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer;

{$INCLUDE 't_dzSortedListTemplate.tpl'}

type
  /// Extends _DZ_SORTED_LIST_TEMPLATE_ to call the item's Free method in FreeItem
  /// thereby allowing to store any TObject descendant.
  _DZ_SORTED_OBJECT_LIST_TEMPLATE_ = class(_DZ_SORTED_LIST_TEMPLATE_)
  private
    FOwnsObjects: Boolean;
  protected
    /// calls the Item's Free method if OwnsObject is true (which it is by default)
    procedure FreeItem(_Item: _ITEM_TYPE_); override;
  public
    constructor Create; 
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}

{$IFDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{ _DZ_SORTED_OBJECT_LIST_TEMPLATE_ }

{$INCLUDE 't_dzSortedListTemplate.tpl'}

constructor _DZ_SORTED_OBJECT_LIST_TEMPLATE_.Create;
begin
  inherited;
  FOwnsObjects := True;
end;

procedure _DZ_SORTED_OBJECT_LIST_TEMPLATE_.FreeItem(_Item: _ITEM_TYPE_);
begin
  if FOwnsObjects then
    _Item.Free;
end;

{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}

