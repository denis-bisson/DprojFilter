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

unit u_dzSortUtils;

interface

// To use any of the sorting algorithms, you need to supply either the two callbacks
// TCompareItemsMeth and TSwapItemsMeth or pass a class implementing IQSDataHandler
// ("QS" stands for "QuickSort" from the time when there was only one sorting algorithm)

type
  ///<summary>
  /// Compare items at the given indexes and return values similar to the standard Delphi
  /// Compare functions e.g. CompareStr or CompareText
  /// @returns 0 if they are equal
  ///          <0 if Item[Idx1] < Item[Idx2]
  ///          >0 if Item[Idx1] > Item[Idx2] </summary>
  TCompareItemsMeth = function(_Idx1, _Idx2: Integer): Integer of object;
  ///<summary>
  /// Swap the items at the given indexes </summary>
  TSwapItemsMeth = procedure(_Idx1, _Idx2: Integer) of object;

type
  ///<summary>
  /// Interface to be implemented for using the sorting algorithms </summary>
  ISortDataHandler = interface ['{C7B22837-F9C0-4228-A2E3-DC8BBF27DBA9}']
    /// Compare items at the given indexes and return values similar to the standard Delphi
    /// Compare functions e.g. CompareStr or CompareText
    /// @returns 0 if they are equal
    ///          <0 if Item[Idx1] < Item[Idx2]
    ///          >0 if Item[Idx1] > Item[Idx2] </summary>
    function Compare(_Idx1, _Idx2: Integer): Integer;
  ///<summary>
  /// Swap the items at the given indexes </summary>
    procedure Swap(_Idx1, _Idx2: Integer);
  end;

type
  IQSDataHandler = ISortDataHandler deprecated; // use ISortDataHandler instead

implementation

end.
