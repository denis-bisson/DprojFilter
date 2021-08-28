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

{: This unit implements a Quicksort procedure that can
   be used to sort anything as well as a binary sarch
   function.
   @author(Thomas Mueller http://www.dummzeuch.de)
}

unit u_dzQuicksort;

interface

uses
  u_dzSortUtils;

type
  // for binary search
  TCompareToItemMeth1 = function(const _Key; _Idx: Integer): Integer of object;
  TCompareToItemMeth2 = function(_Key: pointer; _Idx: Integer): Integer of object;

///<summary>
/// Call Quicksort with two method pointers for
///   comparing and swapping two elements.
///   @longcode(##
///     Quicksort(0, Count-1, self.CompareItems, self.SwapItems);
///   ##) }
procedure QuickSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth); overload;

///<summary>
/// Call Quicksort with an interface that does the comparison and swapping
///   comparing and swapping two elements.
///   @longcode(##
///     Quicksort(0, Count-1, CompareAndSwapInterface);
///   ##) }
procedure QuickSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

///<summary>
/// Call BinarySearch with a method pointer that
///   compares an index to the Item sought.
///   @param Index contains the index where the item is supposed to be
///                (Its index, if it was found or the index where it would be inserted if not)
///   @param Duplicates determines whether duplicates are allowed in the list or not
///   @returns true, if the item was found, false otherwise
///   @longcode(##
///     Found := BinarySearch(0, count-1, Idx, Key, Self.CompareToKey);
///   ##) }
function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  const _Key; _CompareMeth: TCompareToItemMeth1;
  _Duplicates: Boolean = False): Boolean; overload;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth2;
  _Duplicates: Boolean = False): Boolean; overload;

type
  ICompareToKey = interface ['{CEB61050-D71F-4F67-B9BC-FD496A079F75}']
    function CompareTo(_Idx: Integer): Integer;
  end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _CompareInt: ICompareToKey; _Duplicates: Boolean = False): Boolean; overload;

implementation

procedure QuickSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    exit;
  repeat
    I := _Left;
    J := _Right;
    P := (_Left + _Right) shr 1;
    repeat
      while _DataHandler.Compare(I, P) < 0 do
        Inc(I);
      while _DataHandler.Compare(J, P) > 0 do
        Dec(J);
      if I <= J then begin
        if I < J then
          _DataHandler.Swap(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _DataHandler);
    _Left := I;
  until I >= _Right;
end;

procedure QuickSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth);
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    exit;
  repeat
    I := _Left;
    J := _Right;
    P := (_Left + _Right) shr 1;
    repeat
      while _CompareMeth(I, P) < 0 do
        Inc(I);
      while _CompareMeth(J, P) > 0 do
        Dec(J);
      if I <= J then begin
        if I < J then
          _SwapMeth(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _CompareMeth, _SwapMeth);
    _Left := I;
  until I >= _Right;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  const _Key; _CompareMeth: TCompareToItemMeth1;
  _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareMeth(_Key, p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth2;
  _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareMeth(_Key, p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _CompareInt: ICompareToKey; _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareInt.CompareTo(p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

end.

