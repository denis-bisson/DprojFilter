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

unit u_dzNullableTypesUtils;

interface

uses
  SysUtils;

type
  EInvalidValue = class(Exception);

type
  PFormatSettings = ^TFormatSettings;

///<summary>
/// This returns the user's default format settings, which are initialized by a call to
/// GetUserDefaultLocaleSettings on startup. </summary>
function UserLocaleFormatSettings: PFormatSettings;

procedure StrToNumber(const _s: string; out _Value: Integer); overload;
procedure StrToNumber(const _s: string; out _Value: Single); overload;
procedure StrToNumber(const _s: string; out _Value: Double); overload;
{$IFNDEF win64}
// Extended = Double in Win64
procedure StrToNumber(const _s: string; out _Value: Extended); overload;
{$ENDIF}

function TryStrToNumber(const _s: string; out _Value: Integer; const _FormatSettings: TFormatSettings): Boolean; overload;
function TryStrToNumber(const _s: string; out _Value: Int64; const _FormatSettings: TFormatSettings): Boolean; overload;
function TryStrToNumber(const _s: string; out _Value: Single; const _FormatSettings: TFormatSettings): Boolean; overload;
function TryStrToNumber(const _s: string; out _Value: Double; const _FormatSettings: TFormatSettings): Boolean; overload;
{$IFNDEF win64}
function TryStrToNumber(const _s: string; out _Value: Extended; const _FormatSettings: TFormatSettings): Boolean; overload;
{$ENDIF}

function NumberToStr(_Value: Integer): string; overload;
function NumberToStr(_Value: Single): string; overload;
function NumberToStr(_Value: Double): string; overload;
{$IFNDEF win64}
function NumberToStr(_Value: Extended): string; overload;
{$ENDIF}

function TryVar2Number(const _v: Variant; out _Value: Integer): Boolean; overload;
function TryVar2Number(const _v: Variant; out _Value: Int64): Boolean; overload;
function TryVar2Number(const _v: Variant; out _Value: Single): Boolean; overload;
function TryVar2Number(const _v: Variant; out _Value: Double): Boolean; overload;
{$IFNDEF win64}
function TryVar2Number(const _v: Variant; out _Value: Extended): Boolean; overload;
{$ENDIF}

function GetNullableTypesFlagInterface: IInterface;

procedure DivideNumbers(_a, _b: Integer; out _Value: Integer); overload;
procedure DivideNumbers(_a, _b: Int64; out _Value: Int64); overload;
procedure DivideNumbers(_a, _b: Single; out _Value: Single); overload;
procedure DivideNumbers(_a, _b: Double; out _Value: Double); overload;
{$IFNDEF win64}
procedure DivideNumbers(_a, _b: Extended; out _Value: Extended); overload;
{$ENDIF}

implementation

uses
  u_dzVariantUtils,
  u_dzStringUtils;

// this is a fake interfaced object that only exists as the VMT
// It can still be used to trick the compiler into believing an interface pointer is assigned

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

const
  FlagInterfaceVTable: array[0..2] of Pointer =
    (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease
    );
const
  FlagInterfaceInstance: Pointer = @FlagInterfaceVTable;

function GetNullableTypesFlagInterface: IInterface;
begin
  Result := IInterface(@FlagInterfaceInstance);
end;

var
  gblUserLocaleFormatSettings: TFormatSettings;

function UserLocaleFormatSettings: PFormatSettings;
begin
  Result := @gblUserLocaleFormatSettings;
end;

// StrToNumber

procedure StrToNumber(const _s: string; out _Value: Integer);
begin
  _Value := StrToInt(_s);
end;

procedure StrToNumber(const _s: string; out _Value: Single);
begin
  _Value := StrToFloat(_s);
end;

procedure StrToNumber(const _s: string; out _Value: Double);
begin
  _Value := StrToFloat(_s);
end;

{$IFNDEF Win64}
procedure StrToNumber(const _s: string; out _Value: Extended);
begin
  _Value := StrToFloat(_s);
end;
{$ENDIF}

// TryStrToNumber

function TryStrToNumber(const _s: string; out _Value: Integer; const _FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToInt(_s, _Value);
end;

function TryStrToNumber(const _s: string; out _Value: Int64; const _FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToInt64(_s, _Value);
end;

function TryStrToNumber(const _s: string; out _Value: Single; const _FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(_s, _Value, _FormatSettings);
end;

function TryStrToNumber(const _s: string; out _Value: Double; const _FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(_s, _Value, _FormatSettings);
end;

{$IFNDEF Win64}
function TryStrToNumber(const _s: string; out _Value: Extended; const _FormatSettings: TFormatSettings): Boolean;
begin
  Result := TryStrToFloat(_s, _Value, _FormatSettings);
end;
{$ENDIF}

// NumberToStr

function NumberToStr(_Value: Integer): string;
begin
  Result := IntToStr(_Value);
end;

function NumberToStr(_Value: Single): string;
begin
  Result := FloatToStr(_Value);
end;

function NumberToStr(_Value: Double): string;
begin
  Result := FloatToStr(_Value);
end;

{$IFNDEF Win64}
function NumberToStr(_Value: Extended): string;
begin
  Result := FloatToStr(_Value);
end;
{$ENDIF}

// TryVar2Number

function TryVar2Number(const _v: Variant; out _Value: Integer): Boolean;
begin
  Result := TryVar2Int(_v, _Value);
end;

function TryVar2Number(const _v: Variant; out _Value: Int64): Boolean;
begin
  Result := TryVar2Int64(_v, _Value);
end;

function TryVar2Number(const _v: Variant; out _Value: Single): Boolean;
begin
  Result := TryVar2Single(_v, _Value);
end;

function TryVar2Number(const _v: Variant; out _Value: Double): Boolean;
begin
  Result := TryVar2Dbl(_v, _Value);
end;

{$IFNDEF Win64}
function TryVar2Number(const _v: Variant; out _Value: Extended): Boolean;
begin
  Result := TryVar2Ext(_v, _Value);
end;
{$ENDIF}

procedure DivideNumbers(_a, _b: Integer; out _Value: Integer);
begin
  _Value := _a div _b;
end;

procedure DivideNumbers(_a, _b: Int64; out _Value: Int64);
begin
  _Value := _a div _b;
end;

procedure DivideNumbers(_a, _b: Single; out _Value: Single);
begin
  _Value := _a / _b;
end;

procedure DivideNumbers(_a, _b: Double; out _Value: Double);
begin
  _Value := _a / _b;
end;

{$IFNDEF Win64}
procedure DivideNumbers(_a, _b: Extended; out _Value: Extended);
begin
  _Value := _a / _b;
end;
{$ENDIF}

initialization
  gblUserLocaleFormatSettings := GetUserDefaultLocaleSettings;
end.

