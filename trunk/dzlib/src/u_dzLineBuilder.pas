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

unit u_dzLineBuilder;

interface

uses
  SysUtils,
  Classes;

type
  ///<summary> Helper class for building a text line </summary>
  TLineBuilder = class
  private
    FListSeparator: string;
    FContent: string;
    FFormatSettings: TFormatSettings;
    FQuoteChar: Char;
    FColumnCount: Integer;
    FForceQuoted: Boolean;
  public
    ///<summary> Creates a TLineBuilder instance with the given separator
    ///          @param ListSeparator is the separator string to use, defaults to TAB (#9)
    ///          @param DecimalSeparator is the decimal separator to use for floating point
    ///                                  values, defaults to a dot (.). </summary>
    constructor Create(const _ListSeparator: string = #9; const _DecimalSeparator: Char = '.');
    ///<summary> Assigns the contents of another TLineBuilder instance </summary>
    procedure Assign(_Source: TLineBuilder);
    ///<summary> Adds a string column </summary>
    procedure Add(const _Column: string); overload;
    ///<summary> Adds a string column, putting it in quotes </summary>
    procedure AddQuoted(const _Column: string);
    ///<summary> Adds an integer value column </summary>
    procedure Add(_IntValue: Integer); overload;
    ///<summary> Adds a word value column </summary>
    procedure Add(_WordValue: Word); overload;
    ///<summary> Adds an integer value column </summary>
    procedure Add(_ShortIntValue: Shortint); overload;
    ///<summary> Adds a floating point value column</summary>
    procedure Add(_FloatValue: Extended); overload;
    ///<summary> Adds a floating point value column with the given number of decimals </summary>
    procedure Add(_FloatValue: Extended; _Decimals: Integer); overload;
    ///<summary> Adds a floating point value column with the given number of integer digits
    ///          and the given number of fractional digits </summary>
    procedure Add(_FloatValue: Extended; _IntDigits, _FracDigits: Integer); overload;
    ///<summary> Adds a column with a time in hh:mm:ss format </summary>
    procedure Add(_Hours, _Minutes, _Seconds: Integer); overload;
    ///<summary> Adds a column with a time in hh:mm:ss:tt format </summary>
    procedure Add(_Hours, _Minutes, _Seconds, _Hundredth: Integer); overload;
    ///<summary> Adds a boolean column, with 'Y' for true and 'N' for false </summary>
    procedure Add(_b: Boolean); overload;
    ///<summary> Adds a variant column, if it is a float, converts it to string using the configured DecimalSeparator </summary>
    procedure Add(_v: Variant); overload;
    ///<summary> Clears the line </summary>
    procedure Clear;
    ///<summary> Appends the contents of the given line </summary>
    procedure Append(_Line: TLineBuilder);
    ///<summary> Prepends the contents of the given line </summary>
    procedure Prepend(_Line: TLineBuilder);
    ///<summary> Extracts the first column from the line, returns false when empty </summary>
    function ExtractFirst(out _Column: string): Boolean;
    ///<summary> allows read access to the content that has been built </summary>
    property Content: string read FContent;
    property ColumnCount: Integer read FColumnCount;
    property DecimalSeparator: Char read FFormatSettings.DecimalSeparator write FFormatSettings.DecimalSeparator default '.';
    property ListSeparator: string read FListSeparator write FListSeparator;
    ///<summary> If set to true, every column will be enclosed in quotes </summary>
    property ForceQuoted: Boolean read FForceQuoted write FForceQuoted;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property FormatSettings: TFormatSettings read FFormatSettings;
  end;

implementation

uses
  u_dzStringUtils,
  StrUtils,
  u_dzVariantUtils,
  Variants;

{ TLineBuilder }

constructor TLineBuilder.Create(const _ListSeparator: string = #9; const _DecimalSeparator: Char = '.');
begin
  inherited Create;
  FListSeparator := _ListSeparator;
  FFormatSettings := GetUserDefaultLocaleSettings;
  FFormatSettings.DecimalSeparator := _DecimalSeparator;
  FFormatSettings.ThousandSeparator := #0;
  FQuoteChar := '"';
  FColumnCount := 0;
end;

procedure TLineBuilder.Add(_IntValue: Integer);
begin
  Add(IntToStr(_IntValue));
end;

procedure TLineBuilder.Add(_WordValue: Word);
begin
  Add(IntToStr(_WordValue));
end;

procedure TLineBuilder.Add(_ShortIntValue: Shortint);
begin
  Add(IntToStr(_ShortIntValue));
end;

procedure TLineBuilder.Add(_FloatValue: Extended; _Decimals: Integer);
begin
  Add(FloatToStrF(_FloatValue, fffixed, 18, _Decimals, FFormatSettings));
end;

procedure TLineBuilder.Add(_FloatValue: Extended);
begin
  Add(FloatToStr(_FloatValue, FFormatSettings));
end;

procedure TLineBuilder.Add(_FloatValue: Extended; _IntDigits, _FracDigits: Integer);
begin
  Add(Format('%*.*f', [_IntDigits, _FracDigits, _FloatValue], FFormatSettings));
end;

procedure TLineBuilder.Add(const _Column: string);
var
  s: string;
begin
  if FColumnCount > 0 then
    FContent := FContent + FListSeparator;
  if FForceQuoted then
    s := FQuoteChar + _Column + FQuoteChar
  else
    s := _Column;
  FContent := FContent + s;
  Inc(FColumnCount);
end;

function ZeroPadLeft(_Value: Integer; _Len: Integer): string;
var
  s: AnsiString;
begin
  Str(_Value, s);
  Result := string(s);
  while Length(Result) < _Len do
    Result := '0' + Result;
end;

procedure TLineBuilder.Add(_Hours, _Minutes, _Seconds: Integer);
begin
  Add(ZeroPadLeft(_Hours, 2) + ':' + ZeroPadLeft(_Minutes, 2) + ':' + ZeroPadLeft(_Seconds, 2));
end;

procedure TLineBuilder.Add(_Hours, _Minutes, _Seconds, _Hundredth: Integer);
begin
  Add(ZeroPadLeft(_Hours, 2) + ':' + ZeroPadLeft(_Minutes, 2) + ':' + ZeroPadLeft(_Seconds, 2)
    + ':' + ZeroPadLeft(_Hundredth, 2));
end;

procedure TLineBuilder.Add(_b: Boolean);
begin
  Add(IfThen(_b, 'Y', 'N'));
end;

procedure TLineBuilder.Add(_v: Variant);
begin
  if VarIsFloat(_v) then
    Add(Var2ExtEx(_v, 'TLineBuilder.Add'))
  else
    Add(Var2Str(_v, ''));
end;

procedure TLineBuilder.AddQuoted(const _Column: string);
begin
  Add(FQuoteChar + _Column + FQuoteChar);
end;

procedure TLineBuilder.Append(_Line: TLineBuilder);
var
  s: string;
begin
  s := _Line.Content;
  if FColumnCount > 0 then
    FContent := FContent + FListSeparator + s
  else
    FContent := s;
  FColumnCount := FColumnCount + _Line.ColumnCount;
end;

procedure TLineBuilder.Assign(_Source: TLineBuilder);
begin
  FContent := _Source.Content;
  FColumnCount := _Source.ColumnCount;
end;

procedure TLineBuilder.Clear;
begin
  FContent := '';
  FColumnCount := 0;
end;

function TLineBuilder.ExtractFirst(out _Column: string): Boolean;
var
  p: Integer;
begin
  p := Pos(FListSeparator, FContent);
  Result := p <> 0;
  if Result then begin
    _Column := LeftStr(FContent, p - 1);
    FContent := Copy(FContent, p + 1);
    Dec(FColumnCount);
  end;
end;

procedure TLineBuilder.Prepend(_Line: TLineBuilder);
var
  s: string;
begin
  s := _Line.Content;
  if FColumnCount > 0 then
    FContent := s + FListSeparator + FContent
  else
    FContent := s;
  FColumnCount := FColumnCount + _Line.ColumnCount;
end;

end.
