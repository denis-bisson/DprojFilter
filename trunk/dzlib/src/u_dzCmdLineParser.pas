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

{GXFormatter.config=twm}
{: Implements a simple commandline parser based on a state machine.
   To use, call one of the overloaded TCmdLineParser.Execute class methods.
   The state machine implementation is based on a concept described by
   Julian Bucknail in the Delphi Magazine, issue 115: "Object-Oriented State Machines"
   @author Thomas Mueller <http://www.dummzeuch.de> }
unit u_dzCmdLineParser;

{$I jedi.inc}

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator;

type
  EStateEngineError = class(exception);

const
  ALPHANUMERIC_CHARS = ['a'..'z', 'A'..'Z', '0'..'9'];
  ALLCHARS_BUT_NULL = [#1..#255];

type
  IEngineContext = interface ['{F6FB6D03-C90F-468D-9ACC-716C58697CCA}']
    {: returns the next character to parse }
    function GetNextChar: char;
    {: appends a character to the FOption field }
    procedure AddToOption(_c: char);
    {: appends a character to the FParameter field }
    procedure AddToParameter(_c: char);
    {: This is called whenever an option and parameter have been finished.
       It checks what kind of parameter it was, possibly combines them and
       adds it to the appropriate list. }
    procedure HandleCmdLinePart;
  end;

type
  IEngineState = interface ['{B8ADC607-8549-4B4A-A15A-278DAEE5F6CE}']
    function Execute(const _Context: IEngineContext): IEngineState;
    function GetClassName: string;
  end;

type
  TCmdLineParser = class
  public
    {: parses the CmdLine string and returns the options and paramteters
       @param CmdLine is a string with the commandline to parse
       @param Options is a TStrings instance which returns all options found as
              name=value pairs,
              e.g. '--one=two --three -x four -y' will result in
              'one=two'
              'three='
              'x=four'
              'y'
              This list must not be sorted. Duplicates are allowed. The order
              of the options is preserved. If you do not care about duplicates,
              you can use the Values property to access option values.
       @param Params is a TStrings instance  wich returns all parameters found,
              e.g. 'one two three' will result in
              'one'
              'two'
              'three' }
    class procedure Execute(const _CmdLine: string; _Options: TStrings; _Params: TStrings); overload;
    {: parses the application's commandline and returns the options and paramteters }
    class procedure Execute(_Options: TStrings; _Params: TStrings); overload;
    {: Splits the given commandline into the program name and the parameters }
    class procedure SplitCommandline(const _Commandline: string; out _Progname, _Parameters: string); overload;
    {: splits System.CmdLine into the program name and the parameters }
    class procedure SplitCommandline(out _Progname, _Parameters: string); overload;
  end;

implementation

uses
  StrUtils,
  u_dzStringUtils,
  u_dzCmdLineParserStates;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

type
  IEngineContextEx = interface ['{CD19DB13-F344-4E1A-B97F-D235B445B463}']
    procedure GetOptions(_Options: TStrings);
    procedure GetParams(_Params: TStrings);
  end;

type
  {: stores the engine context, passed to the execute method of all engine states }
  TEngineContext = class(TInterfacedObject, IEngineContext, IEngineContextEx)
  protected
    FReadIdx: integer;
    FInput: string;
    {: buffer for the currently handled option }
    FOption: string;
    {: buffer for the currently handled parameter }
    FParameter: string;
    {: Stores options as <optionname>[=<value] in the order they appeared on the
       commandline., note that options can appear multiple times, so using
       FParameter.Values[<name>] might not be appropriate }
    FOptions: TStringList;
    {: stores the Params property }
    FParams: TStringList;
  protected // implements IEngineContext
    {: returns the next character to parse }
    function GetNextChar: char;
    {: appends a character to the FOption field }
    procedure AddToOption(_c: char);
    {: appends a character to the FParameter field }
    procedure AddToParameter(_c: char);
    {: This is called whenever an option and parameter have been finished.
       It checks what kind of parameter it was, possibly combines them and
       adds it to the appropriate list. }
    procedure HandleCmdLinePart;
  protected // implements IEngineContextEx
    procedure GetOptions(_Options: TStrings);
    procedure GetParams(_Params: TStrings);
  public
    constructor Create(_Input: string);
    destructor Destroy; override;
    {: Stores options as <optionname>[=<value] in the order they appeared on the
       commandline., note that options can appear multiple times, so using
       FParameter.Values[<name>] might not be appropriate }
    property Options: TStringList read FOptions;
    {: Stores the parameters, ordered as they appear on the commandline }
    property Params: TStringList read FParams;
  end;

  { TStateParams }

procedure TEngineContext.AddToOption(_c: char);
begin
  FOption := FOption + _c;
end;

procedure TEngineContext.AddToParameter(_c: char);
begin
  FParameter := FParameter + _c;
end;

constructor TEngineContext.Create(_Input: string);
begin
  inherited Create;
  FOptions := TStringList.Create;
  FParams := TStringList.Create;
  FInput := _Input;
  FReadIdx := 0;
end;

destructor TEngineContext.Destroy;
begin
  FParams.Free;
  FOptions.Free;
  inherited;
end;

function TEngineContext.GetNextChar: char;
begin
  if FReadIdx >= Length(FInput) then
    Result := #0
  else
  begin
    Inc(FReadIdx);
    Result := FInput[FReadIdx];
  end;
end;

procedure TEngineContext.GetOptions(_Options: TStrings);
begin
  _Options.Assign(FOptions);
end;

procedure TEngineContext.GetParams(_Params: TStrings);
begin
  _Params.Assign(FParams);
end;

procedure TEngineContext.HandleCmdLinePart;
begin
  if FOption <> '' then
    FOptions.Add(FOption + '=' + FParameter)
  else
    FParams.Add(FParameter);
  FParameter := '';
  FOption := '';
end;

{ TCdmLineParser }

class procedure TCmdLineParser.Execute(const _CmdLine: string; _Options, _Params: TStrings);
var
  Context: IEngineContext;
  ContextEx: IEngineContextEx;
  State: IEngineState;
begin
  Context := TEngineContext.Create(_CmdLine);
  State := TEngineStateStart.Create;
  while State <> nil do
    State := State.Execute(Context);
  ContextEx := Context as IEngineContextEx;
  ContextEx.GetOptions(_Options);
  ContextEx.GetParams(_Params);
end;

class procedure TCmdLineParser.Execute(_Options, _Params: TStrings);
begin
  Execute(CmdLine, _Options, _Params);
end;

class procedure TCmdLineParser.SplitCommandline(out _Progname, _Parameters: string);
begin
  SplitCommandline(System.CmdLine, _Progname, _Parameters);
end;

class procedure TCmdLineParser.SplitCommandline(const _Commandline: string; out _Progname, _Parameters: string);
var
  i: integer;
  Len: integer;
  s: string;
  NeedsClosingQuote: boolean;
begin
  s := Trim(_Commandline);
  Len := Length(s);
  NeedsClosingQuote := False;
  for i := 1 to Len do
  begin
    case s[i] of
      ' ':
        begin
          if not NeedsClosingQuote then
          begin
            _ProgName := Trim(LeftStr(s, i));
            _Parameters := Trim(TailStr(s, i + 1));
            exit;
          end;
        end;
      '"':
        NeedsClosingQuote := not NeedsClosingQuote;
    end;
  end;
  _Progname := s;
  _Parameters := '';
end;

end.

