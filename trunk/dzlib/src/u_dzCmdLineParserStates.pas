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

unit u_dzCmdLineParserStates;

interface

uses
  u_dzCmdLineParser,
  u_dzTranslator;

type
  TEngineStateAbstract = class(TInterfacedObject)
  private
    function GetClassName: string;
  end;

type
  TEngineStateError = class(TEngineStateAbstract, IEngineState)
  private
    FError: string;
    function Execute(const _Context: IEngineContext): IEngineState;
  public
    constructor Create(const _Error: string);
  end;

type
  TEngineStateStart = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateSpace = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateSpaceNoOptions = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateDash = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateDoubleDash = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateLongOption = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortOption = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortSwitch = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedShortParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateLongParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedLongParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateExe = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedExe = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

implementation

uses
  SysUtils,
  u_dzStringUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TEngineStateAbstract }

function TEngineStateAbstract.GetClassName: string;
begin
  Result := ClassName;
end;

{ TEngineStateError }

constructor TEngineStateError.Create(const _Error: string);
begin
  inherited Create;
  FError := _Error;
end;

function TEngineStateError.Execute(const _Context: IEngineContext): IEngineState;
begin
  raise EStateEngineError.Create(FError);
end;

{ TEngineStateStart }

function TEngineStateStart.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedExe.Create;
      end;
    ' ':
      Result := Self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateExe.Create;
  end;
end;

{ TEngineStateSpace }

function TEngineStateSpace.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '-':
      Result := TEngineStateDash.Create;
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    ' ':
      Result := Self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateParam.Create;
  end;
end;

{ TEngineStateSpaceNoOptions }

function TEngineStateSpaceNoOptions.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  // The difference to TEngineStateSpace is that TEngineStateSpaceNoOptions does not allow any
  // options. Everything must be a parameter.
  c := _Context.GetNextChar;
  case c of
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    ' ':
      Result := Self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateParam.Create;
  end;
end;

{ TEngineStateExe }

function TEngineStateExe.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedExe.Create;
      end;
    #0, ' ': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedExe }

function TEngineStateQuotedExe.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateExe.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateParam }

function TEngineStateParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    #0, ' ': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpaceNoOptions.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedParam }

function TEngineStateQuotedParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateDash }

function TEngineStateDash.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  if CharInSet(c, ALPHANUMERIC_CHARS + ['?']) then begin
    _Context.AddToOption(c);
    Result := TEngineStateShortOption.Create;
  end else if c = '-' then
    Result := TEngineStateDoubleDash.Create
  else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
end;

{ TEngineStateDoubleDash }

function TEngineStateDoubleDash.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  if CharInSet(c, ALPHANUMERIC_CHARS) then begin
    _Context.AddToOption(c);
    Result := TEngineStateLongOption.Create;
  end else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
end;

{ TEngineStateShortOption }

function TEngineStateShortOption.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ': begin
        Result := TEngineStateShortParam.Create;
      end;
    '-', '+': begin
        _Context.AddToParameter(c);
        Result := TEngineStateShortSwitch.Create;
      end;
    #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  end;
end;

{ TEngineStateShortSwitch }

function TEngineStateShortSwitch.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end else
    Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  end;
end;

{ TEngineStateShortParam }

function TEngineStateShortParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpaceNoOptions.Create;
      end;
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedShortParam.Create;
      end;
    '-': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateDash.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedShortParam }

function TEngineStateQuotedShortParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateShortParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateLongOption }

function TEngineStateLongOption.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '=':
      Result := TEngineStateLongParam.Create;
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
    '"', '''':
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToOption(c);
    Result := TEngineStateLongOption.Create;
  end;
end;

{ TEngineStateLongParam }

function TEngineStateLongParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedLongParam.Create;
      end;
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateLongParam.Create;
  end;
end;

{ TEngineStateQuotedLongParam }

function TEngineStateQuotedLongParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: Char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateLongParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format(_('Invalid character "%s".'), [c]));
  else
    _Context.AddToParameter(c);
    Result := TEngineStateQuotedLongParam.Create;
  end;
end;

end.

