unit u_DprojEditMain;

interface

uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzDefaultMain;

type
  TDprojEditMain = class(TDefaultMain)
  private
    FDproj: TStringList;
    procedure SetDccNamespace(const _DccNamespace: string);
    function Find(const _Line: string; out _Idx: Integer): Boolean;
    function SameLine(const _Line1, _Line2: string): Boolean;
    function StartsLine(const _SubText, _Line2: string): Boolean;
    function EndsLine(const _SubText, _Line2: string): Boolean;
    procedure SetLine(_Idx: Integer; const _Line: string);
  protected
    procedure InitCmdLineParser; override;
    function doExecute: Integer; override;
  end;

implementation

uses
  StrUtils,
  u_dzStringUtils,
  u_dzFileUtils;

{ TDprojEditMain }

function TDprojEditMain.StartsLine(const _SubText, _Line2: string): Boolean;
begin
  Result := StartsText(_SubText, Trim(_Line2));
end;

function TDprojEditMain.EndsLine(const _SubText, _Line2: string): Boolean;
begin
  Result := EndsText(_SubText, Trim(_Line2));
end;

function TDprojEditMain.SameLine(const _Line1, _Line2: string): Boolean;
begin
  Result := SameText(Trim(_Line1), Trim(_Line2));
end;

function TDprojEditMain.Find(const _Line: string; out _Idx: Integer): Boolean;
var
  i: Integer;
  Line: string;
begin
  for i := 0 to FDproj.Count - 1 do begin
    Line := FDproj[i];
    if SameLine(Line, _Line) then begin
      _Idx := i;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function IsWhitespace(_c: Char): Boolean;
begin
  Result := (_c in [' ', #9]);
end;

procedure TDprojEditMain.SetLine(_Idx: Integer; const _Line: string);
var
  Line: string;
  i: Integer;
begin
  Line := FDproj[_Idx];
  i := 1;
  while (i <= Length(Line)) and IsWhitespace(Line[i]) do
    Inc(i);
  Line := Copy(Line, 1, i - 1);
  FDproj[_Idx] := Line + _Line;
end;

procedure TDprojEditMain.SetDccNamespace(const _DccNamespace: string);
var
  idx: Integer;
  Line: string;
  StartIdx: Integer;
begin
  if not Find('<PropertyGroup Condition="''$(Base)''!=''''">', StartIdx) then
    raise Exception.Create(_('Could not find base configuration.'));

  Inc(StartIdx);
  idx := StartIdx;
  Line := FDproj[idx];
  while (idx < FDproj.Count) and not SameLine('</PropertyGroup>', Line) do begin
    if StartsLine('<DCC_Namespace>', Line) then begin
      if EndsLine('</DCC_Namespace>', Line) then begin
        SetLine(idx, '<DCC_Namespace>' + _DccNamespace + '</DCC_Namespace>');
        Exit;
      end;
    end;
    Inc(idx);
  end;
  FDproj.Insert(StartIdx, #9#9#9'<DCC_Namespace>' + _DccNamespace + '</DCC_Namespace>');
end;

function TDprojEditMain.doExecute: Integer;
var
  DccNamespace: string;
  DprojFile: string;
  OutputFile: string;
  OutDir: string;
begin
  WriteLn(FGetOpt.ProgName);
  FDproj := TStringList.Create;
  try
    FGetOpt.ParamPassed('DprojFile', DprojFile);
    WriteLn('Reading "', DprojFile, '"');
    FDproj.LoadFromFile(DprojFile);

    if FGetOpt.OptionPassed('SetDccNamespace', DccNamespace) then begin
      WriteLn('Setting DCC_Namespace to: "', DccNamespace, '"');
      SetDccNamespace(DccNamespace);
    end;

    if not FGetOpt.OptionPassed('OutputFile', OutputFile) then
      OutputFile := DprojFile;
    OutDir := etpd(ExtractFileDir(OutputFile));
    if OutDir = '' then begin
      OutDir := ExtractFileDir(DprojFile);
      if OutDir <> '' then
        OutputFile := itpd(OutDir) + OutputFile;
    end;
    WriteLn('Writing to "', OutputFile, '"');
    FDproj.SaveToFile(OutputFile);
  finally
    FreeAndNil(FDproj);
  end;
  Result := 0;
end;

procedure TDprojEditMain.InitCmdLineParser;
begin
  inherited;
  FGetOpt.RegisterParam('DprojFile', _('.DPROJ file to edit'));
  FGetOpt.RegisterOption('SetDccNamespace', _('set the DCC_Namespace option'), True);
  FGetOpt.RegisterOption('OutputFile', _('Instead of modifying the .DPROJ file, write to OutputFile'), True);
end;

end.

