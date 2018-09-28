unit u_DprojFilterMain;

{$I 'jedi.inc'}

interface

uses
  RegExpr,
  SysUtils,
  Classes,
  u_dzDefaultMain;

type
  TDprojFilterMain = class(TDefaultMain)
  private
    FDeleteLine: string;
    FChangeFrom: string;
    FChangeTo: string;
    FRegExReplaceFrom: string;
    FRegExReplaceTo: string;
    FInsertAfter: string;
    FInsert: string;
    FInsertAfterAll: Boolean;
    FINsertAfterAllowDuplicates: Boolean;
    procedure HandleParam(const _Parameter: string);
    procedure HandleFile(const _fn: string);
  protected
    procedure InitCmdLineParser; override;
    function doExecute: Integer; override;
  end;

implementation

uses
  u_dzFileUtils,
  u_dzStringUtils,
  u_dzMiscUtils;

{ TDprojFilterMain }

procedure TDprojFilterMain.HandleFile(const _fn: string);
var
  Orig: TStringList;
  Changed: TStringList;
  i: Integer;
  Line: string;
  s: string;
  bakfn: string;
  ChangeCnt: Integer;
  p: Integer;
  InsertAfterDone: Boolean;
  doInsert: Boolean;
  RegExpr: TRegExpr;
begin
  WriteLn('processing file ', _fn);
  InitializeNil(Orig, Changed, RegExpr);
  try
    Orig := TStringList.Create;
    Changed := TStringList.Create;
    RegExpr := TRegExpr.Create;
    try
      RegExpr.Expression := FRegExReplaceFrom;
      Orig.LoadFromFile(_fn);
{$IFDEF SUPPORTS_UNICODE}
      Changed.DefaultEncoding := Orig.Encoding;
{$ENDIF}
      ChangeCnt := 0;
      InsertAfterDone := False;
      for i := 0 to Orig.Count - 1 do begin
        Line := Orig[i];
        s := Trim(Line);
        if SameText(s, FDeleteLine) then begin
          Inc(ChangeCnt);
        end else if SameText(s, FChangeFrom) then begin
          // retain indentation
          p := Pos(s, Line);
          Assert(p > 0);
          s := Copy(Line, 1, p - 1) + FChangeTo;
          Changed.Add(s);
          Inc(ChangeCnt);
        end else if SameText(s, FInsertAfter) then begin
          Changed.Add(Line);
          if not InsertAfterDone then begin
            // retain indentation
            p := Pos(s, Line);
            Assert(p > 0);
            doInsert := True;
            if i < Orig.Count - 1 then begin
              s := Trim(Orig[i + 1]);
              if SameText(s, FInsert) then begin
                // Line to insert already exists
                if not FINsertAfterAllowDuplicates then begin
                  doInsert := False;
                end;
              end;
            end;
            if doInsert then begin
              s := Copy(Line, 1, p - 1) + FInsert;
              Changed.Add(s);
              Inc(ChangeCnt);
            end;
            if not FInsertAfterAll then
              InsertAfterDone := True;
          end;
        end else begin
          if FRegExReplaceFrom <> '' then
            if RegExpr.Exec(Line) then begin
              Line := RegExpr.Replace(Line, FRegExReplaceTo, True);
              Inc(ChangeCnt);
            end;
          Changed.Add(Line);
        end;
      end;
      if ChangeCnt > 0 then begin
        bakfn := _fn + '.bak';
        if TFileSystem.FileExists(bakfn) then
          WriteLn(bakfn, ' already exists, will not overwrite it.')
        else begin
          WriteLn('Writing backup to ', bakfn);
          Orig.SaveToFile(bakfn);
        end;
        Changed.SaveToFile(_fn);
      end;
      WriteLn(Format('Changed %d lines.', [ChangeCnt]));
    except
      on e: Exception do begin
        WriteLn(Format('Error processing "%s": %s (%s)', [_fn, e.Message, e.ClassName]));
      end;
    end;
  finally
    FreeAndNil(Orig, Changed, RegExpr);
  end;
end;

procedure TDprojFilterMain.HandleParam(const _Parameter: string);
var
  FileIdx: Integer;
  Files: TStringList;
begin
  WriteLn('Handling parameter ', _Parameter);
  Files := TStringList.Create;
  try
    TSimpleDirEnumerator.EnumFilesOnly(_Parameter, Files, True);
    WriteLn(Format('Found %d files matching %s', [Files.Count, _Parameter]));
    for FileIdx := 0 to TSimpleDirEnumerator.EnumFilesOnly(_Parameter, Files, True) - 1 do begin
      HandleFile(Files[FileIdx]);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

function SimpleDequoteString(const _s: string): string;
var
  Len: Integer;
begin
  Result := _s;
  Len := Length(Result);
  if Result <> '' then begin
    if (Result[1] = '"') and (Result[Len] = '"') then
      Result := Copy(Result, 2, Len - 2);
  end;
end;

function TDprojFilterMain.doExecute: Integer;
var
  Parameters: TStringList;
  ParamIdx: Integer;
  OptionsOK: Boolean;
begin
  Parameters := TStringList.Create;
  try
    FGetOpt.ParamPassed('DprojFile', Parameters);

    OptionsOK := False;
    if FGetOpt.OptionPassed('DeleteLine', FDeleteLine) then begin
      OptionsOK := True;
    end;

    if FGetOpt.OptionPassed('ChangeFrom', FChangeFrom) then begin
      if FGetOpt.OptionPassed('ChangeTo', FChangeTo) then begin
        OptionsOK := True;
      end else
        raise Exception.Create('--ChangeFrom also requires --ChangeTo option');
    end else begin
      if FGetOpt.OptionPassed('ChangeTo', FChangeTo) then
        raise Exception.Create('--ChangeTo is not allowed without a --ChangeFrom option');
    end;

    if FGetOpt.OptionPassed('RegExReplaceFrom', FRegExReplaceFrom) then begin
      if FGetOpt.OptionPassed('RegExReplaceTo', FRegExReplaceTo) then begin
        OptionsOK := True;
      end else
        raise Exception.Create('--RegExReplaceFrom also requires --RegExReplaceTo option');
    end else begin
      if FGetOpt.OptionPassed('RegExReplaceTo', FRegExReplaceTo) then
        raise Exception.Create('--RegExReplaceTo is not allowed without a --RegExReplaceFrom option');
    end;

    FINsertAfterAllowDuplicates := False;
    FInsertAfterAll := False;
    if FGetOpt.OptionPassed('InsertAfter', FInsertAfter) then begin
      if FGetOpt.OptionPassed('Insert', FInsert) then begin
        OptionsOK := True;
      end else
        raise Exception.Create('--InsertAfter also requires an --Insert option');
      FInsertAfterAll := FGetOpt.OptionPassed('InsertAfterAll');
      FINsertAfterAllowDuplicates := FGetOpt.OptionPassed('InsertAfterAllowDuplicates');
    end else begin
      if FGetOpt.OptionPassed('Insert', FInsert) then
        raise Exception.Create('--Insert is not allowed without an --InsertAfter option');
      if FGetOpt.OptionPassed('InsertAfterAll') then
        raise Exception.Create('--InsertAfterAll is not allowed without an --InsertAfter option');
      if FGetOpt.OptionPassed('InsertAfterAllowDuplicates') then
        raise Exception.Create('--InsertAfterAllowDuplicates is not allowed without an --InsertAfter option');
    end;

    if not OptionsOK then
      raise Exception.Create('You must pass one of the options: --DeleteLine, --ChangeFrom, --RegExReplaceFrom or --InsertAfter');

    FDeleteLine := SimpleDequoteString(FDeleteLine);
    FChangeFrom := SimpleDequoteString(FChangeFrom);
    FChangeTo := SimpleDequoteString(FChangeTo);
    FRegExReplaceFrom := SimpleDequoteString(FRegExReplaceFrom);
    FRegExReplaceTo := SimpleDequoteString(FRegExReplaceTo);
    FInsertAfter := SimpleDequoteString(FInsertAfter);
    FInsert := SimpleDequoteString(FInsert);

    for ParamIdx := 0 to Parameters.Count - 1 do begin
      HandleParam(Parameters[ParamIdx]);
    end;
  finally
    FreeAndNil(Parameters);
  end;
  Result := 0;
end;

procedure TDprojFilterMain.InitCmdLineParser;
begin
  inherited;
  FGetOpt.RegisterParam('DprojFile', 'Dproj file(s) to process wildcards are allowed', 1, MaxInt);
  FGetOpt.RegisterOption('DeleteLine', 'Delete a line matching the parameter. E.g. --DeleteLine="<DCC_DcpOutput>..\..\lib\16</DCC_DcpOutput>"', True);
  FGetOpt.RegisterOption('ChangeFrom', 'Change a line matching the parameter to the parameter of --ChangeTo. E.g. --ChangeFrom="bla" --ChangeTo="blub"', True);
  FGetOpt.RegisterOption('ChangeTo', 'Gives the new content for the ChangeFrom parameter. E.g. --ChangeFrom="bla" --ChangeTo="blub"', True);
  FGetOpt.RegisterOption('RegExReplaceFrom', 'Assuming a given regular expression, replace it if found inside a line with the parameter of --RegExReplaceTo.', True);
  FGetOpt.RegisterOption('RegExReplaceTo', 'Gives the new content for the RegExReplaceFrom parameter. E.g. --RegExReplaceFrom="Ver[0-9]" --RegExReplaceTo="Ver2"', True);
  FGetOpt.RegisterOption('InsertAfter', 'Insert a new line after the one matching the parameter. Requires an --Insert option.', True);
  FGetOpt.RegisterOption('Insert', 'Gives the line to insert for the --InsertAfter option.', True);
  FGetOpt.RegisterOption('InsertAfterAll', 'If given, InsertAfter applies to all matching lines, if not, only to the first. Requires an --Insert option.', False);
  FGetOpt.RegisterOption('InsertAfterAllowDuplicates', 'If given, InsertAfter will skip the check if the line to insert already exists. Requires an --Insert option.', False);
  FGetOpt.DequoteParams := False;
end;

end.
