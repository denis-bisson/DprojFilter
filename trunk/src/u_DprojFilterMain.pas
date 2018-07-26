unit u_DprojFilterMain;

interface

uses
  SysUtils,
  Classes,
  u_dzDefaultMain;

type
  TDprojFilterMain = class(TDefaultMain)
  private
    procedure HandleParam(const _Parameter: string; const _DeleteLine: string;
      const _ChangeFrom, _ChangeTo: string);
    procedure HandleFile(const _fn: string; const _DeleteLine: string;
      const _ChangeFrom, _ChangeTo: string);
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

procedure TDprojFilterMain.HandleFile(const _fn: string; const _DeleteLine: string;
  const _ChangeFrom, _ChangeTo: string);
var
  Orig: TStringList;
  Changed: TStringList;
  i: Integer;
  Line: string;
  s: string;
  bakfn: string;
  ChangeCnt: Integer;
  p: Integer;
begin
  WriteLn('processing file ', _fn);
  InitializeNil(Orig, Changed);
  try
    Orig := TStringList.Create;
    Changed := TStringList.Create;
    try
      Orig.LoadFromFile(_fn);
      ChangeCnt := 0;
      for i := 0 to Orig.Count - 1 do begin
        Line := Orig[i];
        s := Trim(Line);
        if SameText(s, _DeleteLine) then begin
          Inc(ChangeCnt);
        end else if SameText(s, _ChangeFrom) then begin
          // retain indentation
          p := Pos(s, Line);
          Assert(p > 0);
          s := Copy(Line, 1, p - 1) + _ChangeTo;
          Changed.Add(s);
          Inc(ChangeCnt);
        end else begin
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
    FreeAndNil(Orig, Changed);
  end;
end;

procedure TDprojFilterMain.HandleParam(const _Parameter: string; const _DeleteLine: string;
  const _ChangeFrom, _ChangeTo: string);
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
      HandleFile(Files[FileIdx], _DeleteLine, _ChangeFrom, _ChangeTo);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

function TDprojFilterMain.doExecute: Integer;
var
  Parameters: TStringList;
  DeleteLine: string;
  ChangeFrom: string;
  ChangeTo: string;
  ParamIdx: Integer;
  OptionsOK: Boolean;
begin
  Parameters := TStringList.Create;
  try
    FGetOpt.ParamPassed('DprojFile', Parameters);

    OptionsOK := False;
    if FGetOpt.OptionPassed('DeleteLine', DeleteLine) then begin
      OptionsOK := True;
      DeleteLine := UnquoteString(DeleteLine);
    end;

    if FGetOpt.OptionPassed('ChangeFrom', ChangeFrom) then begin
      if FGetOpt.OptionPassed('ChangeTo', ChangeTo) then begin
        OptionsOK := True;
      end else
        raise Exception.Create('--ChangeFrom also requires --ChangeTo option');
    end;

    if not OptionsOK then
      raise Exception.Create('You must pass one of the options: --DeleteLine or --ChangeFrom');

    for ParamIdx := 0 to Parameters.Count - 1 do begin
      HandleParam(Parameters[ParamIdx], DeleteLine, ChangeFrom, ChangeTo);
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
  FGetOpt.RegisterOption('DeleteLine', 'Delete a line matching the parameter, wildcards are not allowed. E.g. --DeleteLine="<DCC_DcpOutput>..\..\lib\16</DCC_DcpOutput>"', True);
  FGetOpt.RegisterOption('ChangeFrom', 'Change a line matching the parameter to the parameter of --ChangeTo. E.g. --ChangeFrom="bla" --ChangeTo="blub"', True);
  FGetOpt.RegisterOption('ChangeTo', 'Gives the new content for the ChangeFrom parameter. E.g. --ChangeFrom="bla" --ChangeTo="blub"', True);
//  FGetOpt.RegisterOption('DeleteLineRegEx', 'Delete a line matching the parameter, regular expressions are allowed. E.g. --DeleteLine="<DCC_DcpOutput>..\..\lib\16</DCC_DcpOutput>"', 1)
end;

end.
