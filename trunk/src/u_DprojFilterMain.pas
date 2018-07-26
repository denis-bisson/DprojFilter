unit u_DprojFilterMain;

interface

uses
  SysUtils,
  Classes,
  u_dzDefaultMain;

type
  TDprojFilterMain = class(TDefaultMain)
  private
    procedure HandleParam(const _Parameter, _DeleteLine: string);
    procedure HandleFile(const _fn, _DeleteLine: string);
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

procedure TDprojFilterMain.HandleFile(const _fn: string; const _DeleteLine: string);
var
  Orig: TStringList;
  Changed: TStringList;
  i: Integer;
  s: string;
  bakfn: string;
  DelCnt: Integer;
begin
  WriteLn('processing file ', _fn);
  InitializeNil(Orig, Changed);
  try
    Orig := TStringList.Create;
    Changed := TStringList.Create;
    try
      Orig.LoadFromFile(_fn);
      DelCnt := 0;
      for i := 0 to Orig.Count - 1 do begin
        s := Trim(Orig[i]);
        if SameText(s, _DeleteLine) then begin
          Inc(DelCnt);
        end else begin
          Changed.Add(Orig[i]);
        end;
      end;
      if DelCnt > 0 then begin
        bakfn := _fn + '.bak';
        if TFileSystem.FileExists(bakfn) then
          WriteLn(bakfn, ' already exists, will not overwrite it.')
        else begin
          WriteLn('Writing backup to ', bakfn);
          Orig.SaveToFile(bakfn);
        end;
      end;
      Changed.SaveToFile(_fn);
      WriteLn(Format('Deleted %d lines.', [DelCnt]));
    except
      on e: Exception do begin
        WriteLn(Format('Error processing "%s": %s (%s)', [_fn, e.Message, e.ClassName]));
      end;
    end;
  finally
    FreeAndNil(Orig, Changed);
  end;
end;

procedure TDprojFilterMain.HandleParam(const _Parameter: string; const _DeleteLine: string);
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
      HandleFile(Files[FileIdx], _DeleteLine);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

function TDprojFilterMain.doExecute: Integer;
var
  Parameters: TStringList;
  DeleteLine: string;
  ParamIdx: Integer;
begin
  Parameters := TStringList.Create;
  try
    FGetOpt.ParamPassed('DprojFile', Parameters);
    if not FGetOpt.OptionPassed('DeleteLine', DeleteLine) then
      raise Exception.Create('You must pass one of the options: --DeleteLine');
    DeleteLine := UnquoteString(DeleteLine);
    for ParamIdx := 0 to Parameters.Count - 1 do begin
      HandleParam(Parameters[ParamIdx], DeleteLine);
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
//  FGetOpt.RegisterOption('DeleteLineRegEx', 'Delete a line matching the parameter, regular expressions are allowed. E.g. --DeleteLine="<DCC_DcpOutput>..\..\lib\16</DCC_DcpOutput>"', 1)
end;

end.
