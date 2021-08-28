unit u_DprojFilterMain;

{$I 'jedi.inc'}

interface

uses
  RegExpr,
  SysUtils,
  Classes,
  u_dzDefaultMain,
  uOutputUserMessage;

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
    FCurrentFilename: string;
    FCurrentFileChangeCount: integer;
    FKeepGoing: integer;
    FOriginalFile: TStringList;
    FModifiedFile: TStringList;
    procedure HandleParam(const slOptionsList: TStringList; const _Parameter: string);
    procedure HandleFile(const slOptionsList: TStringList);
    function HandleParamFile: integer;
    function HandleOptionList(const slOptionsList: TStringList): integer;
    function ParseOptionLine(const sOptionLine: string): integer;
    procedure FlushOptionCommands;
    function isOptionCommandsToDo: boolean;
  protected
    procedure InitCmdLineParser; override;
    function doExecute: Integer; override;
  end;

implementation

uses
  u_dzFileUtils,
  u_dzStringUtils,
  u_dzMiscUtils, System.StrUtils;

{ TDprojFilterMain }

function SimpleDequoteString(const _s: string): string;
var
  Len: Integer;
begin
  Result := _s;
  Len := Length(Result);
  if Result <> '' then
  begin
    if (Result[1] = '"') and (Result[Len] = '"') then
      Result := Copy(Result, 2, Len - 2);
  end;
end;

function TDprojFilterMain.doExecute: Integer;
var
  Parameters: TStringList;
  ParamIdx: Integer;
  slOptionList: TStringList;
  sOptionLine, sMaybeAction: string;
begin
  Parameters := TStringList.Create;
  try
    FGetOpt.ParamPassed('DprojFile', Parameters);

    sOptionLine := '';
    if FGetOpt.OptionPassed('DeleteLine', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--DeleteLine=' + sMaybeAction;
    if FGetOpt.OptionPassed('ChangeFrom', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--ChangeFrom=' + sMaybeAction;
    if FGetOpt.OptionPassed('ChangeTo', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--ChangeTo=' + sMaybeAction;
    if FGetOpt.OptionPassed('RegExReplaceFrom', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--RegExReplaceFrom=' + sMaybeAction;
    if FGetOpt.OptionPassed('RegExReplaceTo', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--RegExReplaceTo=' + sMaybeAction;
    if FGetOpt.OptionPassed('InsertAfter', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--InsertAfter=' + sMaybeAction;
    if FGetOpt.OptionPassed('Insert', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--Insert=' + sMaybeAction;
    if FGetOpt.OptionPassed('InsertAfterAll', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--InsertAfterAll=' + sMaybeAction;
    if FGetOpt.OptionPassed('InsertAfterAllowDuplicates', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--InsertAfterAllowDuplicates=' + sMaybeAction;
    if FGetOpt.OptionPassed('GroupOptions', sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--GroupOptions=' + sMaybeAction;

    if sOptionLine = '' then raise Exception.Create('You must pass one of the options: --DeleteLine, --ChangeFrom, --RegExReplaceFrom or --InsertAfter');

    slOptionList := TStringList.Create;
    try
      slOptionList.Add(sOptionLine);
      for ParamIdx := 0 to pred(Parameters.Count) do HandleParam(slOptionList, Parameters[ParamIdx]);
    finally
      FreeAndNil(slOptionList);
    end;
  finally
    FreeAndNil(Parameters);
  end;
  Result := 0;
end;

{ TDprojFilterMain.HandleParam }
procedure TDprojFilterMain.HandleParam(const slOptionsList: TStringList; const _Parameter: string);
var
  FileIdx: Integer;
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    TSimpleDirEnumerator.EnumFilesOnly(_Parameter, Files, True);
    WriteUserMessage(Format('Found %d file%s matching %s', [Files.Count, IfThen(Files.Count > 1, 's', ''), _Parameter]));
    for FileIdx := 0 to pred(Files.Count) do
    begin
      FCurrentFilename := Files[FileIdx];
      HandleFile(slOptionsList);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

{ TDprojFilterMain.HandleFile }
procedure TDprojFilterMain.HandleFile(const slOptionsList: TStringList);
var
  bakfn: string;
begin
  WriteUserMessage(Format('Processing file "%s"...', [FCurrentFilename]));
  FreeAndNil(FOriginalFile);
  FreeAndNil(FModifiedFile);

  FOriginalFile := TStringList.Create;
  FModifiedFile := TStringList.Create;
  try
    FOriginalFile.LoadFromFile(FCurrentFilename);
{$IFDEF SUPPORTS_UNICODE}
    FOriginalFile.DefaultEncoding := FOriginalFile.Encoding;
{$ENDIF}
    FCurrentFileChangeCount := 0;
    FKeepGoing := 0;

    HandleOptionList(slOptionsList);

    WriteUserMessage(Format('Changed %d line%s.', [FCurrentFileChangeCount, IfThen(FCurrentFileChangeCount > 1, 's', '')]));

    if FCurrentFileChangeCount > 0 then
    begin
      bakfn := FCurrentFilename + '.bak';
      if TFileSystem.FileExists(bakfn) then
        WriteUserMessage(Format('File "%s" already exists, will not overwrite it.', [bakfn]))
      else
      begin
        WriteUserMessage(Format('Writing backup to "%s"', [bakfn]));
        FModifiedFile.SaveToFile(bakfn);
      end;
      FOriginalFile.SaveToFile(FCurrentFilename, TEncoding.UTF8);
    end;

  finally
    FreeAndNil(FOriginalFile);
    FreeAndNil(FModifiedFile);
  end;
end;

{ TDprojFilterMain.HandleOptionList }
function TDprojFilterMain.HandleOptionList(const slOptionsList: TStringList): integer;
var
  iCurrentOptionLine: Integer;
begin
  result := 1;

  iCurrentOptionLine := 0;
  while (iCurrentOptionLine < slOptionsList.Count) and (FKeepGoing = 0) do
  begin
    FKeepGoing := ParseOptionLine(slOptionsList.Strings[iCurrentOptionLine]);
    if FKeepGoing = 0 then
    begin
      if isOptionCommandsToDo then
      begin
        FKeepGoing := HandleParamFile;
        if FKeepGoing = 0 then
        begin
          FOriginalFile.Assign(FModifiedFile);
          FModifiedFile.Clear;
        end;
      end;
    end;
    inc(iCurrentOptionLine);
  end;

  if (iCurrentOptionLine = slOptionsList.Count) and (FKeepGoing = 0) then result := 0;
end;

{ TDprojFilterMain.HandleParamFile }
function TDprojFilterMain.HandleParamFile: integer;
var
  RegExpr: TRegExpr;
  Line, s: string;
  i, p: Integer;
  InsertAfterDone: Boolean;
  doInsert: Boolean;
begin
  InitializeNil(RegExpr);
  RegExpr := TRegExpr.Create;
  try
    RegExpr.Expression := FRegExReplaceFrom;
    try

      InsertAfterDone := False;
      for i := 0 to FOriginalFile.Count - 1 do
      begin
        Line := FOriginalFile[i];
        s := Trim(Line);
        if SameText(s, FDeleteLine) then
        begin
          Inc(FCurrentFileChangeCount);
        end
        else if SameText(s, FChangeFrom) then
        begin
          // retain indentation
          p := Pos(s, Line);
          Assert(p > 0);
          s := Copy(Line, 1, p - 1) + FChangeTo;
          FModifiedFile.Add(s);
          Inc(FCurrentFileChangeCount);
        end
        else if SameText(s, FInsertAfter) then
        begin
          FModifiedFile.Add(Line);
          if not InsertAfterDone then
          begin
            // retain indentation
            p := Pos(s, Line);
            Assert(p > 0);
            doInsert := True;
            if i < FModifiedFile.Count - 1 then
            begin
              s := Trim(FModifiedFile[i + 1]);
              if SameText(s, FInsert) then
              begin
                // Line to insert already exists
                if not FINsertAfterAllowDuplicates then
                begin
                  doInsert := False;
                end;
              end;
            end;
            if doInsert then
            begin
              s := Copy(Line, 1, p - 1) + FInsert;
              FModifiedFile.Add(s);
              Inc(FCurrentFileChangeCount);
            end;
            if not FInsertAfterAll then
              InsertAfterDone := True;
          end;
        end
        else
        begin
          if FRegExReplaceFrom <> '' then
            if RegExpr.Exec(Line) then
            begin
              Line := RegExpr.Replace(Line, FRegExReplaceTo, True);
              Inc(FCurrentFileChangeCount);
            end;
          FModifiedFile.Add(Line);
        end;
      end;

    except
      on e: Exception do
      begin
        WriteUserMessage(Format('Error processing "%s": %s (%s)', [FCurrentFilename, e.Message, e.ClassName]), ouscERROR);
      end;
    end;
  finally
    FreeAndNil(RegExpr);
  end;
  result := 0;
end;

procedure TDprojFilterMain.FlushOptionCommands;
begin
  FDeleteLine := '';
  FChangeFrom := '';
  FChangeTo := '';
  FRegExReplaceFrom := '';
  FRegExReplaceTo := '';
  FInsertAfter := '';
  FInsert := '';
end;

function TDprojFilterMain.isOptionCommandsToDo: boolean;
begin
  if FDeleteLine <> '' then
    result := True
  else if FChangeFrom <> '' then
    result := True
  else if FChangeTo <> '' then
    result := True
  else if FRegExReplaceFrom <> '' then
    result := True
  else if FRegExReplaceTo <> '' then
    result := True
  else if FInsertAfter <> '' then
    result := True
  else if FInsert <> '' then
    result := True
  else
    result := False;
end;

{ TDprojFilterMain.ParseOptionLine }
function TDprojFilterMain.ParseOptionLine(const sOptionLine: string): integer;
var
  OptionsOK: boolean;
  sOptionFilename: string;
  slOptionListOnTheFly: TStringList;
begin
  FlushOptionCommands;
  OptionsOK := False;

  FGetOpt.ParamsFoundList.Clear;
  FGetOpt.OptionsFoundList.Clear;
  FGetOpt.ParseAssumingJustOptions(sOptionLine);

  if FGetOpt.OptionPassed('DeleteLine', FDeleteLine) then OptionsOK := True;

  if FGetOpt.OptionPassed('ChangeFrom', FChangeFrom) then
  begin
    if FGetOpt.OptionPassed('ChangeTo', FChangeTo) then
    begin
      OptionsOK := True;
    end
    else
      raise Exception.Create('--ChangeFrom also requires --ChangeTo option');
  end
  else
  begin
    if FGetOpt.OptionPassed('ChangeTo', FChangeTo) then
      raise Exception.Create('--ChangeTo is not allowed without a --ChangeFrom option');
  end;

  if FGetOpt.OptionPassed('RegExReplaceFrom', FRegExReplaceFrom) then
  begin
    if FGetOpt.OptionPassed('RegExReplaceTo', FRegExReplaceTo) then
    begin
      OptionsOK := True;
    end
    else
      raise Exception.Create('--RegExReplaceFrom also requires --RegExReplaceTo option');
  end
  else
  begin
    if FGetOpt.OptionPassed('RegExReplaceTo', FRegExReplaceTo) then
      raise Exception.Create('--RegExReplaceTo is not allowed without a --RegExReplaceFrom option');
  end;

  FINsertAfterAllowDuplicates := False;
  FInsertAfterAll := False;
  if FGetOpt.OptionPassed('InsertAfter', FInsertAfter) then
  begin
    if FGetOpt.OptionPassed('Insert', FInsert) then
    begin
      OptionsOK := True;
    end
    else
      raise Exception.Create('--InsertAfter also requires an --Insert option');
    FInsertAfterAll := FGetOpt.OptionPassed('InsertAfterAll');
    FINsertAfterAllowDuplicates := FGetOpt.OptionPassed('InsertAfterAllowDuplicates');
  end
  else
  begin
    if FGetOpt.OptionPassed('Insert', FInsert) then
      raise Exception.Create('--Insert is not allowed without an --InsertAfter option');
    if FGetOpt.OptionPassed('InsertAfterAll') then
      raise Exception.Create('--InsertAfterAll is not allowed without an --InsertAfter option');
    if FGetOpt.OptionPassed('InsertAfterAllowDuplicates') then
      raise Exception.Create('--InsertAfterAllowDuplicates is not allowed without an --InsertAfter option');
  end;

  if FGetOpt.OptionPassed('GroupOptions', sOptionFilename) then
  begin
    slOptionListOnTheFly := TStringList.Create;
    try
      slOptionListOnTheFly.LoadFromFile(sOptionFilename);
      HandleOptionList(slOptionListOnTheFly);
    finally
      FreeAndNil(slOptionListOnTheFly);
    end;
    FlushOptionCommands;
    OptionsOk := True;
  end;

  if not OptionsOK then
  begin
    raise Exception.Create('You must pass one of the options: --DeleteLine, --ChangeFrom, --RegExReplaceFrom or --InsertAfter')
  end
  else
  begin
    FDeleteLine := SimpleDequoteString(FDeleteLine);
    FChangeFrom := SimpleDequoteString(FChangeFrom);
    FChangeTo := SimpleDequoteString(FChangeTo);
    FRegExReplaceFrom := SimpleDequoteString(FRegExReplaceFrom);
    FRegExReplaceTo := SimpleDequoteString(FRegExReplaceTo);
    FInsertAfter := SimpleDequoteString(FInsertAfter);
    FInsert := SimpleDequoteString(FInsert);
    result := 0;
  end;
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
  FGetOpt.RegisterOption('GroupOptions', 'Will assume given option will be a text file where each line is a series of options to execute.', True);
  FGetOpt.DequoteParams := False;
end;

end.

