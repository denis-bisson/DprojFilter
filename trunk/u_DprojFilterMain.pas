//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* The main application unit file.                                              *
//* -----------------------------------------------------------------------------*
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
    procedure RaiseExceptionSinceNoOptions;
  protected
    procedure InitCmdLineParser; override;
    function doExecute: Integer; override;
  end;

implementation

uses
  System.StrUtils,
  u_dzFileUtils,
  u_dzStringUtils,
  u_dzMiscUtils,
  u_DprojFilterOptionKeyWords;

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
    FGetOpt.ParamPassed('filename', Parameters);

    sOptionLine := '';
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_DeleteLine], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_DeleteLine]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeFrom], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_ChangeFrom]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeTo], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_ChangeTo]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceFrom], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_RegExReplaceFrom]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceTo], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_RegExReplaceTo]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfter], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_InsertAfter]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_Insert], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_Insert]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAll], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_InsertAfterAll]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_InsertAfterAllowDuplicates]+'=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_GroupOptions], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--'+slKeyWords.Strings[KWD_GroupOptions]+'=' + sMaybeAction;

    if sOptionLine = '' then RaiseExceptionSinceNoOptions;

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
  FDeleteLine := '{0315ED3B-FB6C-410C-82EB-15E2D0CCF22A}'+'{A54D8647-6B69-44AD-8493-2C419E9D5953}';
  FChangeFrom := '{2169FEEA-5E42-4FF2-BD3C-249315C77216}'+'{C1E31065-92D0-432A-9643-C9C718DDB13B}';
  FChangeTo := '';
  FRegExReplaceFrom := '{7246139A-A377-451D-80E3-2CDB4C068D01}'+'{77F749A5-1483-4E7F-BD36-7A1399CF9FCC}';
  FRegExReplaceTo := '';
  FInsertAfter := '{DB34DA7D-D879-48A4-8EDB-F35883AD0E96}'+'{BC3A936A-975B-408A-83A5-E7FDA8454B53}';
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

procedure TDprojFilterMain.RaiseExceptionSinceNoOptions;
var
  sErrorLine:string;
  iOptionIndex:integer;
begin
  sErrorLine:='You must pass at least one of the following options: '+#$0D#$0A;
  for iOptionIndex:=0 to pred(slKeyWords.Count) do
    sErrorLine:=sErrorLine+'  '+slKeyWords.Strings[iOptionIndex]+#$0D+#$0A;
  raise Exception.Create(sErrorLine);
end;

{ TDprojFilterMain.ParseOptionLine }
function TDprojFilterMain.ParseOptionLine(const sOptionLine: string): integer;
var
  OptionsOK: boolean;
  sOptionFilename: string;
  slOptionListOnTheFly: TStringList;
  iLineIndex:integer;
begin
  result:=1;
  FlushOptionCommands;
  OptionsOK := False;

  FGetOpt.ParamsFoundList.Clear;
  FGetOpt.OptionsFoundList.Clear;
  FGetOpt.ParseAssumingJustOptions(sOptionLine);

  if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_DeleteLine], FDeleteLine) then OptionsOK := True;

  if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeFrom], FChangeFrom) then
  begin
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeTo], FChangeTo) then
    begin
      OptionsOK := True;
    end
    else
      raise Exception.Create('--'+slKeyWords.Strings[KWD_ChangeFrom]+' also requires --'+slKeyWords.Strings[KWD_ChangeTo]+' option');
  end
  else
  begin
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeTo], FChangeTo) then
      raise Exception.Create('--'+slKeyWords.Strings[KWD_ChangeTo]+' is not allowed without a --'+slKeyWords.Strings[KWD_ChangeFrom]+' option');
  end;

  if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceFrom], FRegExReplaceFrom) then
  begin
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceTo], FRegExReplaceTo) then
    begin
      OptionsOK := True;
    end
    else
      raise Exception.Create('--'+slKeyWords.Strings[KWD_RegExReplaceFrom]+' also requires --'+slKeyWords.Strings[KWD_RegExReplaceTo]+' option');
  end
  else
  begin
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceTo], FRegExReplaceTo) then
      raise Exception.Create('--'+slKeyWords.Strings[KWD_RegExReplaceTo]+' is not allowed without a --'+slKeyWords.Strings[KWD_RegExReplaceFrom]+' option');
  end;

  FINsertAfterAllowDuplicates := False;
  FInsertAfterAll := False;
  if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfter], FInsertAfter) then
  begin
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_Insert], FInsert) then
    begin
      OptionsOK := True;
    end
    else
      raise Exception.Create('--'+slKeyWords.Strings[KWD_InsertAfter]+' also requires an --'+slKeyWords.Strings[KWD_Insert]+' option');
    FInsertAfterAll := FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAll]);
    FINsertAfterAllowDuplicates := FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates]);
  end
  else
  begin
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_Insert], FInsert) then
      raise Exception.Create('--'+slKeyWords.Strings[KWD_Insert]+' is not allowed without an --'+slKeyWords.Strings[KWD_InsertAfter]+' option');
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAll]) then
      raise Exception.Create('--'+slKeyWords.Strings[KWD_InsertAfterAll]+' is not allowed without an --'+slKeyWords.Strings[KWD_InsertAfter]+' option');
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates]) then
      raise Exception.Create('--'+slKeyWords.Strings[KWD_InsertAfterAllowDuplicates]+' is not allowed without an --'+slKeyWords.Strings[KWD_InsertAfter]+' option');
  end;

  if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_GroupOptions], sOptionFilename) then
  begin
    slOptionListOnTheFly := TStringList.Create;
    try
      sOptionFilename:=SimpleDequoteString(sOptionFilename);
      slOptionListOnTheFly.LoadFromFile(sOptionFilename);
      for iLineIndex:=pred(slOptionListOnTheFly.Count) downto 0 do
        if (Trim(slOptionListOnTheFly.Strings[iLineIndex])='') or (LeftStr(Trim(slOptionListOnTheFly.Strings[iLineIndex]),2)='//')  then
          slOptionListOnTheFly.Delete(iLineIndex);
      if slOptionListOnTheFly.Count>0 then
        HandleOptionList(slOptionListOnTheFly);
    finally
      FreeAndNil(slOptionListOnTheFly);
    end;
    FlushOptionCommands;
    OptionsOk := True;
  end;

  if not OptionsOK then
  begin
    RaiseExceptionSinceNoOptions;
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
  FGetOpt.RegisterParam('filename', 'File(s) to process.\nWildcards are allowed like *.dproj, *.txt, etc.\n@Files.lst will be read line per line and process as well.', 1, MaxInt);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_DeleteLine], 'Delete a line matching "value".\nBlanks at the beginning and/or ending of the lines are ignored prior the evaluation.\nExample: --'+slKeyWords.Strings[KWD_DeleteLine]+'="<DCC_DcpOutput>..\..\lib\16</DCC_DcpOutput>"', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_ChangeFrom], 'Change a line matching the "value" to the one of --'+slKeyWords.Strings[KWD_ChangeTo]+'.\nExample: --'+slKeyWords.Strings[KWD_ChangeFrom]+'="<DCC_RemoteDebug>true" --'+slKeyWords.Strings[KWD_ChangeTo]+'="<DCC_RemoteDebug>false"', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_ChangeTo], 'Gives the new content for the --'+slKeyWords.Strings[KWD_ChangeFrom]+' option.\nSee option --'+slKeyWords.Strings[KWD_ChangeFrom]+'.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_RegExReplaceFrom], 'Idem as --'+slKeyWords.Strings[KWD_ChangeFrom]+' but with "value" being a regular expression.\nMust be paired with --'+slKeyWords.Strings[KWD_RegExReplaceTo]+'.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_RegExReplaceTo], 'Same as '+slKeyWords.Strings[KWD_ChangeFrom]+' but when using Regular Expression.\nSee option --'+slKeyWords.Strings[KWD_RegExReplaceFrom]+'.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_InsertAfter], slKeyWords.Strings[KWD_Insert]+' a new line after the one matching "Value".\nRequires an --'+slKeyWords.Strings[KWD_Insert]+' option.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_Insert], 'Gives the line to insert for the --'+slKeyWords.Strings[KWD_InsertAfter]+' option.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_InsertAfterAll], 'If given, --'+slKeyWords.Strings[KWD_InsertAfter]+' applies to all matching lines.\nIf not, only to the first.\nRequires an --'+slKeyWords.Strings[KWD_Insert]+' option.', False);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates], 'If given, --'+slKeyWords.Strings[KWD_InsertAfter]+' will skip the check if the line to insert already exists.\nRequires an --'+slKeyWords.Strings[KWD_Insert]+' option.', False);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_GroupOptions], 'Will assume "value" is a text file where each line is a series of options to execute.\nEmpty lines and lines beginning with "//" will be ignored.', True);
  FGetOpt.DequoteParams := False;
end;

end.

