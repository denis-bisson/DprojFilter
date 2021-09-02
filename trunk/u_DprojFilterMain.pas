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
  uOutputUserMessage,
  u_GroupOptions;

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
    FFileIdx, FFileCount: integer;
    FFilesToProcess: TStringList;
    FCurrentFilename: string;
    FCurrentFileChangeCount: integer;
    FKeepGoing: integer;
    FOriginalFile: TStringList;
    FModifiedFile: TStringList;
    FGroupOptionsList: TGroupOptionsList;
    procedure GoCollectAllTheFilesToProcess(const _Parameter: string; const sOriginOfLines: string);
    procedure HandleParam(const slOptionsList: TStringList; const sOriginOfLines: string);
    procedure HandleFile(const slOptionsList: TStringList; const sOriginOfLines: string);
    function HandleParamFile: integer;
    function HandleOptionList(const slOptionsList: TStringList; const sOriginOfLines: string; const iDeepLevel: integer): integer;
    function ParseOptionLine(const sOptionLine: string; const sOriginOfLine: string; const iDeepLevel: integer): integer;
    procedure FlushOptionCommands;
    function isOptionCommandsToDo: boolean;
    procedure RaiseExceptionSinceNoOptions;
  protected
    procedure InitCmdLineParser; override;
    function doExecute: Integer; override;
  public
    property GroupOptionsList: TGroupOptionsList read FGroupOptionsList;
  end;

implementation

uses
  System.StrUtils,
  u_dzFileUtils,
  u_dzStringUtils,
  u_dzMiscUtils,
  u_DprojFilterOptionKeyWords;

const
  sCOMMANDLINENAME = 'Command-line';
  sBARELYIMPOSSIBLEVALUE = '{D686FB37-E10E-48AB-8C85-0A6DE4E0247D}-{74CACA31-286D-422E-98D8-39472229F6ED}';

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
  FGroupOptionsList := TGroupOptionsList.Create;
  try
    FGetOpt.ParamPassed('filename', Parameters);

    sOptionLine := '';
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_DeleteLine], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_DeleteLine] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeFrom], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_ChangeFrom] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeTo], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_ChangeTo] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceFrom], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_RegExReplaceFrom] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceTo], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_RegExReplaceTo] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfter], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_InsertAfter] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_Insert], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_Insert] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAll], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_InsertAfterAll] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_InsertAfterAllowDuplicates] + '=' + sMaybeAction;
    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_GroupOptions], sMaybeAction) then sOptionLine := sOptionLine + IfThen(sOptionLine <> '', ' ', '') + '--' + slKeyWords.Strings[KWD_GroupOptions] + '=' + sMaybeAction;

    if sOptionLine = '' then RaiseExceptionSinceNoOptions;

    slOptionList := TStringList.Create;
    try
      FFilesToProcess := TStringList.Create;
      try
        FFilesToProcess.Sorted := True;
        FFilesToProcess.Duplicates := dupIgnore;

        slOptionList.Add(sOptionLine);
        for ParamIdx := 0 to pred(Parameters.Count) do
          GoCollectAllTheFilesToProcess(Parameters[ParamIdx], Parameters[ParamIdx]);

        HandleParam(slOptionList, sCOMMANDLINENAME + ' (' + sOptionLine + ')');
      finally
        FreeAndNil(FFilesToProcess);
      end;
    finally
      FreeAndNil(slOptionList);
    end;
  finally
    FreeAndNil(FGroupOptionsList);
    FreeAndNil(Parameters);
  end;
  Result := 0;
end;

procedure TDprojFilterMain.GoCollectAllTheFilesToProcess(const _Parameter: string; const sOriginOfLines: string);
var
  Files: TStringList;
  iFileIndex: integer;
begin
  Files := TStringList.Create;
  try
    TSimpleDirEnumerator.EnumFilesOnlyFirstLevel(_Parameter, Files, sCOMMANDLINENAME, True);
    for iFileIndex := 0 to pred(Files.Count) do
      FFilesToProcess.Add(Files.Strings[iFileIndex]);

    WriteUserMessage(Format('Found %d file%s matching %s', [Files.Count, IfThen(Files.Count > 1, 's', ''), _Parameter]));
  finally
    FreeAndNil(Files);
  end;
end;

{ TDprojFilterMain.HandleParam }
procedure TDprojFilterMain.HandleParam(const slOptionsList: TStringList; const sOriginOfLines: string);
begin
  FFileIdx := 0;
  FFileCount := FFilesToProcess.Count;
  while FFileIdx < FFileCount do
  begin
    FCurrentFilename := FFilesToProcess[FFileIdx];
    HandleFile(slOptionsList, sOriginOfLines);
    inc(FFileIdx);
  end;
end;

{ TDprojFilterMain.HandleFile }
procedure TDprojFilterMain.HandleFile(const slOptionsList: TStringList; const sOriginOfLines: string);
var
  bakfn: string;
  slOriginalFileLines: TStringList;
  iAttemptIndex: integer;
begin
  WriteUserMessage('');
  WriteUserMessage(Format('   Processing: "%s"', [FCurrentFilename]));
  WriteUserMessage(Format('         File: %d of %d', [succ(FFileIdx), FFileCount]));
  FreeAndNil(FOriginalFile);
  FreeAndNil(FModifiedFile);

  FOriginalFile := TStringList.Create;
  FModifiedFile := TStringList.Create;
  slOriginalFileLines := TStringList.Create;
  try
    FOriginalFile.LoadFromFile(FCurrentFilename);
{$IFDEF SUPPORTS_UNICODE}
    FOriginalFile.DefaultEncoding := FOriginalFile.Encoding;
{$ENDIF}
    slOriginalFileLines.Assign(FOriginalFile);
    FCurrentFileChangeCount := 0;
    FKeepGoing := 0;

    WriteUserMessage(Format('       Before: %d line%s', [FOriginalFile.Count, IfThen(FOriginalFile.Count > 1, 's', '')]));
    HandleOptionList(slOptionsList, sOriginOfLines, 0);
    WriteUserMessage(Format('        After: %d line%s', [FOriginalFile.Count, IfThen(FOriginalFile.Count > 1, 's', '')]));
    WriteUserMessage(Format('Modifications: %d', [FCurrentFileChangeCount]));

    if FCurrentFileChangeCount > 0 then
    begin
      iAttemptIndex := 1;
      repeat
        case iAttemptIndex of
          1: bakfn := FCurrentFilename + '.bak'
        else
          bakfn := FCurrentFilename + '.bak(' + iAttemptIndex.ToString + ')';
        end;
        inc(iAttemptIndex);
      until not TFileSystem.FileExists(bakfn);
      FOriginalFile.SaveToFile(bakfn);
      WriteUserMessage(Format('  Backup file: "%s"', [bakfn]));
      FOriginalFile.SaveToFile(FCurrentFilename, FOriginalFile.DefaultEncoding);
    end;

  finally
    FreeAndNil(slOriginalFileLines);
    FreeAndNil(FOriginalFile);
    FreeAndNil(FModifiedFile);
  end;
end;

{ TDprojFilterMain.HandleOptionList }
function TDprojFilterMain.HandleOptionList(const slOptionsList: TStringList; const sOriginOfLines: string; const iDeepLevel: integer): integer;
var
  iCurrentOptionLine: Integer;
begin
  result := 1;

  iCurrentOptionLine := 0;
  while (iCurrentOptionLine < slOptionsList.Count) and (FKeepGoing = 0) do
  begin
    if (slOptionsList.Strings[iCurrentOptionLine] <> '') and (LeftStr(slOptionsList.Strings[iCurrentOptionLine], 2) <> '//') then
    begin
      FKeepGoing := ParseOptionLine(slOptionsList.Strings[iCurrentOptionLine], IfThen(iDeepLevel = 0, sCOMMANDLINENAME, sOriginOfLines + ' line #' + succ(iCurrentOptionLine).ToString), iDeepLevel);
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
  FDeleteLine := sBARELYIMPOSSIBLEVALUE;
  FChangeFrom := sBARELYIMPOSSIBLEVALUE;
  FChangeTo := '';
  FRegExReplaceFrom := sBARELYIMPOSSIBLEVALUE;
  FRegExReplaceTo := '';
  FInsertAfter := sBARELYIMPOSSIBLEVALUE;
  FInsert := '';
end;

function TDprojFilterMain.isOptionCommandsToDo: boolean;
begin
  if FDeleteLine <> sBARELYIMPOSSIBLEVALUE then
    result := True
  else if FChangeFrom <> sBARELYIMPOSSIBLEVALUE then
    result := True
  else if FChangeTo <> '' then
    result := True
  else if FRegExReplaceFrom <> sBARELYIMPOSSIBLEVALUE then
    result := True
  else if FRegExReplaceTo <> '' then
    result := True
  else if FInsertAfter <> sBARELYIMPOSSIBLEVALUE then
    result := True
  else if FInsert <> '' then
    result := True
  else
    result := False;
end;

procedure TDprojFilterMain.RaiseExceptionSinceNoOptions;
var
  sErrorLine: string;
  iOptionIndex: integer;
begin
  sErrorLine := 'You must pass at least one of the following options: ' + #$0D#$0A;
  for iOptionIndex := 0 to pred(slKeyWords.Count) do
    sErrorLine := sErrorLine + '          ' + slKeyWords.Strings[iOptionIndex] + #$0D + #$0A;
  raise Exception.Create(sErrorLine);
end;

{ TDprojFilterMain.ParseOptionLine }
function TDprojFilterMain.ParseOptionLine(const sOptionLine: string; const sOriginOfLine: string; const iDeepLevel: integer): integer;
var
  OptionsOK: boolean;
  sOptionFilename: string;
  iGroupOptionsIndex: integer;
  AGroupOptions: TGroupOptions;

  procedure RaiseExceptionWithErrorMessage(sErrorMessage: string);
  begin
    WriteUserMessage('ERROR!' + #$0D + #$0A + 'Origin: ' + sOriginOfLine + #$0D#$0A + '  Line: ' + sOptionLine + #$0D#$0A + '  Type: ' + sErrorMessage, ouscERROR);
    if (DebugHook <> 0) and IsConsole then
    begin
      WriteUserMessage(' -- press Enter');
      Readln;
    end;
    halt(2);
  end;

begin
  result := 1;
  FlushOptionCommands;
  OptionsOK := False;

  FGetOpt.ParamsFoundList.Clear;
  FGetOpt.OptionsFoundList.Clear;
  try
    FGetOpt.ParseAssumingJustOptions(sOptionLine);

    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_DeleteLine], FDeleteLine) then OptionsOK := True;

    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeFrom], FChangeFrom) then
    begin
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeTo], FChangeTo) then
        OptionsOK := True
      else
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_ChangeFrom] + ' also requires --' + slKeyWords.Strings[KWD_ChangeTo] + ' option');
    end
    else
    begin
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_ChangeTo], FChangeTo) then
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_ChangeTo] + ' is not allowed without a --' + slKeyWords.Strings[KWD_ChangeFrom] + ' option');
    end;

    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceFrom], FRegExReplaceFrom) then
    begin
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceTo], FRegExReplaceTo) then
      begin
        OptionsOK := True;
      end
      else
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_RegExReplaceFrom] + ' also requires --' + slKeyWords.Strings[KWD_RegExReplaceTo] + ' option');
    end
    else
    begin
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_RegExReplaceTo], FRegExReplaceTo) then
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_RegExReplaceTo] + ' is not allowed without a --' + slKeyWords.Strings[KWD_RegExReplaceFrom] + ' option');
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
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_InsertAfter] + ' also requires an --' + slKeyWords.Strings[KWD_Insert] + ' option');
      FInsertAfterAll := FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAll]);
      FINsertAfterAllowDuplicates := FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates]);
    end
    else
    begin
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_Insert], FInsert) then
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_Insert] + ' is not allowed without an --' + slKeyWords.Strings[KWD_InsertAfter] + ' option');
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAll]) then
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_InsertAfterAll] + ' is not allowed without an --' + slKeyWords.Strings[KWD_InsertAfter] + ' option');
      if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates]) then
        RaiseExceptionWithErrorMessage('--' + slKeyWords.Strings[KWD_InsertAfterAllowDuplicates] + ' is not allowed without an --' + slKeyWords.Strings[KWD_InsertAfter] + ' option');
    end;

    if FGetOpt.OptionPassed(slKeyWords.Strings[KWD_GroupOptions], sOptionFilename) then
    begin
      sOptionFilename := SimpleDequoteString(sOptionFilename);
      if FileExists(sOptionFilename) then
      begin
        iGroupOptionsIndex := GroupOptionsList.FindByFilename(sOptionFilename);
        if iGroupOptionsIndex = -1 then
        begin
          AGroupOptions := TGroupOptions.Create(sOptionFilename);
          iGroupOptionsIndex := GroupOptionsList.Add(AGroupOptions);
        end;

        if GroupOptionsList.GroupOptions[iGroupOptionsIndex].CurrentlyParsing = False then
        begin
          GroupOptionsList.GroupOptions[iGroupOptionsIndex].CurrentlyParsing := True;
          HandleOptionList(GroupOptionsList.GroupOptions[iGroupOptionsIndex].OptionsLines, sOriginOfLine + ' (' + sOptionLine + ')' + #$0D#$0A + '        ' + slKeyWords.Strings[KWD_GroupOptions] + ' from file "' + sOptionFilename + '"', succ(iDeepLevel));
          GroupOptionsList.GroupOptions[iGroupOptionsIndex].CurrentlyParsing := False;
        end
        else
        begin
          RaiseExceptionWithErrorMessage('Circular reference with --' + slKeyWords.Strings[KWD_GroupOptions] + ' assigning a file already in parsing queue.' + #$0D#$0A + '        ' + sOptionFilename);
        end;
      end
      else
      begin
        RaiseExceptionWithErrorMessage('File to include not found "' + sOptionFilename + '"');
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
  except
    on e: Exception do
    begin
      RaiseExceptionWithErrorMessage(e.Message);
    end;
  end;
end;

procedure TDprojFilterMain.InitCmdLineParser;
begin
  inherited;
  FGetOpt.RegisterParam('filename', 'File(s) to process.\nWildcards are allowed like *.dproj, *.txt, etc.\n@Files.lst will be read line per line and process as well.', 1, MaxInt);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_DeleteLine], 'Delete a line matching "value".\nBlanks at the beginning and/or ending of the lines are ignored prior the evaluation.\nExample: --' + slKeyWords.Strings[KWD_DeleteLine] + '="<DCC_DcpOutput>..\..\lib\16</DCC_DcpOutput>"', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_ChangeFrom], 'Change a line matching the "value" to the one of --' + slKeyWords.Strings[KWD_ChangeTo] + '.\nIt checks for the whole line, not part of the line.\nBut blanks at the beginning and/or ending of the lines are ignored prior the evaluation.\nReplacement will be added with the same indentation as the original text.\nSee option --' + slKeyWords.Strings[KWD_RegExReplaceFrom] + ' for replacement inside part of the line.\nExample: --' + slKeyWords.Strings[KWD_ChangeFrom] + '="<DCC_RemoteDebug>true" --' + slKeyWords.Strings[KWD_ChangeTo] + '="<DCC_RemoteDebug>false"', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_ChangeTo], 'Gives the new content for the --' + slKeyWords.Strings[KWD_ChangeFrom] + ' option.\nSee option --' + slKeyWords.Strings[KWD_ChangeFrom] + '.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_RegExReplaceFrom], 'Idem as --' + slKeyWords.Strings[KWD_ChangeFrom] + ' but with "value" being a regular expression.\nMust be paired with --' + slKeyWords.Strings[KWD_RegExReplaceTo] + '.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_RegExReplaceTo], 'Same as ' + slKeyWords.Strings[KWD_ChangeFrom] + ' but when using Regular Expression.\nSee option --' + slKeyWords.Strings[KWD_RegExReplaceFrom] + '.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_InsertAfter], slKeyWords.Strings[KWD_Insert] + ' a new line after the one matching "Value".\nRequires an --' + slKeyWords.Strings[KWD_Insert] + ' option.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_Insert], 'Gives the line to insert for the --' + slKeyWords.Strings[KWD_InsertAfter] + ' option.', True);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_InsertAfterAll], 'If given, --' + slKeyWords.Strings[KWD_InsertAfter] + ' applies to all matching lines.\nIf not, only to the first.\nRequires an --' + slKeyWords.Strings[KWD_Insert] + ' option.', False);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_InsertAfterAllowDuplicates], 'If given, --' + slKeyWords.Strings[KWD_InsertAfter] + ' will skip the check if the line to insert already exists.\nRequires an --' + slKeyWords.Strings[KWD_Insert] + ' option.', False);
  FGetOpt.RegisterOption(slKeyWords.Strings[KWD_GroupOptions], 'Will assume "value" is a text file where each line is a series of options to execute.\nEmpty lines and lines beginning with "//" will be ignored.', True);
  FGetOpt.DequoteParams := False;
end;

end.

