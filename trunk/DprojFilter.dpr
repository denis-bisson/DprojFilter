//********************************************************************************
//* DprojFilter                                                                  *
//* -----------------------------------------------------------------------------*
//* The main projet file.                                                        *
//* Was originally a mix of console/visual application. Now it's console 100%.   *
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

program DprojFilter;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.StrUtils,
  System.Types,
  WinApi.Windows,
  u_DprojFilterMain in 'u_DprojFilterMain.pas',
  u_dzCmdLineParser in 'dzlib\src\u_dzCmdLineParser.pas',
  u_dzCmdLineParserStates in 'dzlib\src\u_dzCmdLineParserStates.pas',
  u_dzConvertUtils in 'dzlib\src\u_dzConvertUtils.pas',
  u_dzDateUtils in 'dzlib\src\u_dzDateUtils.pas',
  u_dzDefaultMain in 'dzlib\src\u_dzDefaultMain.pas',
  u_dzFileStreams in 'dzlib\src\u_dzFileStreams.pas',
  u_dzFileUtils in 'dzlib\src\u_dzFileUtils.pas',
  u_dzGetOpt in 'dzlib\src\u_dzGetOpt.pas',
  u_dzLineBuilder in 'dzlib\src\u_dzLineBuilder.pas',
  u_dzMiscUtils in 'dzlib\src\u_dzMiscUtils.pas',
  u_dzNullableTypesUtils in 'dzlib\src\u_dzNullableTypesUtils.pas',
  u_dzOptionDescList in 'dzlib\src\u_dzOptionDescList.pas',
  u_dzOptionFoundList in 'dzlib\src\u_dzOptionFoundList.pas',
  u_dzOptionNameList in 'dzlib\src\u_dzOptionNameList.pas',
  u_dzOsUtils in 'dzlib\src\u_dzOsUtils.pas',
  u_dzParamDescList in 'dzlib\src\u_dzParamDescList.pas',
  u_dzParamFoundList in 'dzlib\src\u_dzParamFoundList.pas',
  u_dzQuicksort in 'dzlib\src\u_dzQuicksort.pas',
  u_dzSortUtils in 'dzlib\src\u_dzSortUtils.pas',
  u_dzStringUtils in 'dzlib\src\u_dzStringUtils.pas',
  u_dzTranslator in 'dzlib\src\u_dzTranslator.pas',
  u_dzTypes in 'dzlib\src\u_dzTypes.pas',
  u_dzVariantUtils in 'dzlib\src\u_dzVariantUtils.pas',
  RegExpr in 'RegExLib\RegExpr.pas',
  uOutputUserMessage in 'uOutputUserMessage.pas',
  u_DprojFilterOptionKeyWords in 'u_DprojFilterOptionKeyWords.pas',
  u_GroupOptions in 'u_GroupOptions.pas';

{$R *.res}

{ DprojFilter_GetVersionName }
function DprojFilter_GetVersionName: string;
{ ---------------------------------------------------------
  Extracts the FileVersion element of the VERSIONINFO
  structure that Delphi maintains as part of a project's
  options.

  Results are returned as a standard string.  Failure
  is reported as "".

  Note that this implementation was derived from similar
  code used by Delphi to validate ComCtl32.dll.  For
  details, see COMCTRLS.PAS, line 3541.
  -------------------------------------------------------- }
const
  NOVIDATA = '';

var
  dwInfoSize, // Size of VERSIONINFO structure
  dwVerSize, // Size of Version Info Data
  dwWnd: DWORD; // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf: Pointer; // pointer to a version buffer
  V1, V2, V3, V4: string;
begin
  V1 := '';
  V2 := '';
  V3 := '';
  V4 := '';

  dwInfoSize := getFileVersionInfoSize(PChar(GetModuleName(HInstance)), dwWnd);

  if (dwInfoSize = 0) then
    result := NOVIDATA
  else
  begin
    getMem(ptrVerBuf, dwInfoSize);
    try
      if getFileVersionInfo(PChar(GetModuleName(HInstance)), dwWnd, dwInfoSize, ptrVerBuf) then
      begin
        if verQueryValue(ptrVerBuf, '\', Pointer(FI), dwVerSize) then
        begin
          V1 := Format('%d', [hiWord(FI.dwFileVersionMS)]);
          V2 := Format('%d', [loWord(FI.dwFileVersionMS)]);
          V3 := Format('%d', [hiWord(FI.dwFileVersionLS)]);
          V4 := Format('%d', [loWord(FI.dwFileVersionLS)]);
          result := V1 + '.' + V2 + '.' + V3 + '.' + V4;
        end;
      end;
    finally
      freeMem(ptrVerBuf);
    end;
  end;
end;

{ DprojFilter_LogUserMessage }
procedure DprojFilter_LogUserMessage(const sMsgToShow: string; const iContext: tOUS_Context);
var
  sLineHeader: string;
  iLastPosNewLine, iPosNewLine: integer;
  bIsFirstLine: boolean;

  procedure AddChunkToResult(sChunk: string);
  begin
    WriteLn(sLineHeader + sChunk);
    if bIsFirstLine then
    begin
      sLineHeader := StringOfChar(' ', length(sLineHeader));
      bIsFirstLine := False;
    end;
  end;
begin
  //We're building a plain console application.
  //We will ourput to console, both regular and error message to the same output.
  //For the error, we will add '>>> ' at the beginning of the line to attempt to attract the attention.
  bIsFirstLine := True;

  case iContext of
    ouscINFORMATION: sLineHeader := '';
    ouscERROR: sLineHeader := '>>> ';
  end;

  iLastPosNewLine := 1;
  repeat
    iPosNewLine := posex(#$0D#$0A, sMsgToShow, iLastPosNewLine);
    if iPosNewLine = 0 then
    begin
      AddChunkToResult(copy(sMsgToShow, iLastPosNewLine));
    end
    else
    begin
      AddChunkToResult(copy(sMsgToShow, iLastPosNewLine, iPosNewLine - iLastPosNewLine));
      iLastPosNewLine := succ(succ(iPosNewLine));
    end;
  until iPosNewLine = 0;
end;

{ DprojFilter_DisplayAppInfo }
procedure DprojFilter_DisplayAppInfo;
begin
  WriteUserMessage('DprojFilter ver ' + DprojFilter_GetVersionName);
  WriteUserMessage('--------------------------------------------------------------------------------');
  WriteUserMessage('Originally and mainly written by Thomas Mueller');
  WriteUserMessage('  https://osdn.net/projects/dprojfilter');
  WriteUserMessage('This little adaptation written by Denis Bisson, Drummondville, Québec, Canada');
  WriteUserMessage('  https://osdn.net/projects/dprojfilter');
  WriteUserMessage('This adaptation written by Denis Bisson');
  WriteUserMessage('Uging TRegExpr class library from Andrey V. Sorokin, St.Petersburg, Russia');
  WriteUserMessage('  http://RegExpStudio.com http://anso.da.ru/');
  WriteUserMessage('--------------------------------------------------------------------------------');
end;

begin
  OUM_OutputUserMessage := @DprojFilter_LogUserMessage;

  DprojFilter_DisplayAppInfo;

  try
    MainClass := TDprojFilterMain;
    System.ExitCode := Main;
  except
    on e: Exception do WriteUserMessage(Format('"%s" : "%s"', [e.ClassName, e.Message]), ouscERROR);
  end;
end.

