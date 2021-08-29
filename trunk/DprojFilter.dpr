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
//* See following notes for complement information.                              *
//* You should not remove these comments.                                        *
//********************************************************************************
//

program DprojFilter;

{$APPTYPE CONSOLE}

uses
  SysUtils,
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
  u_DprojFilterOptionKeyWords in 'u_DprojFilterOptionKeyWords.pas';

{$R *.res}

{ DprojFilter_LogUserMessage }
procedure DprojFilter_LogUserMessage(const sMsgToShow: string; const iContext: tOUS_Context);
begin
  //We're building a plain console application.
  //We will ourput to console, both regular and error message to the same output.
  //For the error, we will add '>>> ' at the beginning of the line to attempt to attract the attention.
  case iContext of
    ouscINFORMATION: WriteLn(sMsgToShow);
    ouscERROR: WriteLn('>>> ' + sMsgToShow);
  end;
end;

{ DprojFilter_DisplayAppInfo }
procedure DprojFilter_DisplayAppInfo;
begin
  WriteUserMessage('DprojFilter ver 1.0.0.0');
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

