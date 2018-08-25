program dzDprojEdit;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  u_dzDefaultMain in '..\libs\dzlib\src\u_dzDefaultMain.pas',
  u_DprojEditMain in 'u_DprojEditMain.pas';

{$R *_version.res}
{$R *_icon.res}
{$R *_manifest.res}

begin
//       Application.Initialize;
//       Application.MainFormOnTaskbar := true; // optional for Delphi >= 2007
//       Application.Title := '<your application's title here>';
  MainClass := TDprojEditMain;
  System.ExitCode := Main;
end.

