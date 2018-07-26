program DprojFilter;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  u_DprojFilterMain in 'u_DprojFilterMain.pas',
  u_dzDefaultMain in '..\libs\dzlib\src\u_dzDefaultMain.pas';

{$R *_version.res}
{$R *_icon.res}
{$R *_manifest.res}

begin
  try
    MainClass := TDprojFilterMain;
    System.ExitCode := Main;
  except
    on e: Exception do
      Writeln(e.Classname, ': ', e.Message);
  end;
end.
