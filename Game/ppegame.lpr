program ppegame;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMainForm, LResources, gam_uSoundManager, uFSForm, uOptionsForm,
gam_uScore, uSplashScreen;

{$IFDEF WINDOWS}{$R ppegame.rc}{$ENDIF}

begin
  {$I ppegame.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  //Application.CreateForm(TSplashScreenForm, SplashScreenForm);
  Application.CreateForm(TFullScreenForm, FullScreenForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.

