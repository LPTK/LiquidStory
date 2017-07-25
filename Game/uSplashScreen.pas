(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The SplashScreen form
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit uSplashScreen; 

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls; 

type
  
  { TSplashScreenForm }

  TSplashScreenForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    bmp:tBitmap;
  public
    { public declarations }
  end; 

var
  SplashScreenForm: TSplashScreenForm; 

implementation

{ TSplashScreenForm }

procedure TSplashScreenForm.FormCreate(Sender: TObject);
begin
	bmp := tBitmap.Create;
    bmp.loadFromFile('img/LS_SplashScreen.bmp');
    Image1.Canvas.Draw(0,0,bmp);
    bmp.Free;
end;

initialization
  {$I uSplashScreen.lrs}

end.

