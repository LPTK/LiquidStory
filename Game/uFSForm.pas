(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The black form that is behind the main form when fullscreen is enabled
        but the resolution is not 800*600
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit uFSForm; 

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Windows,
  ppe_uTrace
  ;

type
  
  { TFullScreenForm }

  TFullScreenForm = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    //procedure fullScreen(activ:Boolean=true);
  end; 

var
  FullScreenForm: TFullScreenForm; mainForm:TCustomForm;

function SetScreenResolution(Width, Height: integer): Longint;

implementation

function SetScreenResolution(Width, Height: integer): Longint;
var
  DeviceMode: TDeviceMode;
begin
  with DeviceMode do begin
    dmSize := SizeOf(TDeviceMode);
    dmPelsWidth := Width;
    dmPelsHeight := Height;
    dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
  end;
  //Result := ChangeDisplaySettings(DeviceMode, CDS_UPDATEREGISTRY);
  Result := ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN);
end;

procedure TFullScreenForm.FormActivate(Sender: TObject);
begin
     //trace('OK');
     mainForm.setFocus;
end;

procedure TFullScreenForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
	 //trace('!');
     //CanClose := false;
     mainForm.Close;
end;

procedure TFullScreenForm.FormCreate(Sender: TObject);
begin
     //Show;
     Left := 0;
     Top := 0;
     Width := Screen.Width;
     Height := Screen.Height;
     //trace('..');
end;

(*procedure TFullScreenForm.fullScreen(activ:Boolean=true);
begin
  	 Show;
     Left := 0;
     Top := 0;
     Width := Screen.Width;
     Height := Screen.Height;
end;*)

initialization
  {$I FSForm.lrs}

end.

