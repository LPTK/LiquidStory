(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The options form
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit uOptionsForm; 

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls; 

type
  
  { TOptionsForm }

  TOptionsForm = class(TForm)
    btn_close: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure btn_closeClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBox5Change(Sender: TObject);
    procedure CheckBox6Change(Sender: TObject);
    procedure CheckBox7Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
    activ:Boolean;
    procedure ForceClose;
  end; 

var
  OptionsForm: TOptionsForm; 

implementation

uses uMainForm, ppe_uTrace, gam_uSoundManager, gam_uIceParticle;

{ TOptionsForm }

procedure TOptionsForm.CheckBox1Change(Sender: TObject);
begin
     if activ then begin
	   if CheckBox1.Checked then begin
         if Form1.fullScreen and (Screen.Width=FS_SCR_WIDTH) then begin
           Form1.setFullScreen(false);
           Form1.showOptions := true;
           ForceClose;
         end;
         Form1.showConsole;
       end else Form1.hideConsole;
     end;
end;

procedure TOptionsForm.CheckBox2Change(Sender: TObject);
begin
     if activ then begin
       //Close;
       Form1.setFullScreen(CheckBox2.Checked);
       //setFocus;
       Form1.showOptions := true;
       ForceClose;
       //ShowModal;
     end;
end;

procedure TOptionsForm.CheckBox3Change(Sender: TObject);
begin
     if activ then begin
       Form1.mode800_600 := CheckBox3.Checked;
       if Form1.fullScreen then begin
         if Form1.mode800_600 and (Screen.Width<>FS_SCR_WIDTH) then begin
           Form1.setFullScreen;
           //if (Screen.Width=800) then Close;
         end else begin
           Form1.setFullScreen(false);
           Form1.setFullScreen(true);
         end;
         Form1.showOptions := true;
       	 (*Close;*) ForceClose; // see below...
       end;
     end;
end;

procedure TOptionsForm.CheckBox4Change(Sender: TObject);
begin
     Form1.game.usePNG := CheckBox4.Checked;
end;

procedure TOptionsForm.CheckBox5Change(Sender: TObject);
begin
     playSounds := CheckBox5.Checked;
end;

procedure TOptionsForm.CheckBox6Change(Sender: TObject);
begin
     computePolygonAtEachFrame := CheckBox6.Checked;
     ForceClose;
     Form1.restart;
end;

procedure TOptionsForm.CheckBox7Change(Sender: TObject);
begin
     if activ then begin
     	playMusic := CheckBox7.Checked;
     	if playMusic then music.play else music.stop;
     end;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
begin
     activ := false;
     CheckBox1.Checked := Form1.consoleShown;
     CheckBox2.Checked := Form1.fullScreen;
     CheckBox3.Checked := Screen.Width=FS_SCR_WIDTH;
     CheckBox4.Checked := Form1.game.usePNG;
     CheckBox5.Checked := playSounds;
     CheckBox6.Checked := computePolygonAtEachFrame;
     CheckBox7.Checked := playMusic;
     Top := round((Screen.Height-Height)/2);
     Left := round((Screen.Width-Width)/2);
     setFocus;
     activ := true;
end;

// THE FOLLOWING CODE IS HERE BECAUSE FORMS HAVE TROUBLES
// CLOSING WHEN SYSTEM PERFORMANCES ARE LOW:
procedure TOptionsForm.ForceClose;
begin
	 Form1.stopped := true; Close
end;
procedure TOptionsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
     Form1.stopped := true;
     // ^ seems to have no effect; so I've put it in btn_closeClick
end;

procedure TOptionsForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     Form1.stopped := false;
end;
procedure TOptionsForm.btn_closeClick(Sender: TObject);
begin
     ForceClose
end;
// END //

initialization
  {$I uOptionsForm.lrs}

end.

