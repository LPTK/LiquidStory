(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
        Jordan Vincent
    
    Abstract:
    	The main form of the game
        Here is handled the physics engine and the game that uses it
        The images-introduction is also implemented here
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit uMainForm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, dos,
  uFSForm,
  uOptionsForm,
  uSplashScreen,
  ppe_uEngine,
  ppe_uDisplayer,
  ppe_uParticle,
  ppe_uLink,
  ppe_uPropContainer,
  ppe_uTrace,
  gam_uParamGrabber,
  gam_uGame,
  gam_uSoundManager,
  gam_uGameParticle,
  gam_uEyedParticle,
  gam_uStupidParticle,
  gam_uLiquidParticle,
  gam_uIceParticle,
  gam_uCloud,
  gam_ubonus
  ;

const
  CR = #13;
  LF = #10;
  CRLF = CR + LF;
  //askFullScreen = true;
  FS_SCR_WIDTH = 800;
  FS_SCR_HEIGHT = 600;

  
type
  
  { TForm1 }

  TForm1 = class(TForm)
    btn_quit: TButton;
    btn_resume: TButton;
    btn_about: TButton;
    btn_restart: TButton;
    btn_options: TButton;
    btn_activConsole: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    pan_pauseMenu: TPanel;
    Timer1: TTimer;
    procedure btn_aboutClick(Sender: TObject);
    procedure btn_activConsoleClick(Sender: TObject);
    procedure btn_restartClick(Sender: TObject);
    procedure btn_resumeClick(Sender: TObject);
    procedure btn_optionsClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btn_quitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; 
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; 
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; 
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure Memo1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    
  private
    { private declarations }
    consoleWidth:integer;
    activeCanvas:tCanvas;
    firstTime:Boolean;
    procedure GameSession;
    procedure Test1();
    procedure Demo2;
  public
    { public declarations }
    
     engine:cEngine; game:cGame;
     
  	 calculatedFrames,under30FPSframes:integer;
     prevTime:word; fps:real;
     infoString:String;
     
  	 paused, stopped, finished,
     consoleShown, activated,
     fullScreen, mode800_600,
     showOptions, usePNGSave,
     askedNoPNG  								    : Boolean;
     
     rezSaveWidth, rezSaveHeight,cinema, nbimg		: integer;

     imgIntro: array[1..20] of TBitmap;
     
     procedure showConsole; procedure hideConsole;
     procedure pause; procedure unpause;
     procedure restart;
     procedure centerForm;
     procedure setFullScreen(activ:Boolean=true);
     procedure askNoPNG;
     procedure initIntro;
     procedure intro;
     procedure quitIntro;
  end; 

var
  Form1: TForm1;
  initialized:Boolean;

implementation

{ TForm1 }

procedure TForm1.GameSession();
begin
    
	game.autoScroll := not paramExists('autoScroll') or getBool('autoScroll');
    game.omnipotentMouse := getBool('omnipotentMouse');
    game.clickableParticles := getBool('clickableParticles');
    game.spawnableParticles := getBool('spawnableParticles');
    game.needConnection := not paramExists('autoScroll') or getBool('needConnection');
    //game.usePNG := getBool('usePNG');
    
    //if getBool('noIntro') then quitIntro;
    
    //cBonus.Create(100,100,engine,1);
    //cinema := -1; //music.play;
    
	//if not firstTime and playMusic then music.play;
    	// ^ Actually I chose not to restart the music when the game does
    
	//trace(music.getInfo.name);
    firstTime := false;
end;

procedure TForm1.Test1(); var p:cGameParticle; const con = true;
begin
    
    cCloud.Create(300,100,engine);
    cBonus.Create(100,100,engine,1);
    cBonus.Create(300,-200,engine,2);
    cBonus.Create(500,0,engine,3);
    cBonus.Create(400,500,engine,3);
    
	game.autoScroll := true;
    game.omnipotentMouse := false;
    //game.clickableParticles := true;
    //game.spawnableParticles := true;
    //game.needConnection := false;
    game.usePNG := false;
    
	//music.play;
	//trace(music.getInfo.name);
end;

procedure TForm1.Demo2;
var i,j,n,m:integer; p:CParticle; l:cLink; mat:array[0..100,0..100] of cParticle; len,siz,res,ela:real; pc:cPropContainer;
begin
     engine.gravity := 0.05; engine.refreshRate := 1; // 3;
     //n := 10; m := 5; len := 25; res := 15; ela := 1; //ela := 0.1;
     //n := 20; m := 4; len := 25; siz := 30; ela := 0.8; res := 20;//13;
     n := 30; m := 6; len := 25; siz := 30; ela := 0.8; res := 20;//13;
     game.mouseSmooth := 0.01; //disp.showPoints := false;
     game.addParts := false;
     
     //engineRefRate := 3; engine.motionSpeed := 1/engineRefRate; res := 13*engineRefRate;
     
     pc := cPropContainer.Create;
     pc.resistance := res;
     for i:=1 to n do begin
     	 for j:=1 to m do begin
     	     //p := cLiquidParticle.Create(100+i*len,100+j*len,siz); engine.addParticle(p); mat[i][j] := p;
             p := cIceParticle.Create(100+i*len,100+j*len,siz); engine.addParticle(p); mat[i][j] := p;
     	     //if i=1 then p.fixed := true;
     	     if i > 1 then engine.addLink(cLink.Create(p,mat[i-1][j],pc));
     	     if j > 1 then  engine.addLink(cLink.Create(p,mat[i][j-1],pc));
     	     if (i>1)and(j>1) then engine.addLink(cLink.Create(p,mat[i-1][j-1],pc));
     	     if (i>1)and(j<m) then engine.addLink(cLink.Create(p,mat[i-1][j+1],pc));
     	 end;
     end;
     engine.mixLinks;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var a,b,c,d,time:word; bmp:tBitmap; msg:String;
begin
	if initialized and not stopped then
	begin
        //if fullScreen and not Active then SetFocus;
        //if fullScreen and not Active then WindowState := wsMinimized;;
        
       	if showOptions then begin	   // doesn't work
            showOptions := false;
            //OptionsForm.ShowModal;
            //btn_optionsClick(NIL);
    	end;
        if cinema >=0 then intro else
        begin
		  if not paused then begin
              //if Screen.ActiveForm<>self then pause
          	if not Active and not fullScreen then pause
              (*if not Active then begin
                  pause;
                  if fullScreen then WindowState := wsMinimized;
              end*)  // to do this we would need to restore previous definition...
            else begin
               if not finished then
               begin
                 if (Game.state=1) or (Game.state=-1) then
                 begin
                      trace('Player lost the game');
                      stopped := true;
                      if Game.state=-1 then msg:='Your are out of particles!'
                      else msg:='Your tower collapsed!';
                      if MessageDLG('Game Over', msg+CRLF+'You lose with only '+inttostr(game.points)+' points :( ', mtConfirmation, [mbRetry,mbClose],0)=mrRetry then restart
                      //else begin stopped := true; close; end;
                      else begin finished := true; engine.motionSpeed:=0; end;
                      stopped := false;
                 end
                 else if Game.state=2 then
                      begin
                           trace('Player won the game');
                           stopped := true;
                           if MessageDLG('You win', 'You reach the space!'+CRLF+'You collected '+inttostr(game.points)+' points $_$ !', mtConfirmation, [mbRetry,mbClose],0)=mrRetry then restart
                           else begin finished := true; engine.motionSpeed:=0; end;
                  	       stopped := false;
                      end;
               end;
               if finished then game.refresh
               else engine.refresh;	// engine automatically refreshes the game
            end;
        	  //game.refresh;		 // no longer needed, the engine does it itself
		  end;
		  
		  calculatedFrames := calculatedFrames+1;
         	  if {consoleShown and} (calculatedFrames mod 4 = 0) then
         	  begin
          	      a:=0;b:=0;c:=0;d:=0; getTime(a,b,c,d);
        		  time := (((a*60+b)*60+c)*100)+d;
         		  fps := 5*100/(time-prevTime);
         		  prevTime := time;
                  
                  if (fps<30) then under30FPSframes += 1; //if (fps<30) then trace(under30FPSframes);
                  if not askedNoPNG and game.usePNG and (under30FPSframes>30) then askNoPNG;
                  //if (fps<30) then trace('aa');
                  
                  if consoleShown then
     		    infoString := Concat('PPE v.',ppe_uEngine.version,' | pts: ',intToStr(engine.nbPts),
        	 	  ' | links:',intToStr(engine.nbLks),
        	 	  ' | gravity:',floatToStr(engine.gravity),
        	 	  ' | motion:',floatToStr(engine.motionSpeed),
        	 	  ' | airFriction:',floatToStr(engine.airFriction),
        		  ' | refreshRate: ',intToStr(engine.refreshRate),
        		  ' | fps: ',intToStr(round(fps)),
        	      ' | calculated frame: ',intToStr(calculatedFrames),
        		  ' | mouse: [',intToStr(mouseRealLastX),' ',intToStr(mouseRealLastY),']'
        	 	  );
         	  end;
        	  
        	  bmp := game.getBitmap(800,600);
        	  {bmp.Canvas.Brush.Color := clBlue;
              bmp.Canvas.Font.Size := 13; bmp.Canvas.Font.Color := clWhite; bmp.Canvas.Font.Orientation := 30;
              bmp.Canvas.TextOut(10,15,Concat('> Selected element: ',game.currentElement,' <'));}
              
              bmp.Canvas.Font.Orientation := 0;
              bmp.Canvas.Brush.Color := clWhite; bmp.Canvas.Font.Size := 0; bmp.Canvas.Font.Color := clBlack;
        	  if consoleShown then bmp.Canvas.TextOut(0,game.Height-bmp.Canvas.TextHeight(infoString),infoString);
        	  
		  activeCanvas.Draw(0,0,bmp);
          end;
        end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    if cantReadParamFile then MessageDLG('Warning!', 'Unable to read file config.ini', mtWarning, [mbClose],0);
    mainForm := self;
	consoleWidth := traceMemo.Width;
    if not initialized then
    begin
        trace('Initializing interface...');
        Application.CreateForm(TSplashScreenForm, SplashScreenForm);
    	if getBool('splashScreen') then SplashScreenForm.Show
        else SplashScreenForm.Hide;
        Form1.Memo1.Clear;
        activeCanvas := Canvas;
        Width := 800; Height := 600;
    	//showConsole;
    	if not getBool('showConsole') then hideConsole;
        mode800_600 := true;
        showOptions := false;
        usePNGSave := true;
        //if getBool('askNoPNG') then askedNoPNG := false else askedNoPNG := true;
        askedNoPNG := not getBool('askNoPNG');
        //if showIntro then cinema := 1 else cinema := -1;
		gam_uGame.interParticleCollisions := getBool('interParticleCollisions');
        firstTime := true;
       	if getBool('noIntro') then cinema := -1
        else begin cinema := 1; initIntro(); end;
    end;
    Form1.Memo1.Text := traceMemo.Text;
	traceMemo := Form1.Memo1;
    
    engine := cEngine.Create;
        //engine.refreshRate := 4; engine.gravity := 0.2;
		if paramExists('refreshRate')
        	then engine.refreshRate := getInt('refreshRate')
            else engine.refreshRate := 4;
        engine.gravity := 0.2;
		engine.airFriction := 0.002;
    	engine.bottomGround := 600-50;
		engine.leftGround := -9999;	 //0;
		engine.rightGround := 9999; //800;
    
    //gam_uGame.optimizeImages := true;
	game := cGame.create(engine);
    	 game.usePNG := usePNGSave;
    
    paused := false; stopped := false; finished := false;
    if not initialized then begin
      SplashScreenForm.Hide;
      SplashScreenForm.Free;
      game.usePNG := getBool('usePNG');
    end;
	trace('Initializing the game...');
	initialized := true;
	//Test1;
    GameSession;
    activated := false;
end;

procedure TForm1.centerForm;
begin
    Top := round((Screen.Height-Height)/2);
    Left := round((Screen.Width-Width)/2);
end;

procedure TForm1.setFullScreen(activ:Boolean=true);
begin
     fullScreen := activ;
     if fullScreen then
     begin
         if activated then begin
            trace('Enabling fullScreen mode');
         	rezSaveWidth := Screen.Width; rezSaveHeight := Screen.Height;
            //if askFullScreen and mode800_600 then begin
            if getBool('ask800_600') and mode800_600 then begin
         	    stopped := true;
         	    with application do begin
     	 	        if MessageDlg ('Change resolution?',
                    'Going into 800*600 mode offers a greater game experience.'+CRLF
                    //+'but your icons and currently opened windows may be reorganized.'+CRLF
                    +'Your icons and currently opened windows won''t be reorganized.'+CRLF
                    +'Do you wish to use this resolution?',
                    mtConfirmation,
                    [mbYes, mbNo],0) = mrYes
   		 	        then begin SetScreenResolution(FS_SCR_WIDTH,FS_SCR_HEIGHT); hideConsole; end;
       	 	    end;
         	    stopped := false;
            end else if mode800_600 then begin SetScreenResolution(FS_SCR_WIDTH,FS_SCR_HEIGHT); hideConsole; end;
         end;
         FullScreenForm.Show;
         FormStyle := fsSystemStayOnTop;	//FormStyle := fsNormal;
         centerForm;
     	 //Width := Screen.Width; Height := Screen.Height;
         BorderStyle := bsNone;
         //ShowModal;
     end
     else begin
         if activated then trace('Disabling fullScreen mode');
         //FormStyle := fsSystemStayOnTop;
         FullScreenForm.Hide;
         FormStyle := fsNormal;
         BorderStyle := bsSingle;
         if activated and (rezSaveWidth<>Screen.Width) or (rezSaveHeight<>Screen.Height)
         	then SetScreenResolution(rezSaveWidth, rezSaveHeight);
         centerForm;
     end;
end;

procedure TForm1.askNoPNG;
begin
     stopped := true;
     with application do begin
     	if MessageDlg ('Change display settings?',
        'It seems your computer is too slow for these display settings.'+CRLF
        +'Indeed, you are under 30 fps. For a better game experience, you should disable background PNGs.'+CRLF
        +'Do you wish to disable PNGs now?',
        mtConfirmation,
        [mbYes, mbNo],0) = mrYes
   		then begin game.usePNG := false; end;
     end;
     askedNoPNG := true;
     stopped := false;
end;

procedure TForm1.restart;
begin
	if paused then unpause;
    usePNGSave := game.usePNG;
    engine.Free;
    game.Destroy;
    FormCreate(NIL);
end;

procedure TForm1.pause;
begin
    if not paused then begin
        pan_pauseMenu.Visible := true;
        trace('Game paused');
    end;
    paused := true;
end;

procedure TForm1.unpause;
begin
    if paused then begin
        pan_pauseMenu.Visible := false;
        trace('Game unpaused');
    end;
    //Memo1.Enabled := false;
    paused := false;
end;

procedure TForm1.showConsole;
begin
    //if consoleShown then begin end;
    //Width := 948;
    if Screen.Width<>FS_SCR_WIDTH then Width := 800+consoleWidth;
    Memo1.Visible := true;
    if Screen.Width<>FS_SCR_WIDTH then Left := Left - Round(consoleWidth/2);
    trace('Console shown');
    consoleShown := true;
end;
procedure TForm1.hideConsole;
begin
    //if not consoleShown then begin end;
    Width := 800;
    Memo1.Visible := false;
    if Screen.Width<>800 then Left := Left + Round(consoleWidth/2);
    trace('Console hidden');
    consoleShown := false;
end;


procedure TForm1.initIntro();
var i:integer;
begin
	nbimg:=8;
    for i:=1 to nbimg do begin
        imgIntro[i]:= TBitmap.create;
    	imgIntro[i].loadfromfile('img/intro'+intToStr(i)+'.bmp');
    end;
end;

procedure TForm1.intro;
begin
  activecanvas.draw(1,1,imgIntro[cinema]);
end;

procedure TForm1.quitIntro;
begin
  cinema:=-1;
  if playMusic then music.play;
end;


procedure TForm1.btn_aboutClick(Sender: TObject); //var i:integer;
begin
	stopped := true;
	 MessageDlg ('About...',
     'Liquid Story'+CRLF
     +'All rights reserved.'+CRLF+CRLF
     +'Credits:'+CRLF
     +'Lionel Parreaux - Lead Programmer, artist, coordinator'+CRLF
     +'Jordan Vincent - Lead Artist, programmer'+CRLF
     +'Pierre *CURIS* - programmer'+CRLF
     +'Najoua El-Ouadie - programmer'+CRLF
     +'Music by Zodiak (Erik Stridell, 1993)'+CRLF
     +'Sounds and music played using SoundLib3 (www.crossfire-designs.de)'
     , mtInformation, [mbOk],0);
    stopped := false;
end;

procedure TForm1.btn_activConsoleClick(Sender: TObject);
begin
  	if Memo1.Enabled then begin
    	Memo1.SetFocus;
        Memo1.Enabled := false;
        btn_activConsole.Caption := 'Enable';
  	end else begin
      	Memo1.Enabled := true;
    	Memo1.SetFocus;
      	btn_activConsole.Caption := 'Disable';
  	end;
end;

procedure TForm1.btn_restartClick(Sender: TObject);
begin restart; end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
    trace(Concat('Key [',intToStr(integer(Key)),'] pressed (',key,')'));
    Case integer(Key) of
         8:if consoleShown then hideConsole else showConsole;
         13:if cinema>=0 then quitIntro else setFullScreen(not fullScreen);
    	 27:if cinema>=0 then quitIntro else if paused then unpause else pause;
    	 178:if consoleShown then hideConsole else showConsole;
    end;

end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    //trace('K');
end;

procedure TForm1.btn_resumeClick(Sender: TObject);
begin
    unpause;
end;

procedure TForm1.btn_optionsClick(Sender: TObject); var fsSave:TFormStyle;
begin
    //if fullScreen then setFullScreen(false);
    fsSave := FormStyle;
    FormStyle := fsNormal;
    OptionsForm.ShowModal;
    FormStyle := fsSave;
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
    if not activated then begin
       //sleep(500);
       centerForm;
       if getBool('fullScreen') then
       begin
         setFullScreen;
         setFullScreen(false);
         activated := true;
         setFullScreen;
       end;
    end;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
    setFocus;
end;

procedure TForm1.btn_quitClick(Sender: TObject);
begin
	stopped := true;
	//playsound('snd\bye.wav',0,0);
	Close;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    stopped := true;
    setFullScreen(false);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; 
  Shift: TShiftState; X, Y: Integer);
begin
    //trace(engine.nbPts); //trace(nbGamPts);
    //if not paused and (Button=mbRight) then game.changeElement
	//else if not paused then game.mouseDown(x,y);
    if (cinema>=0) and (cinema <nbimg) then cinema:=cinema+1
    else if cinema>=nbimg then quitIntro
    else begin
      if not paused then
         Case Button of
    	   mbRight:game.changeElement;
           mbMiddle:pause;
      	   else if not finished then game.mouseDown(x,y);
      	 end;
    end;
    //trace(engine.nbPts); //trace(nbGamPts);
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; 
  Shift: TShiftState; X, Y: Integer);
begin
	if not paused then game.mouseUp(x,y);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; 
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	if not paused then game.cameraY -= WheelDelta; //trace(WheelDelta);
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin	  							// Doesn't work: can't be clicked while disabled!
    pause; Memo1.Enabled := true;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, 
  Y: Integer);
begin
	if not paused then game.mouseMove(x,y);
end;

initialization
  {$I umainform.lrs}

initialized := false;

end.

