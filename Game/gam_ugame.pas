(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
        Jordan Vincent
        Pierre *CURIS*
    
    Abstract:
    	Here is the main game implementation unit
        The 'cGame' class extends 'cDisplayer' for it's where is handled user
        interactions and stuff like that
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uGame;

{$mode objfpc}{$H+}

interface

uses
FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  Classes, SysUtils, dos,
  ppe_uDisplayer,
  ppe_uparticle,
  ppe_uMaths,
  ppe_uengine,
  ppe_ulink,
  ppe_uTrace,
  gam_uParamGrabber,
  gam_uGraphic,
  gam_uSoundManager,
  gam_uGameParticle,
  gam_uStupidParticle,
  gam_uLiquidParticle,
  gam_uIceParticle,
  gam_uVaporParticle,
  gam_uBonus,
  gam_uElementLinks,
  gam_uCloud,
  gam_uScore
;

const version = 'v0.15';
const heightToWin = 5000; //4600;

type tPartsArray = record
      nb : integer;
     arr : array[1..100] of cGameParticle;
end;

type TarrGraph = array[1..1000] of cGraphic;

type cGame = Class(cDisplayer)

 Width,Height,state,points:integer;
 
 addParts,
 autoScroll,
 paused,
 spawnableParticles,
 needConnection,
 usePNG
 :Boolean;
 
 currentElement:String;
 
 constructor create(_engine:cEngine); override;
 function mouseDown(x,y:integer):cParticle; override;
 procedure mouseUp(x,y:integer); override;
 procedure mouseMove(x,y:integer); override;
 procedure refresh(ref_id:integer=1); override;
 function getBitmap(_width,_height:integer):tBitmap; override;
 procedure changeElement;
 destructor destroy(); override;
 
 protected
 nbCenter, nbSides,nbnoPNG, maxAddingPartNb, minAddingPartNb:integer;
 linkType:tClass;
 lastMaxHeight,maxAddingPartRadius:real;
 Score:cScore;
 
 function removeGamePart(p:cParticle):boolean;
 function partsFinder(x,y,rmax:real; minNumber, maxNumber:integer; ignoreVapor:Boolean=true):tPartsArray;
 function onScreen(img:TGraphic; x,y:real):boolean;
 procedure display( img:TGraphic; x,y:real; canvas:tCanvas);
 procedure randomClouds(h:integer);
 procedure randomBonus(h:integer);
 function getMaxHeight():real; // altitude of the highest particle
 procedure cursor(canv:tcanvas); // draw the current altitude
 procedure checkstate;
end;

var interParticleCollisions:Boolean; game:cGame;

implementation

const ELT_LIQ = 'Liquid Water'; ELT_ICE = 'Ice'; ELT_VAP = 'Vapor';

var imagesLoaded:Boolean;
    centerGraphics: array[1..1000] of cGraphic;
    noPNGGraphics: array[1..1000] of cGraphic;
    sidesGraphics: array[1..1000] of cGraphic;
    Score_vap:tgraphic; Score_liq:tgraphic; Score_sol:tgraphic;

constructor cGame.create(_engine:cEngine); var i,j,k:integer; p:cGameParticle;
var a,b,c,d,time,lastTime:word;
begin
    Randomize;
	inherited;
    game := self;
    
	noClear 	   	   := true;
    drawLinksFirst 	   := true;
    paused 			   := false;
	addParts 		   := true;
    autoScroll 		   := true;
	omnipotentMouse    := false;
    spawnableParticles := false;
    clickableParticles := false;
    needConnection	   := true;
    usePNG			   := true;
    //interParticleCollisions := false;

    Score := cScore.create;

	Width := 800;
    Height := 600;

    state:=0;
    
    currentElement := ELT_ICE; changeElement;
    lastMaxHeight := 55;
    linkType := cLiquidLink;
    
    // 160 -> 600
    for i:=0 to 6 do begin
        p:=cGameParticle.Create(180+i*70,engine.bottomGround+50,0);
        p.fixed := true; p.connected := true;
        engine.addParticle(p);
    end;
    
    randomClouds(3800);
    randomBonus(3200);
    
    nbCenter:=28;
    nbsides:=11;
    nbnoPNG:=14;
	
    if not imagesLoaded then begin
      trace('Loading background images');
      a:=0;b:=0;c:=0;d:=0; getTime(a,b,c,d);
      lastTime := (((a*60+b)*60+c)*100)+d;
      
	    centerGraphics[1]:= cGraphic.create(-50,123,5,1.2,'img/hill1.png');
        centerGraphics[2]:= cGraphic.create(0,0,0,0,'img/bottom-center.bmp');
        centerGraphics[3]:= cGraphic.create(-115,236,4,2.4,'img/center21.png');       // Astuce de la mort qui tue: décomposer l'image en 5!
        centerGraphics[4]:= cGraphic.create(-115,251,4,2.4,'img/center22.png');       // Ou comment passer de 25 à 42 fps ! La classe, non ?
        centerGraphics[5]:= cGraphic.create(-32,500,4,2.4,'img/center23.png');
        centerGraphics[6]:= cGraphic.create(846,236,4,2.4,'img/center24.png');
        centerGraphics[7]:= cGraphic.create(-32,251,4,2.4,'img/center25.bmp');
        centerGraphics[8]:= cGraphic.create(-234,385,3,3.4,'img/center31.bmp');
        centerGraphics[9]:= cGraphic.create(-400,385,3,3.4,'img/center32.png');
        centerGraphics[10]:= cGraphic.create(-234,776,3,3.4,'img/center33.png');
        centerGraphics[11]:= cGraphic.create(-1043,385,3,3.4,'img/center34.png');
        centerGraphics[12]:= cGraphic.create(0,761,2,4,'img/cliff-center11.bmp');
        centerGraphics[13]:= cGraphic.create(0,1111,2,4,'img/cliff-center12.bmp');
        centerGraphics[14]:= cGraphic.create(0,1461,2,4,'img/cliff-center2.png');            // layer: O(background)---> 100(front)
        centerGraphics[15]:= cGraphic.create(-45,1350,1,4.6,'img/center-mountain1.bmp');     // scrolling: 5(background)---> 0(front)
        centerGraphics[16]:= cGraphic.create(-374,1350,1,4.6,'img/center-mountain2.png');    // I know it's not logical but that's life
        centerGraphics[17]:= cGraphic.create(-45,1770,1,4.6,'img/center-mountain3.png');
        centerGraphics[18]:= cGraphic.create(861,1350,1,4.6,'img/center-mountain4.png');
        centerGraphics[19]:= cGraphic.create(0,1470,0,5,'img/sky11.bmp');
        centerGraphics[20]:= cGraphic.create(0,1870,0,5,'img/sky12.bmp');
        centerGraphics[21]:= cGraphic.create(0,2270,0,5,'img/sky21.bmp');
        centerGraphics[22]:= cGraphic.create(0,2670,0,5,'img/sky22.bmp');
        centerGraphics[23]:= cGraphic.create(0,3070,0,5,'img/sky31.bmp');
        centerGraphics[24]:= cGraphic.create(0,3470,0,5,'img/sky32.bmp');
        centerGraphics[25]:= cGraphic.create(0,3870,0,5,'img/sky33.bmp');
        centerGraphics[26]:= cGraphic.create(0,4070,0,5,'img/space1.bmp');
        centerGraphics[27]:= cGraphic.create(0,4470,0,5,'img/space2.bmp');
        centerGraphics[28]:= cGraphic.create(0,4870,0,5,'img/space3.bmp');

        sidesGraphics[1]:= cGraphic.create(800,0,0,0,'img/bottom.bmp');
        sidesGraphics[2]:= cGraphic.create(800,123,6,1.2,'img/hill1.png');
        sidesGraphics[3]:= cGraphic.create(800,220,4,2.4,'img/side21.png');
        sidesGraphics[4]:= cGraphic.create(800,245,4,2.4,'img/side22.bmp');
        sidesGraphics[5]:= cGraphic.create(800,480,4,2.4,'img/side23.png');
        sidesGraphics[6]:= cGraphic.create(800,373,3,3.4,'img/side31.bmp'); // Petite astuce : décomposer l'image en 2 (une en bmp l'autre en png)
        sidesGraphics[7]:= cGraphic.create(800,763,3,3.4,'img/side32.png');
        sidesGraphics[8]:= cGraphic.create(800,761,2,4,'img/cliff-side11.bmp');
        sidesGraphics[9]:= cGraphic.create(800,1111,2,4,'img/cliff-side13.bmp');
        sidesGraphics[10]:= cGraphic.create(800,1478,2,4,'img/cliff-side21.png');
        sidesGraphics[11]:= cGraphic.create(800,0,0,0,'img/noPNG6.bmp');

        noPNGGraphics[1]:= cGraphic.create(0,0,0,0,'img/noPNG11.bmp');
        noPNGGraphics[2]:= cGraphic.create(0,100,0,5,'img/noPNG12.bmp');
        noPNGGraphics[3]:= cGraphic.create(0,500,0,5,'img/noPNG2.bmp');
        noPNGGraphics[4]:= cGraphic.create(0,1000,0,5,'img/noPNG3.bmp');
        noPNGGraphics[5]:= cGraphic.create(0,1500,0,5,'img/noPNG4.bmp');
        noPNGGraphics[6]:= cGraphic.create(0,2000,0,5,'img/noPNG5.bmp');
        //noPNGGraphics[5]:= centerGraphics[19];
        //noPNGGraphics[5]:= centerGraphics[20];
        noPNGGraphics[7]:= centerGraphics[21];
        noPNGGraphics[8]:= centerGraphics[22];
        noPNGGraphics[9]:= centerGraphics[23];
        noPNGGraphics[10]:= centerGraphics[24];
        noPNGGraphics[11]:= centerGraphics[25];
        noPNGGraphics[12]:= centerGraphics[26];
        noPNGGraphics[13]:= centerGraphics[27];
        noPNGGraphics[14]:= centerGraphics[28];

        Score_vap := TPortableNetworkGraphic.create;
        Score_vap.loadFromFile ('img/score_vap.png');
        score_liq := TPortableNetworkGraphic.create;
        score_liq.loadFromFile ('img/score_liq.png');
        score_sol := TPortableNetworkGraphic.create;
        score_sol.loadFromFile ('img/score_sol.png');
        
        getTime(a,b,c,d);
      	time := (((a*60+b)*60+c)*100)+d;
        trace('It took '+floatToStr((time-lastTime)/100)+' seconds');
        
        //if optimizeImages then
        if getBool('optimizeImages') then
        begin
          trace('Optimizing images...');
          lastTime := time;
          displayBitmap.Width:=0;displayBitmap.Height:=0;// << useless: it doesn't seem to make it faster
          for i:= 1 to nbCenter do displayBitmap.Canvas.Draw(0,0,centerGraphics[i].image);
    	  for i:= 1 to nbsides do displayBitmap.Canvas.Draw(0,0,sidesGraphics[i].image);
    	  for i:= 1 to nbnoPNG do displayBitmap.Canvas.Draw(0,0,noPNGGraphics[i].image);
          getTime(a,b,c,d);
      	  time := (((a*60+b)*60+c)*100)+d;
          trace('It took '+floatToStr((time-lastTime)/100)+' seconds');
		end;
        
      imagesLoaded := true
    end;
end;

destructor cGame.destroy();	// var i:integer;
begin
    trace('Destroying the game!');
      // No need to free the images when the game is
      // destroyed because it just means it will be restarted !!
    (*for i:=1 to nbCenter do centerGraphics[i].Free;
    for i:=1 to nbsides do sidesGraphics[i].Free;*)
    Score.Destroy;
    nbGamPts := 0;
    inherited;
end;
																		// v at first it was false by def, but after all why not always true?
function cGame.partsFinder(x,y,rmax:real; minNumber, maxNumber:integer; ignoreVapor:Boolean=true):tPartsArray;
var i,j,k:integer; cased:Boolean; p,nearestConPart:cGameParticle; dist,nearestConPartDist:real;
begin
     partsFinder.nb := 0;
     nearestConPart := NIL; nearestConPartDist := rmax*10;
     //for i := 1 to engine.nbPts do
     for i := 1 to nbGamPts do
     begin
     	  p := gameParts[i]; dist := distance(p.x,p.y,x,y);
          //if (p as cGameParticle<>NIL) and (distance(p.x,p.y,x,y)<= rmax) then //and (distance(p2.x,p2.y,x,y)>= p2.size/2) then		<- useless
          if (dist <= rmax) and (not ignoreVapor or not (p.partType=ptVapor)) then //and (distance(p2.x,p2.y,x,y)>= p2.size/2) then		<- useless
          begin
            if p.connected or not needConnection and (nearestConPartDist>dist) then
               begin nearestConPart := p; nearestConPartDist := dist end;
          	cased := false;
           	for j:=1 to partsFinder.nb do
               if distance(partsFinder.arr[j].x,partsFinder.arr[j].y,x,y) >= dist then
               begin
                  cased := true;
                  if partsFinder.nb < maxNumber then partsFinder.nb += 1;
                  for k:=partsFinder.nb-1 downto j do partsFinder.arr[k+1] := partsFinder.arr[k];
                  partsFinder.arr[j] := p;
                  break;
               end;
           	if not cased and (partsFinder.nb < maxNumber) then
           	begin
             	partsFinder.nb += 1;
             	partsFinder.arr[partsFinder.nb] := p;
           	end;
          end
     end;
     if (partsFinder.nb=1) and (partsFinder.arr[1].partType=ptVapor) then
        partsFinder := partsFinder(x,y,rmax,minNumber,maxNumber,true)
     else if (nearestConPart=NIL) or (partsFinder.nb<minNumber) then partsFinder.nb := 0
     else for j:=1 to partsFinder.nb do
          	  if partsFinder.arr[j].connected then break
          	  else if j=partsFinder.nb then partsFinder.arr[j] := nearestConPart;
end;

function cGame.onScreen(img:TGraphic; x,y:real):boolean;
begin
onScreen:=false;
if ((-cameraY-Height)<= (y))
  and ((-cameraY+Height)>= (y- img.height))
  and ((cameraX+width)>= (x))
  and ((cameraX)<= (x+ img.width))  then onScreen:=true;
end;

procedure cGame.display( img:TGraphic; x,y:real; canvas:tCanvas);
begin
if onScreen(img,x,y) then
    canvas.draw(round(-cameraX+x),round(-y-cameraY+height-img.height),img);
end;

procedure cGame.refresh(ref_id:integer=1); const margin = 120; sensitivity = 0.16; //const margin = 160; sensitivity = 0.1; //sensitivity = 0.15; //const margin = 100; sensitivity = 0.4;
var i:integer; p:cGameParticle; _motionSpeed:real;
begin if not paused then begin inherited;
	_motionSpeed := engine.motionSpeed/engine.refreshRate;
	for i := 1 to nbGamPts do
	begin
		p := gameParts[i];
		p.enterFrame(_motionSpeed);
	end;
	
	gam_uLiquidParticle.screamingSoundTimer -= _motionSpeed; //trace(screamingSoundTimer);
	
	if ref_id=1 then begin
     if autoScroll (*and (mouseRealLastY > margin/2)*) then begin
     	if mouseRealLastX > Width-margin then cameraX := cameraX + (mouseRealLastX-Width+margin)*sensitivity;
     	if mouseRealLastX < margin then cameraX := cameraX + (mouseRealLastX-margin)*sensitivity;
     	if mouseRealLastY > Height-margin then cameraY := cameraY + (mouseRealLastY-Height+margin)*sensitivity;
     	if mouseRealLastY < margin then cameraY := cameraY + (mouseRealLastY-margin)*sensitivity;
     end;
	 if cameraY > 0 then cameraY := 0;
	end;
    points := score.points;
    checkstate;
end;
end;

procedure cGame.checkstate; var m:real;
begin
  m:=getMaxHeight;
  if (m+350 <= score.maxheight) then state:=1
  else if ((score.maxSol-score.nbSol=0)
    and (score.maxVap-score.nbVap=0)
    and (score.maxLiq-score.nbLiq=0))
    then state:=-1;
  if m>=heightToWin then state:=2;
end;

procedure cGame.randomClouds(h:integer);
const nbc = 17 {15}; var i:integer;
begin
     for i:=1 to nbc do
        cCloud.create(-1000+random(2000),-random(h),engine);
end;

procedure cGame.randomBonus(h:integer);
const nbb = 15; var i:integer;
begin
     for i:=1 to nbb do
        cBonus.create(-1000+random(2000),-random(h),engine,Random(3)+1);
end;


function cGame.getBitmap(_width,_height:integer):tBitmap;
var p:cparticle; i,j,k,l:integer; canv:tCanvas; parts:tPartsArray; ix:real;
begin
	canv := displayBitmap.canvas;
    canv.Brush.Color := clBlack;
	canv.Clear;
	(*canv.draw(round(-cameraX),round(Height-backGroundBmp.Height-cameraY),backGroundBmp);
        //canv.draw(0,250,hill);*)
	
    for k:=0 to 6 do
   begin
         for j:=1 to nbsides do
             if ((sidesGraphics[j].layer=k) and (usePNG {or not sidesGraphics[j].isPNG})) or ((j=11) and (not usePNG)) then
             begin
                  l:=trunc((CameraX-sidesGraphics[j].x-0.2*CameraX*sidesGraphics[j].scrolling)/sidesGraphics[j].image.width);
                  ix:= sidesGraphics[j].x+0.2*CameraX*sidesGraphics[j].scrolling+l*sidesGraphics[j].image.width;
                  while onScreen(sidesGraphics[j].image,ix,sidesGraphics[j].y) do
                        begin
                             if ix>= width then display( sidesGraphics[j].image, ix, sidesGraphics[j].y , canv);
                             ix:=ix+sidesGraphics[j].image.width;
                        end;

                  l:= trunc((-CameraX-sidesGraphics[j].x+0.2*CameraX*sidesGraphics[j].scrolling)/sidesGraphics[j].image.width);
                  ix:= -sidesGraphics[j].x+width-l*sidesGraphics[j].image.width-sidesGraphics[j].image.width+0.2*CameraX*sidesGraphics[j].scrolling;
                  while onScreen(sidesGraphics[j].image,ix,sidesGraphics[j].y) do
                        begin
                             if ix<= -sidesGraphics[j].image.width then display( sidesGraphics[j].image, ix, sidesGraphics[j].y , canv);
                             ix:=ix-sidesGraphics[j].image.width;
                        end;
             end;
             for j:=1 to nbcenter do
             if (centerGraphics[j].layer=k) and (usePNG {or not centerGraphics[j].isPNG})
                then display( centerGraphics[j].image, centerGraphics[j].x+0.2*CameraX*centerGraphics[j].scrolling, centerGraphics[j].y , canv);
    end;
    if not usePNG then for j:=1 to nbnoPNG do
    display( noPNGGraphics[j].image, noPNGGraphics[j].x+0.2*CameraX*noPNGGraphics[j].scrolling, noPNGGraphics[j].y , canv);
    
	getBitmap := inherited;
    
	if addParts and (draggingParticle=NIL) then
	begin
		parts := partsFinder(mouseLastX,mouseLastY,maxAddingPartRadius,minAddingPartNb,maxAddingPartNb);
		
		canv.pen.color:= clred; canv.Pen.Style := psDot; //canv.pen.Width := 3;
          canv.Brush.Style := bsClear;
		for i:= 1 to parts.nb do
        	begin
        		p := parts.arr[i];
          	canv.line(round(mouseLastX-cameraX),round(mouseLastY-cameraY),round(p.x-cameraX),round(p.y-cameraY));
        	end;
        	canv.pen.Style := psSolid; canv.pen.Width := 1;
          canv.Brush.Style := bsSolid;
	end;
    cursor(canv); // gives altitude

    if currentElement=ELT_ICE then canv.draw(700,10,score_sol)
    else if currentElement=ELT_LIQ then canv.draw(700,10,score_liq)
    else if currentElement=ELT_VAP then canv.draw(700,10,score_vap);
    Score.Display(engine, canv);
    {canv.Brush.Color := clBlue;
            Canv.Font.Size := 13; Canv.Font.Color := clWhite; Canv.Font.Orientation := 30;
            Canv.TextOut(10,15,Concat('> Selected element: ',currentElement,' <')); }

end;

function cGame.getMaxHeight:real;
begin
    getMaxHeight := Score.getMaxHeight
end;

procedure cGame.cursor(canv:tcanvas); const speedCoeff = 0.1;
var text:string; gmh,maxHeight:real;
begin
     gmh := getMaxHeight+5;
     maxHeight := lastMaxHeight+(gmh-lastMaxHeight)*speedCoeff;
     canv.pen.color:= clwhite;
     canv.brush.color:= clwhite;
     //text:= Concat(' ',floatToStr(round(maxHeight-55)/100),'m');
     text:= Concat(' ',floatToStr(round(maxHeight-55)));
     canv.line(round(0),round(-maxHeight-cameraY+height),round(200),round(-maxHeight-cameraY+height));
     canv.pen.color:= clblack;
     canv.TextOut(0,round(-maxHeight-cameraY+height),text);
     lastMaxHeight := maxHeight;
end;

procedure cGame.changeElement;
begin
    (*Case currentElement of	// IMPOSSIBLE: 'Ordinal Required'... -_-'
    	...
    end;*)
    if currentElement=ELT_ICE then begin
        currentElement := ELT_LIQ;
        maxAddingPartNb := 3;
        minAddingPartNb := 2;
        linkType := cLiquidLink;
        maxAddingPartRadius := 90;
    end else if currentElement=ELT_LIQ then begin
    	currentElement := ELT_VAP;
    	maxAddingPartNb := 1;
        minAddingPartNb := 1;
    	linkType := cVaporLink;
        maxAddingPartRadius := 110;
    end else begin
        currentElement := ELT_ICE;
        maxAddingPartNb := 4;
        minAddingPartNb := 2;
        linkType := cIceLink;
        maxAddingPartRadius := 120;
    end;
end;

function cGame.mouseDown(x,y:integer):cParticle;
const size = 25; ela = 0.1; res = -1; //res = 100;
var i:integer; p,newP:cGameParticle; l:cLink; _x,_y:real; parts:tPartsArray; b:cBonus;
begin
	_x := x+cameraX; _y := y+cameraY;
    
    mouseDown := inherited;
	if (mouseDown = NIL) or (not clickableParticles and not interParticleCollisions) then
	begin
      parts := partsFinder(_x,_y,maxAddingPartRadius,minAddingPartNb,maxAddingPartNb);
      if spawnableParticles or (parts.nb>0) then
      begin
           if (((currentElement=ELT_ICE) and (score.noSol()=false)) or ((currentElement=ELT_LIQ) and (score.noLiq()=false)) or ((currentElement=ELT_VAP) and (score.noVap()=false))) then
           begin
		      if (currentElement=ELT_ICE) and (score.noSol()=false) then newP := cIceParticle.create(_x,_y,size)
    	      else if (currentElement=ELT_LIQ) and (score.noLiq()=false) then newP := cLiquidParticle.create(_x,_y,size)
    	      else if (currentElement=ELT_VAP) and (score.noVap()=false) then newP := cVaporParticle.create(_x,_y,size);
		      engine.addParticle(newP); playSound('create');
		      //startDrag(newP);
              if (parts.nb>0) then newP.connected := true;
		      for i:= 1 to parts.nb do
        	      begin
        		      p := parts.arr[i];
                      p.connected := true;
                      if p.ClassNameIs('cBonus') then
                      begin
                         b := (p as cBonus);
                         b.PartBonus(gam_uScore.mainScore);
                         removeGamePart(p);
                         engine.removeParticle(p);
                      end else begin
                        l := cLink(linkType.Create); l.Create(p,newP);
        		        l.elasticity := ela; l.resistance := res;
         		        engine.addLink(l);
                      end;
        	      end
	      end else playSound('pickup');
      end;
    end;
end;

function cGame.removeGamePart(p:cParticle):boolean; var i,j:Integer;
begin
	 removeGamePart := false;
     for i := 1 to nbGamPts do if gameParts[i].id = p.id then
     begin
        nbGamPts -= 1;
        for j := i to nbGamPts do gameParts[j] := gameParts[j+1];
        removeGamePart := true;
        break;
     end;
end;

procedure cGame.mouseUp(x,y:integer);
begin
	inherited;
end;

procedure cGame.mouseMove(x,y:integer);
begin
	inherited;
end;

initialization
imagesLoaded := false;

end.

