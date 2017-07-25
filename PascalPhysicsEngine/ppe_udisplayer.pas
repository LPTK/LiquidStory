(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	cDisplayer is used both to yield a bitmap of the simulation and to
        handle user interactions like clicks and particle dragging
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uDisplayer;

{$mode objfpc}{$H+}

interface

uses
  FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  Classes, SysUtils, ppe_uEngine, ppe_uparticle, ppe_uGround, ppe_umaths, ppe_uTrace;

const mousePower = 0.5; 

type cDisplayer = Class
     public
     engine:cEngine;
     showPoints,showLinks,drawLinksFirst,clickableParticles:Boolean;
     draggingParticle:cParticle;
     mouseSpeedX, mouseSpeedY:real; mouseSmooth:real;
     omnipotentMouse:Boolean; // <- If the mouse can bring any particle anywhere
     						  // or if it can't overcome the constraints
     cameraX,cameraY:real;
     
     constructor create(_engine:cEngine); virtual;
     procedure refresh(ref_id:integer); virtual;
     procedure drawBitmap(bmp:tBitmap); virtual;
     function getBitmap(width,height:integer):tBitmap; virtual;
     function mouseDown(x,y:integer):cParticle; virtual;
     procedure mouseUp(x,y:integer); virtual;
     procedure mouseMove(x,y:integer); virtual;
     procedure updateDragging(x,y:real);
     procedure startDrag(p:cParticle);
     procedure stopDrag();

     protected
     fixedSave:Boolean;
     displayBitmap:tBitmap;
     noClear:Boolean;
     procedure drawLinks(canv:tCanvas);
end;

var mouseLastX, mouseLastY:real; mouseRealLastX, mouseRealLastY:integer;

implementation

constructor cDisplayer.create(_engine:cEngine);
begin
     engine := _engine;
     engine.activeDisplayer := self;
     
     showPoints := true;
     showLinks := true;
     drawLinksFirst := false;
     clickableParticles := true;
     
     draggingParticle := NIL; mouseSmooth := 1;
     mouseLastX := 0; mouseLastX := 0; mouseSpeedX := 0; mouseSpeedY := 0;
     displayBitmap := tBitmap.Create;
     cameraX := 0; cameraY := 0;
	 omnipotentMouse := true;
     //partBmp := tBitmap.Create; partBmp.LoadFromFile('stupid_part.bmp'); // pour le souvenir !
     noClear := false;
end;

procedure cDisplayer.drawLinks(canv:tCanvas); var i:integer; p1,p2:cParticle;
begin
	for i := 1 to engine.nbLks do if showLinks and engine.links[i].isVisible and (not engine.links[i].broken) then begin
         p1 := engine.links[i].p1; p2 := engine.links[i].p2;
         engine.links[i].display(p1.x-cameraX,p1.y-cameraY,p2.x-cameraX,p2.y-cameraY, canv);
     end;
end;

procedure cDisplayer.refresh(ref_id:integer);
begin
	//updateDragging(mouseLastX,mouseLastY);
    updateDragging(mouseRealLastX+cameraX,mouseRealLastY+cameraY);
end;

procedure cDisplayer.drawBitmap(bmp:tBitmap);
var i:integer; p:cParticle; size,x,y:real; canv:tCanvas;
begin
     canv := bmp.Canvas;
     
     {for i := 1 to engine.nbGnds do begin
     	g := engine.grounds[i];
     	canv.line(round(g.p1.x-cameraX),round(g.p1.y-cameraY),round(g.p2.x-cameraX),round(g.p2.y-cameraY));
     end;}
     
     //if showPoints then for i := 1 to engine.nbPts do engine.parts[i].display(p.x,p.y,canv); <<-- CRASHES!!(??)
     
     if drawLinksFirst then drawLinks(canv);
     
     if showPoints then for i := 1 to engine.nbPts do begin
     	p := engine.parts[i]; size := p.size; x := p.x-cameraX; y := p.y-cameraY;
     	if (x>-size) and (x<canv.width+size) and (y>-size) and (y<canv.height+size) then
     		p.display(x,y,canv)
     end;
     
     if not drawLinksFirst then drawLinks(canv);
     
end;

function cDisplayer.getBitmap(width,height:integer):tBitmap;
begin
     displayBitmap.Width := width; displayBitmap.Height := height;
     displayBitmap.canvas.Brush.Color := clWhite;
     if not noClear then displayBitmap.canvas.Clear;
     drawBitmap(displayBitmap);
     getBitmap := displayBitmap;
end;

         
function cDisplayer.mouseDown(x,y:integer):cParticle; var _x,_y:real;
var i:integer; p:cParticle; d:real;
begin
	 _x := x+cameraX; _y := y+cameraY;
     mouseDown := NIL;
     for i := engine.nbPts downto 1 do
     if engine.parts[i].draggable then begin
         p := engine.parts[i];
         d := distance(p.x,p.y,_x,_y);
         if d < p.size/2 then begin
         	mouseDown := p;
            if clickableParticles then startDrag(p);
            break;
         end;
     end;
end;                               

procedure cDisplayer.mouseUp(x,y:integer); var r:real;
begin
	 if draggingParticle <> NIL then begin
     	if not fixedSave then begin
           	if engine.motionSpeed = 0 then r := 1 else r := 1/engine.motionSpeed;
             	draggingParticle.sx := mouseSpeedX*mousePower*r;
            	draggingParticle.sy := mouseSpeedY*mousePower*r;
      	end;
     	stopDrag();
     end;
end;

procedure cDisplayer.mouseMove(x,y:integer); var _x,_y:real;
begin
	 _x := x+cameraX; _y := y+cameraY;
     updateDragging(_x,_y);
     mouseRealLastX := x; mouseRealLastY := y; 
end;

procedure cDisplayer.startDrag(p:cParticle);
begin
     draggingParticle := p;
     //p.sx := 0; p.sy := 0;
     fixedSave := p.fixed; if omnipotentMouse then p.fixed := true;
     trace(Concat('Picked up particle ',intToStr(p.id)));
end;

procedure cDisplayer.stopDrag();
begin
     if draggingParticle <> NIL then begin
        //draggingParticle.sx := 0; draggingParticle.sy := 0;
        draggingParticle.fixed := fixedSave;
        trace(Concat('Released Particle ',intToStr(draggingParticle.id)));
        draggingParticle := NIL;
     end;     
end;

procedure cDisplayer.updateDragging(x,y:real); var _mouseSmooth:real;
begin
	 mouseSpeedX := x-mouseLastX;
     mouseSpeedY := y-mouseLastY;
     mouseLastX := x;
	 mouseLastY := y;
     
	_mouseSmooth := mouseSmooth/engine.refreshRate;
	//if not omnipotentMouse then _mouseSmooth += mousePower/100;
     if x = mouseLastX then mouseSpeedX := 0; if y = mouseLastY then mouseSpeedY := 0;
     if draggingParticle <> NIL then begin
        //draggingParticle.x := x;
        //draggingParticle.y := y;
       if omnipotentMouse then begin
          draggingParticle.x := draggingParticle.x + (x-draggingParticle.x)*_mouseSmooth;
          draggingParticle.y := draggingParticle.y + (y-draggingParticle.y)*_mouseSmooth;
       end; //trace(draggingParticle.fixed);
        //draggingParticle.sx := 0; draggingParticle.sy := 0;
        draggingParticle.sx := (x-draggingParticle.x)*_mouseSmooth;
        draggingParticle.sy := (y-draggingParticle.y)*_mouseSmooth;
     end;
end;

end.

