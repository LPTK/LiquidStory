(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Links are the main constraint of the engine
        They deal with two particles and two min/max distances
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ppe_uParticle,
  ppe_uMaths,
  ppe_uPropContainer,
  ppe_uTrace;

//const INFINITY = 999999;

type cLink = Class
     protected
     length1,length2:real;
     visible:boolean;
     public
     id:Integer;
     p1,p2:cParticle; thickness:integer;
     elasticity, resistance, plasticDomain, plasticCoeff:real;
     // !! TODO: Implement these new parameters: plasticDomain, plasticCoeff
     
     broken:boolean; color:tColor;
     
     constructor Create(_p1,_p2:CParticle; _length1,_length2:real);
     constructor Create(_p1,_p2:CParticle; _length:real);
     constructor Create(_p1,_p2:CParticle);
     constructor Create(_p1,_p2:CParticle; pc:cPropContainer);
     
     procedure init; virtual;
     procedure refresh(computeSpeeds:Boolean; motionSpeed:real); virtual;
     procedure display(x1,y1,x2,y2:real; var canv:tCanvas);
     procedure breakLink(); virtual;
     function isVisible:boolean;
     procedure show;
     procedure hide;
     function getPropContainer():cPropContainer; virtual;

end;

implementation

var nbLinks:Integer;

constructor cLink.Create(_p1,_p2:CParticle; _length1,_length2:real);
begin
     p1 := _p1; p2 := _p2; length1 := _length1; length2 := _length2; thickness := 1;
     elasticity := 1; resistance := -1; plasticDomain := -1; plasticCoeff := 0.5; visible := true;
     id := nbLinks; nbLinks += 1;
end;

constructor cLink.Create(_p1,_p2:CParticle; _length:real);
begin
     Create(_p1,_p2,_length,_length);
end;

constructor cLink.Create(_p1,_p2:CParticle);
begin
     Create(_p1,_p2,distance(_p1,_p2));
     init;
end;

constructor cLink.Create(_p1,_p2:CParticle; pc:cPropContainer);
begin
     if pc.length1 < 0 then length1 := distance(_p1,_p2) else length1 := pc.length1;
     if pc.length2 < 0 then length2 := distance(_p1,_p2) else length2 := pc.length2;
     Create(_p1,_p2,length1,length2);
     elasticity := pc.elasticity; resistance := pc.resistance; thickness := pc.thickeness;
     plasticDomain := pc.plasticDomain; plasticCoeff := pc.plasticCoeff;
end;

procedure cLink.init; begin end;

function cLink.getPropContainer():cPropContainer;
begin
     getPropContainer := cPropContainer.Create();
     getPropContainer.elasticity := elasticity; getPropContainer.resistance := resistance;
     getPropContainer.plasticDomain := plasticDomain; getPropContainer.plasticCoeff := plasticCoeff;
end;

procedure cLink.display(x1,y1,x2,y2:real; var canv:tCanvas);
begin
	//if visible then begin
    canv.Pen.Color := color; canv.Pen.Style := psSolid; canv.Pen.Width := thickness;
    canv.line(round(x1),round(y1),round(x2),round(y2));
	//end;
end;

procedure cLink.refresh(computeSpeeds:Boolean; motionSpeed:real);
var dist,len,co,co1,co2,el,heiCo,widCo,r:real;
begin if not broken then begin
     dist := distance(p1,p2); // ! TODO: use isInBigSquare/isOutOfSmallSquare optimization functions in "maths"
     len := -1;
     if dist<length1 then len := length1 else if dist>length2 then len := length2;
     
     if (len>=0) and (dist<>0) then begin
     	
     	co := len/dist;
     	if p1.fixed then co1 := 0 else co1 := p2.mass/(p1.mass+p2.mass);
     	if p2.fixed then co2 := 0 else co2 := p1.mass/(p1.mass+p2.mass);
     	
     	el := elasticity;
     	widCo := (p2.x-p1.x)*(1-co)*el;
	
    	heiCo := (p2.y-p1.y)*(1-co)*el;
    	
    	p1.x := p1.x+widCo*co1;
    	p1.y := p1.y+heiCo*co1;
    	
    	p2.x := p2.x-widCo*co2;
    	p2.y := p2.y-heiCo*co2;
    	
    	if motionSpeed = 0 then r := 1 else r := 1/motionSpeed;
    	
    	if computeSpeeds then begin
    	   p1.sx := p1.sx+widCo*co1*r;
    	   p1.sy := p1.sy+heiCo*co1*r;
    	   p2.sx := p2.sx-widCo*co2*r;
    	   p2.sy := p2.sy-heiCo*co2*r;
    	end;
    	
    	if (resistance >= 0) and (abs(len-dist)*r > resistance) then breakLink;
    					
     end;
end; end;

procedure cLink.breakLink();
begin
     broken := true;
end;

function cLink.isVisible:boolean;
begin
     isVisible := visible;
end;

procedure cLink.show;
begin
     visible := true;
end;

procedure cLink.hide;
begin
     visible := false;
end;

end.

