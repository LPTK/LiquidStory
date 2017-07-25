(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
    
    Abstract:
    	The vapor game particle
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uVaporParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,
  ppe_uTrace,
  gam_uGameParticle,
  gam_uEyedParticle;

type cVaporParticle = Class(cEyedParticle)
  procedure enterFrame(motionSpeed:real); override;
  procedure init; override;
  
  protected
  subjectiveTime, lastSubTime:real; dX,dY:integer;
  procedure drawBody(_x,_y:real; var canv:tCanvas); override;
end;

implementation

procedure  cVaporParticle.enterFrame(motionSpeed:real);
const vol=0.5;//0.6;//0.4
begin
     inherited;
	 sy -= vol*motionspeed;
     subjectiveTime += motionSpeed;
end;
procedure cVaporParticle.init;
begin
	inherited;
	//color := clWhite;
    //color := RGBToColor(195,249,255); //$c3f9ff;
    color := RGBToColor(220,220,220);
    subjectiveTime := 0; lastSubTime := 0;
    partType := ptVapor;
end;
procedure cVaporParticle.drawBody(_x,_y:real; var canv:tCanvas);
const nb = 5; delta = 3;// 5 / 4
var i:integer;
begin
    canv.Pen.Style:=psClear;
	canv.Brush.Color := color;
    for i:= 1 to nb do begin
        if subjectiveTime<>lastSubTime then begin
           dX := Random(delta*2)-delta;
           dY := Random(delta*2)-delta;
        end;
    	canv.Ellipse(round(_x-size/2)+dX,round(_y-size/2)+dY,
     		   round(_x+size/2)+dX,round(_y+size/2)+dY);
    end;
    lastSubTime := subjectiveTime;
end;

end.

