(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The ice particles are entirely drawn by code!!
        I added an option 'computePolygonAtEachFrame' so that the polygon is
        not calculated at each frame, but then it cannot rotate anymore
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uIceParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ppe_uTrace,
  ppe_uMaths, ppe_uPoint,
  gam_uGameParticle,
  gam_uEyedParticle;

const ptsPerBranch = 12; ptsNb = 6*ptsPerBranch;

type cIceParticle = Class(cEyedParticle)
     public
     procedure init; override;
     procedure enterFrame(motionSpeed:real); override;
     procedure display(_x,_y:real; var canv:tCanvas); override;

     protected
     mouthState:Boolean; mouthTime:real; angle,angleSpeed:real;
     pts,ptsSave:array[1..ptsNb] of tPoint;
     procedure drawBody(_x,_y:real; var canv:tCanvas); override;
     procedure computePolygon(_x,_y:real);
     procedure updatePolygon(_x,_y:real);
     //branchPt:cPoint; branchSpeed:cPoint;
end;

var computePolygonAtEachFrame:Boolean;

implementation

procedure cIceParticle.init;
begin
	inherited;
	//color := clWhite;
	color := RGBToColor(176,255,245);
	mouthState := false; mouthTime := 0;
	angle := 2*PI*Random(100)/100; angleSpeed := (Random(100)/100-0.5)*0.03;
	computePolygon(0,0);
    ptsSave := pts;
    partType := ptIce;
	//branchPt := cPoint.Create(x,y); branchSpeed := cPoint.Create(0,0);
end;

procedure cIceParticle.enterFrame(motionSpeed:real);
const chatteringPeriod = 2; speedCoeff = 0.98; speedFriction = 0.85;
begin
    
    inherited; // Should its eyes blink?
    
	if mouthTime>0 then mouthTime -= motionSpeed
	else begin
		mouthTime := chatteringPeriod;
		mouthState := not mouthState;
	end;
	//angleSpeed += sx/1000;
    angleSpeed += sx/1000*motionSpeed;
    
	angleSpeed *= 0.95;
	angle += angleSpeed;
	
	{expérimentation moisie:} {branchSpeed.x += (x-branchPt.x)*speedCoeff;
	branchSpeed.y += (y-branchPt.y)*speedCoeff;
	branchSpeed.x *= speedFriction;
	branchSpeed.y *= speedFriction;
	
	branchPt.x += branchSpeed.x;
	branchPt.y += branchSpeed.y;}
end;

procedure cIceParticle.computePolygon(_x,_y:real);
var i,j:integer; r1,r2,r3,r4,siz,theta,phi,
shift0,shift1,shift2,posShift,plus,dx,dy:real;//,pos_shift:real;
begin
	siz := size*1;
	r1 := siz*0.5; r2 := siz*0.6; r3 := siz*0.75; r4 := siz; plus := 1.1;
	theta := angle; shift0 := PI/40; shift1 := PI/15; shift2 := PI/25; posShift := siz*0.15;
	
	{now useless:} dx := 0; dy := 0; //dx := (x-branchPt.x); dy := (y-branchPt.y);
	
	{truc marrant:} {if true then for i := 1 to ptsNb do begin
		pts[i].x := round(_x+Random(50)-25);
		pts[i].y := round(_y+Random(50)-25);
	end else}
	for i := 0 to 5 do begin
		j := i*ptsPerBranch+1;
		phi := theta;
		pts[j].x := round(_x+cos(phi)*r1);
		pts[j].y := round(_y+sin(phi)*r1);
		j += 1;
		pts[j].x := round(_x+cos(phi+shift0)*r2+dx);
		pts[j].y := round(_y+sin(phi+shift0)*r2+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi-shift1*plus)*(r2+posShift)+dx);
		pts[j].y := round(_y+sin(phi-shift1*plus)*(r2+posShift)+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi-shift1)*(r3+posShift)+dx);
		pts[j].y := round(_y+sin(phi-shift1)*(r3+posShift)+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi+shift0)*r3+dx);
		pts[j].y := round(_y+sin(phi+shift0)*r3+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi+shift2)*r4+dx);
		pts[j].y := round(_y+sin(phi+shift2)*r4+dy);
		j += 1;
		phi += 2*PI/15;
		pts[j].x := round(_x+cos(phi-shift2)*r4+dx);
		pts[j].y := round(_y+sin(phi-shift2)*r4+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi-shift0)*r3+dx);
		pts[j].y := round(_y+sin(phi-shift0)*r3+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi+shift1)*(r3+posShift)+dx);
		pts[j].y := round(_y+sin(phi+shift1)*(r3+posShift)+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi+shift1*plus)*(r2+posShift)+dx);
		pts[j].y := round(_y+sin(phi+shift1*plus)*(r2+posShift)+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi-shift0)*r2+dx);
		pts[j].y := round(_y+sin(phi-shift0)*r2+dy);
		j += 1;
		pts[j].x := round(_x+cos(phi)*r1);
		pts[j].y := round(_y+sin(phi)*r1);
		
		theta += 2*PI/6;
	end;
end;

procedure cIceParticle.updatePolygon(_x,_y:real); var i:integer;
begin
    if computePolygonAtEachFrame then computePolygon(_x,_y)
    else begin
        for i := 0 to ptsNb do begin
            pts[i].x := round(ptsSave[i].x + _x);
            pts[i].y := round(ptsSave[i].y + _y);
        end;
    end;
end;

procedure cIceParticle.drawBody(_x,_y:real; var canv:tCanvas);
begin
    //computePolygon(_x,_y);
    updatePolygon(_x,_y);
	canv.Brush.Color := color; //canv.Pen.Width := 1; //canv.pen.Cosmetic:=false;
	canv.Pen.Style := psClear;
	canv.Polygon(pts);
	canv.Pen.Style := psSolid;
end;


procedure cIceParticle.display(_x,_y:real; var canv:tCanvas);
var sep,hei,siz,ouv:real; ex,ey:real; i,nb:integer; //blinking:Boolean;
begin
	inherited;
	
	//sep := size*0.2; siz := size*0.3; hei := size*0.1; ouv := size*0.03;
	sep := size*0.2; siz := size*0.35; hei := size*0.15; ouv := size*0.04;
	_y += sep;
	canv.Brush.Color := clWhite;
	if mouthState then hei += ouv;
	canv.Rectangle(round(_x-siz),round(_y-hei),round(_x+siz),round(_y+hei));
	nb := 6;
	for i := 1 to nb-1 do canv.Line(round(_x-siz+i*siz*2/nb),round(_y-hei),round(_x-siz+i*siz*2/nb),round(_y+hei));
	{if mouthState then hei := ouv
	else hei := 0;}
	hei := ouv; //hei /= 4;
	canv.Brush.Color := clBlack;
	if mouthState then canv.Rectangle(round(_x-siz),round(_y-hei),round(_x+siz),round(_y+hei))
	else canv.Line(round(_x-siz),round(_y),round(_x+siz),round(_y));
	
	//siz := size*0.3; canv.Ellipse(round(branchPt.x-siz),round(branchPt.y-siz),round(branchPt.x+siz),round(branchPt.y+siz));
end;

initialization
computePolygonAtEachFrame := false;

end.

