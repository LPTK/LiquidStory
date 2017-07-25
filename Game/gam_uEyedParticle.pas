(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
    
    Abstract:
    	A general class for particles with eyes that follow the mouse
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uEyedParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ppe_uDisplayPoint,
  ppe_uDisplayer,
  ppe_uMaths,
  ppe_uTrace,
  gam_uGameParticle;

type cEyedParticle = Class(cGameParticle)
     public
     procedure init; override;
     procedure enterFrame(motionSpeed:real); override;
     procedure display(_x,_y:real; var canv:tCanvas); override;
     
     protected
     blinkingTime:real; blinking:real;
end;

implementation

procedure cEyedParticle.init;
begin
	color := clBlack;
	inherited;	//trace('init!');
	blinking := 0;
	blinkingTime := 0;
end;

procedure cEyedParticle.enterFrame(motionSpeed:real);
const blinkAvgPeriod = 100; // 150
begin
    inherited;
    if blinking>0 then blinking := blinking-motionSpeed;
	if Random(blinkAvgPeriod*100)<motionSpeed*100 then
	begin
		blinking := 2;
		blinkingTime := 0;
	end;
end;

procedure drawEye(x,y,_x,_y,radius:real; blinking:Boolean; color:tColor; var canv:tCanvas);
const ratio = 0.4; var d,coef:real;//var ix,iy:integer;
begin
	canv.Brush.Color := clWhite; canv.pen.Color := clBlack;
	if blinking then canv.Brush.Color := color;
	canv.Ellipse(round(x-radius),round(y-radius),round(x+radius),round(y+radius));
	
	if blinking then canv.Line(round(x-radius),round(y),round(x+radius),round(y))
	else begin
      	d := distance(x,y,_x,_y);
      	if d > radius*(1-ratio) then begin
      		//_x := x; _y := y;
      		coef := radius*(1-ratio)/d;
      		_x := x+(_x-x)*coef; _y := y+(_y-y)*coef;
      	end {else if d>0 then begin
      		coef := radius*(1-ratio)/d;
      		_x := x+(_x-x)/coef; _y := y+(_y-y)/coef;
      	end};
      	//_x := x; _y := y;
      	canv.Brush.Color := clBlack; radius := radius*ratio;
      	canv.Ellipse(round(_x-radius),round(_y-radius),round(_x+radius),round(_y+radius));
	end;
end;

procedure cEyedParticle.display(_x,_y:real; var canv:tCanvas);
var sep,sep2,siz,ouv:real; ex,ey:real; //blinking:Boolean;
begin
	inherited;
	defCanv(canv);
	
	sep := size*0.23; sep2 := size*0.25; siz := size*0.24;
	ex := mouseRealLastX; ey := mouseRealLastY;
	
	ouv := distance(0,0,sx,sy)*2; //if ouv>size*0.45 then ouv := size*0.45;
	if ouv > size*0.45/2 then begin
		ex := _x + sx*10; ey := _y + sy*10;
	end;
	
	drawEye(_x-sep,_y-sep,ex,ey,siz,blinking>0,color, canv);
	drawEye(_x+sep,_y-sep,ex,ey,siz,blinking>0,color, canv);
	
end;

end.

