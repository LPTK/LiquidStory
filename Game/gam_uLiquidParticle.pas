(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The liquid particles
        They scream when they are moving too fast!
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uLiquidParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, //ppe_uTrace,
  ppe_uMaths,
  ppe_uTrace,
  gam_uSoundManager, soundlib3dll,
  gam_uGameParticle,
  gam_uEyedParticle;

type cLiquidParticle = Class(cEyedParticle)
     public
     screamSound:tSoundStream; screaming:Boolean;

     procedure init; override;
     procedure display(_x,_y:real; var canv:tCanvas); override;
     
     protected
     procedure drawBody(_x,_y:real; var canv:tCanvas); override;
end;

var screamingSoundTimer:real;//partpng : TPortableNetworkGraphic;

implementation

procedure cLiquidParticle.init;
begin
	inherited;
	color := RGBToColor(53,158,215);;//clBlue;
	screamSound := getSound('scream'); screaming := false;
    contourColor := rgbToColor(0,108,255); //noContour := true;
    partType := ptLiquid;
end;

procedure cLiquidParticle.display(_x,_y:real; var canv:tCanvas);
var sep,sep2,siz,ouv:real; ex,ey:real; //pngpart : TPortableNetworkGraphic; //blinking:Boolean;
L,R,alpha,xs,ys,qx,qy,speedMult:real;
begin
	inherited;
	//canv.draw( round(_x-partpng.height/2),round(_y-partpng.height/2),partpng);
	sep := size*0.23; sep2 := size*0.25; siz := size*0.24;
	ouv := distance(0,0,sx,sy)*2; if ouv>size*0.45 then ouv := size*0.45;
	if ouv>size*0.3 then begin
		if not screaming then begin
			//trace('ok!');
			screaming := true;
			if not screamSound.isplaying and (screamingSoundTimer<=0) then begin
				(*screamSound.play;*) screamingSoundTimer := 30 + Random(20); end;
		end;
	end else //if ouv<size*0.2 then
		screaming := false;
	
	//ex := mouseLastX; ey := mouseLastY;
	if ouv > size*0.45/2 then begin
		ex := _x + sx*10; ey := _y + sy*10;
		//if not screamSound.isplaying and (screamingSoundTimer>10000) then begin screamSound.play; screamingSoundTimer := 0; end;
	end;
	canv.Brush.Color := clWhite;
	canv.Ellipse(round(_x-ouv),round(_y+sep2-ouv/2),round(_x+ouv),round(_y+sep2+ouv/2));
        //pngpart := TPortableNetworkGraphic.create;
        //pngpart.loadFromFile ('img/transp.png');
    
	//sx := 0; sy := 5;
	
	
	// v TODO !!!!!
	
	speedMult := size/2/100;
	xs := sx*speedMult; ys := sy*speedMult;
	L := distance(0,0,xs,ys);
    R := size/2;
    
	if L>R then begin
		
    	//if (L = 0) ...
    	if (L <> 0) then begin //and (sx<>0) then begin
    		//trace(arccos(R/L)); //trace(sin(2*PI));
    		//trace(xs);
    		alpha := arccos(sx/L)-arccos(R/L);
    		//alpha := arctan(R/L);
    		qx := _x+sin(alpha)*R; qy := _y+cos(alpha)*R;
    		canv.line(round(qx),round(qy),round(_x+xs),round(_y+ys));
    		//qx := _x+cos(alpha)*R; qy := _y-sin(alpha)*R;
    		//canv.line(round(qx),round(qy),round(_x+xs),round(_y+ys));
    	end;
    	
	end;
		
end;

procedure cLiquidParticle.drawBody(_x,_y:real; var canv:tCanvas);
begin
	inherited;
	//canv.draw(round(_x-partpng.height/2),round(_y-partpng.height/2),partpng);
end;


initialization
//partPng := TPortableNetworkGraphic.create; partPng.loadFromFile ('img/liquid.png');
screamingSoundTimer := 0;

end.

