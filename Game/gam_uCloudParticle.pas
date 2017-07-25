(*
		Liquid Story - Copyright(c) 2011

	Authors:
        Pierre *CURIS*
    	Lionel Parreaux
    
    Abstract:
    	The game particles used for the clouds
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uCloudParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ppe_uDisplayPoint,
  gam_uGameParticle;

type cCloudParticle = Class(cGameParticle)
     public
     procedure init; override;
     //procedure enterFrame(motionSpeed:real); override;
     procedure drawBody(_x,_y:real; var canv:tCanvas); override;
end;

implementation

procedure cCloudParticle.init;
begin
	inherited;
    partType := ptCloud;
	color := clWhite;
end;

(*procedure cCloudParticle.enterFrame(motionSpeed:real);
begin
	
end;*)

procedure cCloudParticle.drawBody(_x,_y:real; var canv:tCanvas);
begin
	canv.Pen.Style:=psClear;
	canv.Brush.Color := color;
	canv.Ellipse(round(_x-size/2),round(_y-size/2),
     		   round(_x+size/2),round(_y+size/2));
end;

end.

