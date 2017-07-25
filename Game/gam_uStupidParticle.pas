(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
    
    Abstract:
    	This is the stupid particle
        It was used when we didn't have any other skin!
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uStupidParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, gam_uGameParticle;

type cStupidParticle = Class(cGameParticle)
     public
     procedure display(_x,_y:real; var canv:tCanvas); override;
end;

implementation

var partBmp:tBitmap;

procedure cStupidParticle.display(_x,_y:real; var canv:tCanvas);
begin
	//partBmp.SetSize(round(size),round(size));
	size := partBmp.Width;
	canv.Draw(round(_x-partBmp.Width/2),round(_y-partBmp.Height/2),partBmp);
end;

initialization

partBmp := tBitmap.Create; partBmp.LoadFromFile('img/stupid_part.bmp');

end.

