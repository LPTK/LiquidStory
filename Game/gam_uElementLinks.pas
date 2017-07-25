(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The different kinds of links in the game
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uElementLinks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ppe_uLink,
  ppe_uTrace,
  gam_ubonus;

type cElementLink = Class(cLink)
     procedure init; override;
  end;
  cLiquidLink = Class(cElementLink)
     procedure init; override;
  end;
  cVaporLink = Class(cElementLink)
     procedure init; override;
  end;
  cIceLink = Class(cElementLink)
     procedure init; override;
  end;

implementation

procedure cElementLink.init; //var b:cBonus;
begin
     inherited;
     {b := NIL;		trace('el created');
     if (p1.ClassType=cBonus) then b := (p1 as cBonus)//cBonus(p1)
     else if (p2.ClassType=cBonus) then b := (p2 as cBonus);
     if b<>NIL then
     begin
          if isVisible and (b.catched=false) then
          begin
               b.catched:=true;
               b.PartBonus(gam_uScore.mainScore);
               trace('CATCH');
          end;
     end;}
end;

procedure cLiquidLink.init;
begin
     inherited;
  	 thickness := 2;
     color := clBlue;
end;

procedure cVaporLink.init;
begin
     inherited;
  	 thickness := 1;
     color := RGBToColor(220,220,220);;
end;

procedure cIceLink.init;
begin
     inherited;
  	 thickness := 3;
     color := clWhite;
end;

end.


