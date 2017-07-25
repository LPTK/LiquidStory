(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	This is used to easily set links (or particles) parameters when
        creating a bunch of them, like in big simulations...
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uPropContainer; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type cPropContainer = Class
     public
     // For particles:
     x,y,size:real;
     // For links:
     length1,length2:real;
     elasticity, resistance, plasticDomain, plasticCoeff:real;
     thickeness:integer;
     constructor Create;
     //constructor Create(l:cLink);
end;

implementation

constructor cPropContainer.Create();
begin
     size := 0; length1 := -1; length2 := -1; thickeness := 1;
     elasticity := 1; resistance := -1; plasticDomain := -1; plasticCoeff := 0.5;
end;

{constructor cPropContainer.Create(l:cLink);
begin
     size := 0; length1 := -1; length2 := -1;
     elasticity := l.elasticity; resistance := l.;
     plasticDomain := l.plasticDomain; plasticCoeff := l.plasticCoeff;
end;}

end.

