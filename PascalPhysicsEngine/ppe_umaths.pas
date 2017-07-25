(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Here are some useful mathematical functions
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uMaths;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppe_uPoint;

function distance(x1,y1,x2,y2:real):real; // Distance between 2 points
function distance(p1,p2:cPoint):real;     // Distance between 2 cPoints
function distance(p1,p2:tPoint):real;     // Distance between 2 tPoints
function ArcCos ( X : Real ): Real;
function ArcSin ( X : Real ): Real; 

function isInBigSquare(x1,y1,x2,y2,dist:real):Boolean;
// This function used to know if a distance calculation is needed or if we're
// sure the points are too distant (out of the majoration square)
// (it's in the case we have a minimum distance to check)

function isOutOfSmallSquare(x1,y1,x2,y2,dist:real):Boolean;
// In the same manner as isInBigSquare, it checks if a fine
// calculation is needed in the case we have a maximum distance to check


implementation

const sqrt2 = 1.414213562;

function distance(x1,y1,x2,y2:real):real;
begin
     distance := sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
end;

function distance(p1,p2:cPoint):real;
begin
     distance := sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y));
end;

function distance(p1,p2:tPoint):real;
begin
     distance := sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y));
end;


FUNCTION ArcCos(X: Real): Real; { Arc cosine in degrees }
BEGIN { Function ArcCos }
  IF X = 0.0 THEN  ArcCos := 90.0
  ELSE
    IF X = 1.0 THEN ArcCos := 0.0
    ELSE
      IF X = -1.0 THEN ArcCos := 180.0
      //else trace(Sqrt(1 - Sqr(X)));
      ELSE ArcCos := Arctan( X/ Sqrt(1.0 - Sqr(X)))
END; { Function ArcCos }


 function ArcSin ( X : Real ): Real; 
 begin 
   if X = 1.0 then { to avoid division by 0 } 
     ArcSin := Pi / 2.0 
   else 
     ArcSin := ArcTan ( X / Sqrt ( 1 - X * X ) ); 
 end;


function isInBigSquare(x1,y1,x2,y2,dist:real):Boolean;
begin
     isInBigSquare := (abs(x1-x2)<=dist) and (abs(y1-y2)<=dist);
end;

function isOutOfSmallSquare(x1,y1,x2,y2,dist:real):Boolean;
begin
     isOutOfSmallSquare := (abs(x1-x2)>=dist/sqrt2) and (abs(y1-y2)>=dist/sqrt2);
end;


end.
                
