unit vectors;

interface

type
  tSVector =  array[0..2] of double;
(*  tSVector =  packed record
              case integer of
               0 : (raw : array[0..2] of double);
               1 : (x,y,z : double);
             end;*)

const
  axis_x = 0;
  axis_y = 1;
  axis_z = 2;

function getDistanceVolumeFactor(dist : double) : double;
procedure quickSort(var A: array of tSVector);


function betrag(vec : tSVector) : double;
function getVecAngleCos(vec1,vec2 : tSVector) : double;
function addVec(vec1,vec2 : tSVector) : tSVector;
function subVec(vec1,vec2 : tSVector) : tSVector;
function rotateVec(vec : tSVector; angle : tSVector) : tSVector;
function multVec(vec : tSVector; factor : double) : tSVector;
function divVec(vec : tSVector; divisor : double) : tSVector;
function getDistance(vec1,vec2 : tSVector) : double;



implementation

function getDistanceVolumeFactor(dist : double) : double;
// Returns volume factor for given distance in metres
begin
  if (dist <> 0) then result := 1/dist else result := 1;
end;

procedure quickSort(var A: array of tSVector);

 procedure quick_sort(var A: array of tSVector; iLo, iHi: Integer);
 var
   Lo, Hi : integer;
   Mid,T  : tSVector;

 begin
   Lo := iLo;
   Hi := iHi;
   Mid := A[(Lo + Hi) div 2];
   repeat
     while (A[Lo][0] < Mid[0]) do Inc(Lo);
     while (A[Hi][0] > Mid[0]) do Dec(Hi);
     if (Lo <= Hi) then
     begin
       T := A[Lo];
       A[Lo] := A[Hi];
       A[Hi] := T;
       Inc(Lo);
       Dec(Hi);
     end;
   until Lo > Hi;
   if (Hi > iLo) then quick_sort(A, iLo, Hi);
   if (Lo < iHi) then quick_sort(A, Lo, iHi);
 end;

begin
 quick_sort(A, Low(A), High(A));
end;


// Vector functions
function betrag(vec : tSVector) : double;
var
  i : integer;
  sum : double;

begin
  sum := 0;
  for i := 0 to 2 do sum := sum+sqr(vec[i]);
  result := sqrt(sum);
end;


function addVec(vec1,vec2 : tSVector) : tSVector;
// adds vec1 to vec2
var
  sum : tSVector;
  i   : integer;

begin
  for i := 0 to 2 do sum[i] := vec1[i]+vec2[i];

  result := sum;
end;

function subVec(vec1,vec2 : tSVector) : tSVector;
// Subtracts vec2 from vec1
var
  sum : tSVector;
  i   : integer;

begin
  for i := 0 to 2 do sum[i] := vec1[i]-vec2[i];

  result := sum;
end;

function rotateVec(vec : tSVector; angle : tSVector) : tSVector;
// Rotates vector by definied angle(s)
var
  tempvec : tSVector;

begin
  if (angle[axis_x] <> 0) then
  begin // X axis rotate
    tempvec[axis_x] := vec[axis_x];
    tempvec[axis_y] := vec[axis_y]*cos(angle[axis_x])-vec[axis_z]*sin(angle[axis_x]);
    tempvec[axis_z] := vec[axis_y]*sin(angle[axis_x])+vec[axis_z]*cos(angle[axis_x]);
    vec := tempvec;
  end;

  if (angle[axis_y] <> 0) then
  begin // Y axis rotate
    tempvec[axis_x] := vec[axis_x]*cos(angle[axis_y])+vec[axis_z]*sin(angle[axis_y]);
    tempvec[axis_y] := vec[axis_y];
    tempvec[axis_z] := -vec[axis_x]*sin(angle[axis_y])+vec[axis_z]*cos(angle[axis_y]);
    vec := tempvec;
  end;

  if (angle[axis_z] <> 0) then
  begin // Z axis rotate
    tempvec[axis_x] := vec[axis_x]*cos(angle[axis_z])-vec[axis_y]*sin(angle[axis_z]);
    tempvec[axis_y] := vec[axis_x]*sin(angle[axis_z])+vec[axis_y]*cos(angle[axis_z]);
    tempvec[axis_z] := vec[axis_z];
    vec := tempvec;
  end;

  result := vec;
end;

function multVec(vec : tSVector; factor : double) : tSVector;
// Multiplies vector by factor
begin
  vec[axis_x] := vec[axis_x]*factor;
  vec[axis_y] := vec[axis_y]*factor;
  vec[axis_z] := vec[axis_z]*factor;
  result := vec;
end;

function divVec(vec : tSVector; divisor : double) : tSVector;
// Divides vector by divisor
begin
  if (divisor <> 0) then
  begin
    vec[axis_x] := vec[axis_x]/divisor;
    vec[axis_y] := vec[axis_y]/divisor;
    vec[axis_z] := vec[axis_z]/divisor;
  end;

  result := vec;
end;


function getDistance(vec1,vec2 : tSVector) : double;
// Returns distance between two vectors
begin
  result := betrag(subVec(vec1,vec2));
end;

function getAngleByListener(vec1,vec2,userangle : tSVector) : tSVector;
// Returns angles between two vectors, corrected by listener angle
begin

end;

function getVecAngleCos(vec1,vec2 : tSVector) : double;
// Returns angle between two vectors
var
  divisor : double;

begin
  divisor := betrag(vec1)*betrag(vec2);
  if (divisor > 0) then result := (vec1[0]*vec2[0]+vec1[1]*vec2[1]+vec1[2]*vec2[2])/divisor
  else result := 0;
end;

// Vector functions end


end.
