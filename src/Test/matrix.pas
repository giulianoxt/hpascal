program TestMatriz;

const N = 3;

type
  MatrizT = array [1 .. N, 1 .. N] of integer;

var
  x : MatrizT;


procedure printMatrizT (var m : MatrizT);
var i, j : integer;
begin
 for i := 1 to N do begin
  for j := 1 to N do 
    write(m[i][j], ' ');
  writeln();
 end;
end;


begin

 x[1][1] := 2;

 printMatrizT(x);

end.

