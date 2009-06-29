program TestMatriz;

type
  MatrizT = array [1 .. 3, 1 .. 5] of integer;

var
  x : MatrizT;


procedure printMatrizT (var m : MatrizT);
var i, j : integer;
begin
 for i := low(m) to high(m) do begin
  for j := low(m[i]) to high(m[i]) do 
    write(m[i][j], ' ');
  writeln();
 end;
end;


begin

 x[1][1] := 2;

 printMatrizT(x);

end.

