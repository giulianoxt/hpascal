program TestArray;

const TAM = 2;

type
  vectort = array [1 .. TAM] of integer;

var
  v  : vectort;

procedure readVector(var vec : vectort);
var i : integer;
begin
 for i := 1 to TAM do
  vec[i] := readint();
end;

begin
 readVector(v);
 writeln(v);
 
 v[1] := 666;
 writeln(v);

end.

