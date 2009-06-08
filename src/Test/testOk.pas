{-
	testOk
	- Nenhum erro
-}

program Test;
var
  x, y : integer;
  z : boolean;
begin
  x := 2;
  x += (x + 1) {- comentario -} div 3;
  x := 3 shl 2;
  y := x + 3 * 3 / (4 * x -  5);
end.

