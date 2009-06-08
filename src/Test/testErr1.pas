{-
  testErr1
  
  - Passa OK pelo parser
  - Reporta dois erros de compilação após o parsing (tipos indefinidos)
-}

program Test;
var
  x, y : integer2;
  z : boolean2;
begin
  x := 2;
  x += (x + 1) {- comentario -} div 3;
  x := 3 shl 2;
  y := x + 3 * 3 / (4 * x -  5);
end.

