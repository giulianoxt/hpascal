{-
	testOk
	- Nenhum erro
-}

program Test;
var
  x, y : integer;
  z : boolean;
begin
  writeln("Contando até 20");

  x := 1;

  while (x <= 20) do
  begin
    writeln(x);
    x := x + 1;
  end;

  writeln("Tah contado boooy");
end.

