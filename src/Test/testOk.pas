{ TesteOK - 1 }

program Test;
var
  x, y : integer;
  z    : boolean;
begin
  writeln("Contando até 20");

  x := 1;

  while (x <= 20) do
  begin
    writeln(x);
    x := x + 1;
  end;

  writeln("Tire onda booooyy! hauahuhauha");

  y := x - 1;

  if (y = 20) then
   writeln("eita")
  else
   writeln("opa");

end.

