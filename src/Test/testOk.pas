{ TesteOK - 1 }

program Test;
var
  x, y : integer;

procedure setX(val : integer = 0);
begin
 x := val;
end;

begin
  writeln("Contando até 20");

  x := 1;

  while (x <= 20) do
  begin
    writeln(x);
    x += 1;
  end;

  writeln("Ok...");
end.

