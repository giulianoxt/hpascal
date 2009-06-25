program Test;
var
  x, y : integer;

procedure setX(a : integer);
begin
 x := a;
end;

begin
  //writeln("Contando até 20");

  setX(1);

  while (x <= 20) do
  begin
    //writeln(x);
    x += 1;
  end;

  //writeln("Ok...");
end.

