program Test;
var
  x, y : integer;

procedure setX(a : integer; b : integer = 0);

  procedure Test();
  begin
   a := b;
  end;

begin
 x := a;

 setX(2);
end;

begin
  //writeln("Contando até 20");

  x := 1;

  while (x <= 20) do
  begin
    //writeln(x);
    x += 1;
  end;

  //writeln("Ok...");
end.

