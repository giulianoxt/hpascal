program RefTest;

var x, y : integer;

procedure setVal(var v : integer; n : integer);

 procedure setValDouble(var x : integer);
 begin
  x := n * 2;
 end;

begin
 setValDouble(v);
end;

begin
 x := 0;
 y := 0;

 setVal(x, 1);
 setVal(y, x+1);

 writeln("x = ", x);
 writeln("y = ", y);
end.

