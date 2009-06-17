program FibTest;

  var n : integer = 7;
  var a, b, tmp, i : integer;

  procedure ProcTes (a : integer = 3);
  begin
    a := 1;
  end;

begin

  a := 1;
  b := 1;

  for i := 0 to (n-2) do
  begin
    tmp := a;
    b := a + b;
    a := tmp
  end;

 writeln(b);

end.

