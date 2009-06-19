program Test;

var a : integer;
var x, y : boolean;
    b : integer = 2 * 2;

procedure testProc(var a : integer; b : integer = 0);
begin
  if b >= 2 then
    a := a + b;
end;

begin
  a := 2;
  b += a * 4 + 5;
  x := (a + 3) > 2;

  if (a > b) then
   x := false
  else begin
   b := 3;
   a -= b;
  end;

  while (not x and (a < b)) do
  begin
   b := a + b;
  end;

  for a := 0 to 15 do begin
   if (a mod 2 = 1) then
    b += 1
   else
    b -= 1
  end;
 
  repeat 
    a += 1;
    b *= 2;
  until (a + b) >= 10;

  case (a div b) of
   1, 2 .. 4 :
     a := 3;
   5 :
     a := 4;
   else
     a := 5;
  end;

  testProc(a, b);

  writeln(a);
end.

