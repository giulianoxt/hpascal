program Test;
var
  x, y : integer;

function double_val(n : integer) : integer;

procedure B();
begin
 double_val := n * 2;
end;

begin
  B();
end;

begin

 writeln(double_val(2));

 writeln(pow(2,10));

end.

