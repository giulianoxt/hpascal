program FibTest;

var
 i    : integer;
 done : boolean;

function fib (n : integer) : integer;
var
  j, k : integer;
begin
  if n = 1 then
    fib := 1
  else if n = 2 then
    fib := 1
  else
    fib := fib(n-1) + fib(n-2)
end

begin
 done := false;

 while not done do
 begin
  write("i: ");
  i := readInt();

  if i = 0 then
    done := true
  else
    writeln("fib(",i,") = ", fib(i))
 end;

end.

