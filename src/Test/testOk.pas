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

 write("Digite um número inteiro: ");

 x := readInt();

 writeln("Dobro = ", double_val(x));

end.

