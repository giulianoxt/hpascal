program Test;
var
  x, y : integer;
  z : real;
  
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

 z := 0.8;
 
 writeln("Dobro = ", double_val(x));

 writeln("log = ", log(z)) ;
 writeln("sqr = ", sqr(z)) ;
 writeln("sqrt = ", sqrt(z)) ;
 writeln("arcsin = ", arcsin(z)) ;
 writeln("arccos = ", arccos(z)) ;
 writeln("arctan = ", arctan(z)) ;
 writeln("!(false) = ", not(false)) ;
 writeln("even(x) = ", even(x)) ;
 writeln("odd(x) = ", odd(x)) ;
 writeln("sin(z) = ", sin(z)) ;
 writeln("cos(z) = ", cos(z)) ;
 writeln("tan(z) = ", tan(z)) ;
 
end.

