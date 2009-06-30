program Problema5;

type rational_t = record
				      numerador, denominador : integer
				  end;

var
	r1, r2, r3 : rational_t;
	b1, b2 : boolean;

function build(a, b : integer) : rational_t;
var res : rational_t;
begin
	res.numerador := a;
	res.denominador := b;
	sub1 := res;
end;

function equals(a, b : rational_t) : boolean;
begin
	sub2 := a.numerador*b.denominador = a.denominador*b.numerador;
end;

begin
	r1 := build(3, 4);
	r2 := build(4, 6);
	r3 := build(6, 8);
	b1 := sub2(r1, r2);
	b2 := sub2(r1, r3);
	writeln(b1, ' ', b2);
end.
