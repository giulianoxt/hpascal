program Problema4;

var m, n, r : integer;

procedure mdc(m, n : integer; var r : integer);
	begin
		if m mod n = 0 then
		    r := n
		else if n mod m = 0 then
			r := m
		else if m < n then
			mdc(n, m, r)
		else
			mdc(n, m mod n, r)
	end;

begin
	writeln('Digite o valor de m:');
	read(m);
	writeln('Digite o valor de n:');
	read(n);
	mdc(m, n, r);
	writeln('mdc(m, n) = ', r);
end.
