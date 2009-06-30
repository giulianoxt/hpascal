program Problema2;

var n, p, x, y, res, i : integer;

begin
	read(n);
	read(p);
	res := 0;
	for i := 1 to n do
	    begin
	        read(x);
	        read(y);
	        if x+y >= p then
				res := res+1;
		end;
	writeln(res);
end.
