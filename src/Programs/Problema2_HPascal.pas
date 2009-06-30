program Problema2;

var n, p, x, y, res, i : integer;

begin
	n := readint();
	p := readint();
	res := 0;
	for i := 1 to n do
	    begin
	        x := readint();
	        y := readint();
	        if x+y >= p then
				res := res+1;
		end;
	writeln(res);
end.
