program Problema3;

type matrix_t = array[1 .. 100, 1 .. 100] of integer;

var
	m1, m2, res : matrix_t;
	rows1, cols1, rows2, cols2, i, j, k : integer;

procedure printRes(rows, cols : integer);
begin
	for i := 1 to rows do
		begin
			for j := 1 to cols do
				write(res[i][j], ' ');
			writeln();
		end;
end;

procedure addMatrix();
begin
	for i := 1 to rows1 do
		for j := 1 to cols1 do
		    res[i][j] := m1[i][j]+m2[i][j];
end;

procedure multMatrix();
begin
	for i := 1 to rows1 do
	    for j := 1 to cols2 do
	        res[i][j] := 0;
	for i := 1 to rows1 do
	    for j := 1 to cols2 do
	        for k := 1 to cols1 do
	            res[i][j] := res[i][j] + m1[i][k]*m2[k][j];
end;

begin
	writeln("Digite o numero de linhas e o numero de colunas da primeira matriz, respectivamente:");
	rows1 := readint();
	cols1 := readint();
	writeln("Digite os elementos da primeira matriz, em ordem de linha maior:");
	for i := 1 to rows1 do
	    for j := 1 to cols1 do
			m1[i][j] := readint();
	writeln("Digite o numero de linhas e o numero de colunas da segunda matriz, respectivamente:");
	rows2 := readint();
	cols2 := readint();
	writeln("Digite os elementos da segunda matriz, em ordem de linha maior:");
	for i := 1 to rows2 do
	    for j := 1 to cols2 do
			m2[i][j] := readint();
	if (rows1 = rows2) and (cols1 = cols2) then
	    begin
			addMatrix();
			printRes(rows1, cols1);
		end
	else
		writeln("Impossivel efetuar a operacao de soma entre as matrizes");
	if cols1 = rows2 then
	    begin
	        multMatrix();
	        printRes(rows1, cols2);
		end
	else
		writeln("Impossivel efetuar a operacao de multiplicacao entre as matrizes");
end.
