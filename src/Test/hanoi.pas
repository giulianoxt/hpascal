program TowersOfHanoi;

var
  disks : integer = 3;

procedure Hanoi(
  source, temp, destination : string;
  n                         : integer
);
begin
  if n > 0 then
  begin
    Hanoi(source, destination, temp, n-1);

    writeln(" Move disk "  , n,
            " from tower ", source,
            " to tower " , destination);

    Hanoi(temp, source, destination, n-1);
  end
end;


begin
  writeln("Hanoi Towers\n");
  writeln("Showing solution for n = ", disks, ":");

  Hanoi("A", "B", "C", disks);

  writeln();
end.

