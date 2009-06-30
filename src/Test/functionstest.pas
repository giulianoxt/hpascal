program Test;

uses String, Builtin ;

var
  x, y : integer;
  z : real;
  a : string ;
  b : string ;
  
function double_val(n : integer) : integer;

procedure B();
begin
 double_val := n * 2;
end;

begin
  B();
end;

begin

 a := "Hello " ;
 b := "World" ;
 
 writeln(strcat(a,b)) ;
 writeln(strcmp(a,b)) ;
 writeln(substr(a,1,3)) ;
 writeln(strchr(a,'l',1)) ;
 
end.

