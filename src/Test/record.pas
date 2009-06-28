program RecordTest;

type
  point = record
           x, y : integer;
          end;

  testR = record
             testF : record
                      field1 : boolean;
                     end;
          end;

var p : point;
    x : testR;

procedure readPoint(var p : point);
begin
 p.x := readint();
 p.y := readint();
end;

procedure printPoint(p : point);
begin
 writeln("Point<",p.x,", ",p.y,">");         
end;

begin
  x.testF.field1 := true;
  writeln("field1 = ", x.testF.field1);

//  readPoint(p);
  printPoint(p);
end.

