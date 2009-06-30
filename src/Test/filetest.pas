program FileTest ;

uses Builtin, FileBasic ;

begin

	writeFile("ex1.txt", "HelloWorld") ;
	appendFile("ex1.txt", "www") ;
	writeln(readFile("ex1.txt")) ;
	
end.

