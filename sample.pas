program sample(input, output);

var
  num : integer;

function factorial(num : integer): integer;
var
  count: integer;

begin
  count := 1;
  while num > 1 do
  begin
     count := count * num;
     num := num - 1
   end;
  factorial := count
end;

begin
  writeln(factorial(10))
end.
