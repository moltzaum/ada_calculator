
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

-- Not an actual generic package or type,
-- but this package contains a union-like
-- type which can swich modes between floating
-- point and integer types.
--
-- If this was an actual package we might use:
-- package GenericNumber is new GenericNumber(Float)
-- however, the typing here is static which I don't
-- want for my use case.
with Generic_Number; use Generic_Number;

procedure Number_Driver is
  n1 : Number;
  n2 : Number;
begin
  Set(n2, -2);
  Set(n1, 4);
  n1 := n2 * n1 / 1;
  Put(n1);
  --Set(n1, 4.0);
  --Put(n1);
  
  --Put(n1.Float_Value);
  NULL;
end Number_Driver;

