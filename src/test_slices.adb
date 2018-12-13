with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Slices is
   Str : constant String := "a+cde+fgh";
   N : constant := 3;
   M : constant := 3;
begin
  --bcd
  --bcdefgh
  --abcdefg
  --efg
  --fgh

  --Str'Last
  Put_Line (Str(N..-1 + Index(Str(N..Str'Last), "-")));
  Put(Index(Str(N..Str'Last), "-")); New_Line;
  Put_Line (Str(N..-1 + Index(Str(N..Str'Last), "+")));
  Put(Index(Str(N..Str'Last), "+")); New_Line;
  --Put_Line (Str (1..1 + N -1 + N + M - 2));
  --Put_Line (Str (1 + N - 1..Str'Last));
  --Put_Line (Str (1..Str'Last - 1));
  --Put_Line (Head (Tail (Str, Str'Last - Index (Str, "d", 1)), M));
  --Put_Line (Head (Tail (Str, Str'Last - Index (Str, "de", 1) - 1), M));
end Test_Slices;
