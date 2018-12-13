with Ada.Text_IO, Ada.Integer_Text_IO, Unbound_Stack;
use Ada.Text_IO, Ada.Integer_Text_IO;

procedure String_Reverse is
	package Unbound_Character_Stack is new Unbound_Stack (Character);
	use Unbound_Character_Stack;
	Buffer : String (1..80);
	N : Natural;
	Char_Stack : Unbound_Character_Stack.Stack;
begin

	Get_Line (Buffer, N);
	for i in Natural range 1..N loop
		Push (Buffer(i), onto => Char_Stack);
	end loop;
	while not Is_Empty(Char_Stack) loop
		declare
			Ch : Character;
		begin
			Pop (Ch, from => Char_Stack);
			Put (Ch);
		end;
	end loop;
	New_Line;
end String_Reverse;
