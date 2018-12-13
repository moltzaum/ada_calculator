package body Unbound_Stack is

	type Cell is record
		Item : Item_Type;
		Next : Stack;
	end record;
	
	procedure Push (Item : in Item_Type; Onto : in out Stack) is
	begin
		Onto := new Cell'(Item => Item, Next => Onto);
	end Push;
	
	procedure Pop (Item : out Item_Type; From : in out Stack) is
	begin
		if Is_Empty(From) then
			raise Underflow;
		end if;
		Item := From.Item;
		From := From.Next;
	end Pop;
	
	function Is_Empty (S : Stack) return Boolean is
	begin
		return S = null;
	end Is_Empty;
	
	function Top (S : Stack) return Item_Type is
	begin
	    if Is_Empty(S) then
		    raise Underflow;
		end if;
		return S.Item;
	end Top;
			
end Unbound_Stack;

