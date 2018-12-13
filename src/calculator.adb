with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO, Unbound_Stack;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Generic_Number; use Generic_Number;

procedure Calculator is

	package Unbound_Character_Stack is new Unbound_Stack(Character);
	package Unbound_Number_Stack is new Unbound_Stack(Number);
  use Unbound_Character_Stack, Unbound_Number_Stack;

	Buffer : String(1..1000);
	Last : Natural;
  Result : Number;
	Expression_Error : exception;
	
	-- Evaluates the integer expression in Buffer and returns the result. An
	-- Expression_Error is raised if the expression has an error;
  function Evaluate return Number is
	  Operator_Stack : Unbound_Character_Stack.Stack;
	  Operand_Stack : Unbound_Number_Stack.Stack;
    Char : Character;
    Result : Number;
    Idx : Integer := 1;
    Expect_Operand : Boolean := true;

    -- If A = 0 set A to be maximum integer value
    function Invalidate_Zero(A : Integer) return Integer is
    begin
      if A = 0 then return Integer'Last;
      else return A; end if;
    end Invalidate_Zero;

    function Min(A : Integer; B : Integer) return Integer is
    begin
      if A < B then return A;
      else return B; end if;
    end Min;

    -- Reads an operand from the buffer
    function Next_Operand return Number is
      Value : Integer := 0;
      Value2 : Float := 0.0;
      Result : Number;
      Nearest : Integer := Integer'Last;
    begin
      -- I can grab a slice from the current index to the nearest operator
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), "*")));
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), "/")));
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), "+")));
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), "-")));
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), ")")));
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), "(")));
      Nearest := Min(Nearest, Invalidate_Zero(Index(Buffer(Idx..Buffer'Last), " ")));
      Nearest := Nearest - 1; 
      Nearest := Min(Nearest, Last);
   
      --Print what we have to validate what we have is correct
      --Put("'"); Put(Buffer(Idx..Nearest)); Put("'"); New_Line;

      -- If we can't convert to an integer, try float
      -- If we can't convert to a float, then the expression is invalid
      begin Set(Result, Integer'Value(Buffer(Idx..Nearest)));
      exception when Constraint_Error =>
          begin Set(Result, Float'Value(Buffer(Idx..Nearest)));
          exception when Constraint_Error =>
              raise Expression_Error;
          end;
      end;
      Idx := Idx + Buffer(Idx..Nearest)'Length;
      return Result;
    end Next_Operand;

	  -- Returns the precedence of Operator. Raises Exception_Error if
	  -- Operator is not a known operator.
	  function Precedence(Operator : Character) return Integer is
	  begin
	    case Operator is
        when '+' | '-' => return 0;
        when '*' | '/' => return 1;
        when others => raise Expression_Error;
      end case;
	  end Precedence;
	  
	  -- Applies the top operator on the Operator_Stack to its right and left
	  -- operands on the Operand Stack.
	  procedure ApplyOperator is
      Left : Number;
      Right : Number;
      Operator : Character;
	  begin
      Pop(Operator, from => Operator_Stack);
      Pop(Right, from => Operand_Stack);
      Pop(Left, from => Operand_Stack);
	    
      case Operator is
        when '+' => Push(Left + Right, Operand_Stack);
        when '-' => Push(Left - Right, Operand_Stack);
        when '*' => Push(Left * Right, Operand_Stack);
        when '/' => Push(Left / Right, Operand_Stack);
        when others => raise Expression_Error;
      end case;

	  end ApplyOperator;
	
  begin -- Evaluate
	  
    -- Process the expression left to right once character at a time.
	  while Idx <= Last loop

      Char := Buffer(Idx);

      -- This case statement takes care of handling Expect_Operand and advancing the cursor
      -- I can't advance the cursor in all cases since Next_Operand advances the cursor itself
      case Char is
        when '0'..'9' =>
          if not Expect_Operand then raise Expression_Error; end if;
          Expect_Operand := false;
        when '+' | '-' | '*' | '/' =>
          if Expect_Operand then raise Expression_Error; end if;
          Expect_Operand := true;
          Idx := Idx + 1;
        when '(' =>
          Expect_Operand := true;
          Idx := Idx + 1;
        when others => Idx := Idx + 1;
      end case;

      -- This case statement takes care of the rest of the logic
      case Char is
		   
        when '.' => NULL;
          --Push(Next_Operand(start => decimal), Operand_Stack); 
        when '0'..'9'  =>
				  Push(Next_Operand, Operand_Stack);
        
		    when '+' | '-' | '*' | '/' =>

          -- We never expect ')', since it will be applied immediately
          if Is_Empty(Operator_Stack) or else Top(Operator_Stack) = '(' then NULL; 
          elsif Precedence(Top(Operator_Stack)) >= Precedence(Char) then
            ApplyOperator;
          end if;
          Push(Char, onto => Operator_Stack);
        
        when '(' =>
          Push(Char, onto => Operator_Stack);

        when ')' =>
          while Top(Operator_Stack) /= '(' loop ApplyOperator; end loop;
          declare Empty : Character;
          begin Pop(Empty, from => Operator_Stack); end;

		    when ' ' => NULL;
		    when others => raise Expression_Error;
		  end case;

	  end loop;
	  
    -- Apply remaining operators, pop result, and if we have more operands we have an error
    while not Is_Empty(Operator_Stack) loop ApplyOperator; end loop;
    Pop(Result, from => Operand_Stack);
    if not Is_Empty(Operand_Stack) then raise Expression_Error; end if;
    return Result;
	  
    exception
	  	when Unbound_Character_Stack.Underflow |
	  	     Unbound_Number_Stack.Underflow =>
	  	    raise Expression_Error;
	  end Evaluate;
	
begin -- Calculator

  -- Evaluate all expressions in standard input (separated by new lines) and print the result. 
  while not End_of_File loop
    begin
      Get_Line(Buffer, Last);
		  Put_Line(Buffer(1..Last));
      Result := Evaluate;
      Put(Result);
      New_Line;
	  exception
      when Expression_Error => Put_Line("EXPRESSION ERROR");
		  when others => Put_Line("ERROR");
	  end;
  end loop;

end Calculator;

