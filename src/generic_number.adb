with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

package body Generic_Number is
  
  function "/" (A : Number; B : Number) return Number is
  begin
    case B.Num_Type is
      when Float_Type   => return A / B.Float_Value;
      when Integer_Type => return A / B.Integer_Value;
    end case;
  end "/";
  
  function "/" (A : Number; B : Integer) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => return A / Float(B);
      when Integer_Type =>
        if A.Integer_Value mod B = 0 then
          Set(result, A.Integer_Value / B);
        else
          Set(result, 0.0);
          result := result + A;
          result := result / B;
        end if;
    end case;
    return result;
  end "/";
  
  
  function "/" (A : Number; B : Float) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => Set(result, A.Float_Value / B);
      when Integer_Type => Set(result, Float(A.Integer_Value) / B);
    end case;
    return result;
  end "/";
  
  function "*" (A : Number; B : Number) return Number is
  begin
    case B.Num_Type is
      when Float_Type   => return A * B.Float_Value;
      when Integer_Type => return A * B.Integer_Value;
    end case;
  end "*";
  
  function "*" (A : Number; B : Integer) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => return A * Float(B);
      when Integer_Type => Set(result, A.Integer_Value * B);
    end case;
    return result;
  end "*";
  
  
  function "*" (A : Number; B : Float) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => Set(result, A.Float_Value * B);
      when Integer_Type => Set(result, B);
        return result * A.Integer_Value;
    end case;
    return result;
  end "*";
  
  function "-" (A : Number; B : Number) return Number is
  begin
    case B.Num_Type is
      when Float_Type   => return A - B.Float_Value;
      when Integer_Type => return A - B.Integer_Value;
    end case;
  end "-";
  
  function "-" (A : Number; B : Integer) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => return A - Float(B);
      when Integer_Type => Set(result, A.Integer_Value - B);
    end case;
    return result;
  end "-";
  
  
  function "-" (A : Number; B : Float) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => Set(result, A.Float_Value - B);
      when Integer_Type => Set(result, Float(A.Integer_Value) - B);
    end case;
    return result;
  end "-";

  function "+" (A : Number; B : Number) return Number is
  begin
    case B.Num_Type is
      when Float_Type   => return A + B.Float_Value;
      when Integer_Type => return A + B.Integer_Value;
    end case;
  end "+";
  
  function "+" (A : Number; B : Integer) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => return A + Float(B);
      when Integer_Type => Set(result, A.Integer_Value + B);
    end case;
    return result;
  end "+";
  
  
  function "+" (A : Number; B : Float) return Number is
    result : Number;
  begin
    case A.Num_Type is
      when Float_Type   => Set(result, A.Float_Value + B);
      when Integer_Type => Set(result, B);
        return result + A.Integer_Value;
    end case;
    return result;
  end "+";
  
  -- Define the operators the opposite way around
  function "+" (A : Integer; B : Number) return Number is
  begin return B + A; end "+";
  
  function "+" (A : Float; B : Number) return Number is
  begin return B + A; end "+";
  
  function "-" (A : Integer; B : Number) return Number is
  begin return B - A; end "-";
  
  function "-" (A : Float; B : Number) return Number is
  begin return B - A; end "-";
  
  function "*" (A : Integer; B : Number) return Number is
  begin return B * A; end "*";
  
  function "*" (A : Float; B : Number) return Number is
  begin return B * A; end "*";
  
  function "/" (A : Integer; B : Number) return Number is
  begin return B / A; end "/";
  
  function "/" (A : Float; B : Number) return Number is
  begin return B / A; end "/";
  
  procedure Set(this : out Number; val : Float) is
    New_Num : Number(Float_Type);
  begin
    case this.Num_Type is
      when Float_Type =>
        this.Float_Value := val;
      when Integer_Type =>
        Set(New_Num, val);
        this := New_Num;
    end case;
  end set;

  procedure Set(this : out Number; val : Integer) is
    New_Num : Number(Integer_Type);
  begin
    case this.Num_Type is
      when Float_Type =>
        Set(New_Num, val);
        this := New_Num;
      when Integer_Type =>
        this.Integer_Value := val;
    end case;
  end set;
  
  procedure Put(N : Number) is
  begin
   if N.Num_Type = Float_Type then
    Put(N.Float_Value);
   elsif N.Num_Type = Integer_Type then
    Put(N.Integer_Value);
   end if; 
  end Put;

end Generic_Number;

