package Generic_Number is

  Invalid_Type : exception;

  -- Stict mode disallows setting conflicting types
  -- Override changes the underlying type
  --  (only works if declared as Number and not Number(xxx_Type)
  -- Convert will case the provided value
  -- Mode currently isn't in use, the default is override
  type Mode is (Strict, Override, Convert);

  type Number_Type is (Float_Type, Integer_Type);
  
  type Number(Option : Number_Type := Float_Type) is
    record
      Num_Type : Number_Type := Option;

      case Option is
        when Float_Type =>
          Float_Value : Float := 0.0;
        when Integer_Type =>
          Integer_Value : Integer := 0;
       end case;
    end record;

  procedure Set(this : out Number; val : Float); 
  procedure Set(this : out Number; val : Integer); 
 
  -- `Num op Num` will call `Num op Int` or something else
  -- depending on the type of the number. Integer division
  -- will convert the number to a float type. The new value
  -- is returned, so you must use the assignment operator if
  -- want the changes applied to the number you used the operation on.
  function "+" (A : Number; B : Number) return Number;
  function "+" (A : Number; B : Integer) return Number;
  function "+" (A : Integer; B : Number) return Number;
  function "+" (A : Number; B : Float) return Number;
  
  function "-" (A : Number; B : Number) return Number;
  function "-" (A : Number; B : Integer) return Number;
  function "-" (A : Integer; B : Number) return Number;
  function "-" (A : Number; B : Float) return Number;
  
  function "*" (A : Number; B : Number) return Number;
  function "*" (A : Number; B : Integer) return Number;
  function "*" (A : Integer; B : Number) return Number;
  function "*" (A : Number; B : Float) return Number;
  
  function "/" (A : Number; B : Number) return Number;
  function "/" (A : Number; B : Integer) return Number;
  function "/" (A : Integer; B : Number) return Number;
  function "/" (A : Number; B : Float) return Number;
  
  procedure Put(N : Number);

end Generic_Number;

