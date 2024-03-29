-- with GNAT.Debug_Pools;
with System.Pool_Local;

with Ada.Unchecked_Deallocation;

package Option_Processor is
   
   UNKNOWN_OPTION : exception;
   AMBIGUOUS_OPTION : exception;
   MISSING_OPTION_ARGUMENT : exception;
   OPTION_VALUE_TOO_LONG : exception;
   
   HELP_PRINTED : exception;
   
   -- Package configuration variables:
   
   type Configuration_Type is record
      Read_STDIN_If_No_Files : Boolean := True;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True;
   end record;
   
   Package_Configuration : Configuration_Type;
   
   procedure Configure
     (
      Read_STDIN_If_No_Files : Boolean := True;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True
     );
   
   -- Storage management:
   
   -- See
   --  https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/gnat_and_program_execution.html#id56
   --  [acessed 2023-11-26T12:14+02:00]:
   
   Reclaiming_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   -- Reclaiming_Pool : GNAT.Debug_Pools.Debug_Pool;
   
   type Option_Value_String is access String;
   for Option_Value_String'Storage_Pool use Reclaiming_Pool;
   
   type String_Access is access String;
   for String_Access'Storage_Pool use Reclaiming_Pool;
   
   type Option_Value_Kind is
     (
      STRING_OPT, INTEGER_OPT, FLOAT_OPT, DOUBLE_OPT, NATURAL_OPT,
      POSITIVE_OPT, CHARACTER_OPT, BOOLEAN_TRUE_OPT, BOOLEAN_FALSE_OPT,
      FLAG_OPT, HELP_OPT
     );
   
   type Option_Value_Type (Option_Kind : Option_Value_Kind := STRING_OPT)
      is record
      case Option_Kind is
         when STRING_OPT =>
            String_Value : Option_Value_String := new String'("");
         when INTEGER_OPT =>
            Integer_Value : Integer := 0;
         when FLOAT_OPT =>
            Float_Value : Float := 0.0;
         when DOUBLE_OPT =>
            Double_Value : Long_Float := 0.0;
         when NATURAL_OPT =>
            Natural_Value : Natural := 0;
         when POSITIVE_OPT =>
            Positive_Value : Positive := 1;
         when CHARACTER_OPT =>
            Character_Value : Character := ' ';
         when BOOLEAN_TRUE_OPT | BOOLEAN_FALSE_OPT =>
            Boolean_Value : Boolean := False;
         when FLAG_OPT =>
            Flag_Value : Option_Value_String := new String'("");
         when HELP_OPT =>
            null;
      end case;
   end record;
   
   type Option_Value_Access is access Option_Value_Type;
   for Option_Value_Access'Storage_Pool use Reclaiming_Pool;
   
   type Option_Type is record
      Short_Option_Prefix : Character; -- '-' or, optionally, '+';
      Short_Option : Character;
      Short_Option_Suffix : Character; -- '+' or '-' for negation;
      Long_Option : String_Access;
      Option_Kind : Option_Value_Kind;
      Value : Option_Value_Access;
      Is_Present : Boolean := False;
      Negated : Boolean := False;
      Help : Option_Value_String;
   end record;
   
   Default_Short_Option_Value : constant Character := ' ';
   Default_Short_Option_Suffix : constant Character := ' ';
   
   type Option_Array is array (Positive range <>) of Option_Type;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Option_Kind : Option_Value_Kind;
      Value_Ref : Option_Value_Access;
      Help : String := ""
     ) return Option_Type;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Option_Kind : Option_Value_Kind;
      Help : String := ""
     ) return Option_Type;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Value_Ref : Option_Value_Access;
      Help : String := ""
     ) return Option_Type;
   
   function Help_Option
     (
      Short_Option, Long_Option : String;
      Help : String := ""
     ) return Option_Type;

   type File_Index_Array is array (Positive range <>) of Natural;
   
   type File_Index_Array_Access is access File_Index_Array;
   for File_Index_Array_Access'Storage_pool use Reclaiming_Pool;
   
   function Get_Options
     (
      Options : in out Option_Array;
      Help_Printer : access procedure := null
     ) return File_Index_Array;
   
   function Get_Options
     (
      Options : in out Option_Array;
      Read_STDIN_If_No_Files : Boolean;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True;
      Help_Printer : access procedure := null
     ) return File_Index_Array;
   
   -- ------------------------------------------------------------------------
   
   function Option_Value_As_String (Value : in Option_Value_Type) return String;
   
   procedure Put (Value : Option_Value_Access);
   
   -- ------------------------------------------------------------------------
   
   function File_Name_Count return Natural;
   
   function Get_File_Name (N : Natural) return String;
   
   function Get_File_Argument_Index (N : Natural) return Natural;
      
   function Get_File_Indices return File_Index_Array;
   
   procedure Process_Options
     (
      Options : in out Option_Array;
      Help_Printer : access procedure := null
     );
   
   procedure Process_Options
     (
      Options : in out Option_Array;
      Read_STDIN_If_No_Files : Boolean;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True;
      Help_Printer : access procedure := null
     );
   
   -- ------------------------------------------------------------------------
   
   function Get_Option_Value
     (
      Option_Name : String;
      Options : Option_Array
     ) return Option_Value_Type;
      
   function Get_String_Option_Value
     (
      Option_Name : String;
      Options : Option_Array
     ) return String;
     
   function Get_Integer_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Integer;
   
   function Get_Float_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Float;
   
   function Get_Double_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Long_Float;
   
   function Get_Natural_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Natural;
   
   function Get_Positive_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Positive;
   
   function Get_Character_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Character;
   
   function Get_Boolean_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Boolean;
   
end Option_Processor;
