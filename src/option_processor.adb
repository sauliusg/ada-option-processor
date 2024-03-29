with Text_IO;           use Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Command_Line;  use Ada.Command_Line;

with Ada.Unchecked_Deallocation;

package body Option_Processor is
   
   procedure Configure
     (
      Read_STDIN_If_No_Files : Boolean := True;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True
     ) is
   begin
      Package_Configuration.Read_STDIN_If_No_Files := Read_STDIN_If_No_Files;
      Package_Configuration.Treat_Single_Dash_As_STDIN := Treat_Single_Dash_As_STDIN;
      Package_Configuration.Tread_Double_Dash_As_End_Of_Options := Tread_Double_Dash_As_End_Of_Options;
      Package_Configuration.Leading_Plus_Starts_Short_Option := Leading_Plus_Starts_Short_Option;
   end;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Option_Kind : Option_Value_Kind;
      Value_Ref : Option_Value_Access;
      Help : String := ""
     ) return Option_Type is
   begin
      return 
        (
         (if Short_Option'Length > 0
            then Short_Option (Short_Option'First)
            else ' '),
         (if Short_Option'Length > 1
            then Short_Option (Short_Option'First + 1)
            else Default_Short_Option_Value),
         (if Short_Option'Length > 2
            then Short_Option (Short_Option'First + 2)
            else Default_Short_Option_Suffix),
         new String'(Long_Option),
         Option_Kind,
         Value_Ref,
         Help => (if Help /= "" then new String'(Help) else null),
         others => <>
        );
   end;

   function Option
     (
      Short_Option, Long_Option : String;
      Option_Kind : Option_Value_Kind;
      Help : String := ""
     ) return Option_Type is
   begin
      return Option
        (
         Short_Option, Long_Option, Option_Kind,
         new Option_Value_Type (Option_Kind),
         Help
        );
   end;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Value_Ref : Option_Value_Access;
      Help : String := ""
     ) return Option_Type is
   begin
      return Option
        (
         Short_Option, Long_Option,
         Value_Ref.Option_Kind, Value_Ref,
         Help
        );
   end;
   
   function Help_Option
     (
      Short_Option, Long_Option : String;
      Help : String := ""
     ) return Option_Type is
   begin
      return Option
        (
         Short_Option, Long_Option,
         HELP_OPT,
         new Option_Value_Type (HELP_OPT),
         Help
        );
   end;
   
   -- -------------------------------------------------------------------------
   
   procedure Free is new Ada.Unchecked_Deallocation
     (String, Option_Value_String);
   
   function Value_Image_Length (Value : Option_Value_Access) return Natural is
   begin
      case Value.Option_Kind is
         when STRING_OPT    => return Value.String_Value.all'Length;
         when INTEGER_OPT   => return Value.Integer_Value'Image'Length;
         when FLOAT_OPT     => return Value.Float_Value'Image'Length;
         when DOUBLE_OPT    => return Value.Double_Value'Image'Length;
         when NATURAL_OPT   => return Value.Natural_Value'Image'Length;
         when POSITIVE_OPT  => return Value.Positive_Value'Image'Length;
         when CHARACTER_OPT => return Value.Character_Value'Image'Length;
         when others => return 0;
      end case;
   end;
   
   function Value_Length (Value : Option_Value_Access) return Natural is
   begin
      case Value.Option_Kind is
         when STRING_OPT =>
            return Value_Image_Length (Value) + 3;
         when others => 
            return (if Value_Image_Length (Value) <= 2 
                      then 0 
                      else Value_Image_Length (Value) - 2);
      end case;
   end;
      
   function Option_Value_As_String (Value : in Option_Value_Type) return String is
   begin
      case Value.Option_Kind is
         when STRING_OPT    => return Value.String_Value.all;
         when INTEGER_OPT   => return Value.Integer_Value'Image;
         when FLOAT_OPT     => return Value.Float_Value'Image;
         when DOUBLE_OPT    => return Value.Double_Value'Image;
         when NATURAL_OPT   => return Value.Natural_Value'Image;
         when POSITIVE_OPT  => return Value.Positive_Value'Image;
         when CHARACTER_OPT => return Value.Character_Value'Image;
         when FLAG_OPT      => return Value.Flag_Value.all;
         when BOOLEAN_TRUE_OPT | BOOLEAN_FALSE_OPT =>
            return Value.Boolean_Value'Image;
         when HELP_OPT =>
            return "";
      end case;
   end;
      
   procedure Put (Value : Option_Value_Access) is
   begin
      if Value.Option_Kind /= FLAG_OPT then
         Put (Option_Value_As_String (Value.all));
      end if;
   end;
   
   procedure Put_Default_Option_Description (Option : Option_Type) is
   begin
      case Option.Option_Kind is
         when STRING_OPT    => Put ("specify a string value");
         when INTEGER_OPT   => Put ("specify an integer value");
         when FLOAT_OPT     => Put ("specify a float value");
         when DOUBLE_OPT    => Put ("specify a double value");
         when NATURAL_OPT   => Put ("specify a natural value '>= 0'");
         when POSITIVE_OPT  => Put ("specify a positive value");
         when CHARACTER_OPT => Put ("specify a single ASCII character");
         when HELP_OPT      => Put ("print a short help message and exit");
         when others => Put ("toggle the option");
      end case;      
   end;
   
   procedure Put_Option_Description (Option : Option_Type) is
   begin
      if Option.Help /= null then
         Put (Option.Help.all);
      else
         Put_Default_Option_Description (Option);
      end if;
   end;
   
   procedure Put_Option_Help (Options : Option_Array) is
      Short_Option : String (1..3);
      Max_Value_Length : Natural := 0;
      Max_Long_Option_Length : Natural := 0;
      Option_Start_Position : Positive := 3;
      Help_Text_Position : Positive := Option_Start_Position + Short_Option'Length;
   begin
      for I in Options'Range loop
         if Max_Value_Length < Value_Length (Options(I).Value) then
            Max_Value_Length := Value_Length (Options(I).Value);
         end if;
         if Max_Long_Option_Length < Options(I).Long_Option.all'Length then
            Max_Long_Option_Length := Options(I).Long_Option.all'Length;
         end if;
      end loop;
      Help_Text_Position :=
        Help_Text_Position + Max_Value_Length + Max_Long_Option_Length;
      Put_Line ("OPTIONS:");
      for I in Options'Range loop
         Short_Option := 
           (
            Options(I).Short_Option_Prefix,
            Options(I).Short_Option,
            Options(I).Short_Option_Suffix
           );
         Set_Col (Text_IO.COunt (Option_Start_Position));
         Put (Short_Option);
         if Short_Option /= 3 * " " then
            Put (",");
         else 
            Put (" ");
         end if;
         Put (" ");
         Put (Options(I).Long_Option.all);
         Put (Options(I).Value);
         Set_Col (Text_IO.Count (Help_Text_Position));
         Put_Option_Description (Options(I));
         New_Line;
      end loop;
   end;
   
   function Get_Value_String (Option_Index : in out Integer) return String is
      Value_Separator : String := "=";
      Value_Sep_Pos : Integer := Index (Argument (Option_Index), Value_Separator);
   begin
      if Value_Sep_Pos >= Argument (Option_Index)'First then
         return Argument (Option_Index) (Value_Sep_Pos + 1 .. Argument (Option_Index)'Last);
      else
         Option_Index := Option_Index + 1;
         if Option_Index <= Argument_Count then
            return Argument (Option_Index);
         else
            raise MISSING_OPTION_ARGUMENT;
         end if;
      end if;
   end;
   
   procedure Process_Option 
     (
      Cmd_Option : String;
      Option_Index : in out Integer;
      Option : in out Option_Type;
      Options : in Option_Array;
      Help_Printer : access procedure
     ) is
   begin
      case Option.Option_Kind is
         when STRING_OPT =>
            Free (Option.Value.String_Value);
            Option.Value.String_Value := new String'(Get_Value_String (Option_Index));
         when INTEGER_OPT =>
            Option.Value.Integer_Value := Integer'Value (Get_Value_String (Option_Index));
         when FLOAT_OPT =>
            Option.Value.Float_Value := Float'Value (Get_Value_String (Option_Index));
         when DOUBLE_OPT =>
            Option.Value.Double_Value := Long_Float'Value (Get_Value_String (Option_Index));
         when NATURAL_OPT =>
            Option.Value.Natural_Value := Natural'Value (Get_Value_String (Option_Index));
         when POSITIVE_OPT =>
            Option.Value.Positive_Value := Positive'Value (Get_Value_String (Option_Index));
         when CHARACTER_OPT =>
            declare
               Value_String : String := Get_Value_String (Option_Index);
            begin
               if Value_String'Length > 1 then
                  raise OPTION_VALUE_TOO_LONG with
                    "value '" & Value_String & "' too long for character option '" &
                    Cmd_Option & "'";
               else
                  Option.Value.Character_Value := Value_String (Value_String'First);
               end if;
            end;
         when BOOLEAN_TRUE_OPT =>
            Option.Value.Boolean_Value := True;
         when BOOLEAN_FALSE_OPT =>
            Option.Value.Boolean_Value := False;
         when FLAG_OPT =>
            Free (Option.Value.Flag_Value);
            Option.Value.Flag_Value := new String'(Cmd_Option);
         when HELP_OPT =>
            if Help_Printer /= null then
               Help_Printer.all;
            end if;
            Put_Option_Help (Options);
            raise HELP_PRINTED;
         when others =>
            raise UNKNOWN_OPTION with
              "INTERNAL ERROR -- unknown option kind '" &
              Option.Option_Kind'Image & "' for option " &
              Cmd_Option & "'";
      end case;
      Option.Is_Present := True;
   exception
      when MISSING_OPTION_ARGUMENT =>
         raise MISSING_OPTION_ARGUMENT with
           "option '" & Cmd_Option & "' requires a value";
   end;
   
   type Index_List_Type is array (Positive range <>) of Integer;
   
   function Concatenate_Options
     (
      Found_Instance_Count : Integer;
      Found_Instances : Index_List_Type;
      Options : Option_Array
     ) return String
   is
      Option_List_Length : Integer := 0;
   begin
      for X in 1 .. Found_Instance_Count loop
         Option_List_Length := Option_List_Length +
           Options (Found_Instances (X)).Long_Option.all'Length;
         if X > 1 then
            Option_List_Length := Option_List_Length + 2;
         end if;
      end loop;
      declare
         Pos : Integer := 1;
         Len : Integer;
         Option_List : String (1 .. Option_List_Length);
      begin
         for X in 1 .. Found_Instance_Count loop
            if X > 1 then
               Option_List (Pos .. Pos + 1) := ", ";
               Pos := Pos + 2;
            end if;
            Len := Options (Found_Instances (X)).Long_Option.all'Length;
            Option_List (Pos .. Pos + Len - 1) :=
              Options (Found_Instances (X)).Long_Option.all;
            Pos := Pos + Len;
         end loop;
         return Option_List;
      end;
   end;
   
   procedure Get_Option
     (
      Option_String : String;
      Option_Index : in out Positive;
      Options : in out Option_Array;
      Help_Printer : access procedure
     )
   is
      Found_Instance_Count : Integer := 0;
      Found_Instances : Index_List_Type (1 .. Options'Length);
   begin
      
      if (Option_String (Option_String'First) = '-' or else 
            Option_String (Option_String'First) = '+')
        and then 
        Option_String'Length > 1 and then
        Option_String (Option_String'First + 1) /= '-'
      then
         for I in Options'Range loop
            declare
               Option_Letter_Idx : constant Integer := Option_String'First + 1;
            begin
               if Option_String (Option_String'First) = Options (I).Short_Option_Prefix and then
                 Option_String (Option_Letter_Idx) = Options (I).Short_Option and then
                 ((Option_String'Length = 2 and then
                     Options (I).Short_Option_Suffix = Default_Short_Option_Suffix)
                  or else
                    (Option_String'Length = 3 and then 
                       Option_String (Option_Letter_Idx + 1) = Options (I).Short_Option_suffix))
               then
                  Process_Option 
                    (
                     Option_String,
                     Option_Index,
                     Options (I),
                     Options,
                     Help_Printer
                    );
                  return;
               end if;
            end;
         end loop;
         raise UNKNOWN_OPTION with
           "unknown short option '" & Option_String & "'";
      else
         declare
            Value_Separator : String := "=";
            Value_Sep_Pos : Integer := Index (Option_String, Value_Separator);
            Option_Only : String :=
              (if Value_Sep_Pos >= Option_String'First 
                 then Option_String (Option_String'First .. Value_Sep_Pos - 1)
                 else Option_String);
         begin
            for I in Options'Range loop
               if Index (Options (I).Long_Option.all, Option_Only) = 1 then
                  Found_Instance_Count := Found_Instance_Count + 1;
                  Found_Instances (Found_Instance_Count) := I;
               end if;
            end loop;
            if Found_Instance_Count = 1 then
               Process_Option
                 (
                  Option_Only,
                  Option_Index,
                  Options (Found_Instances (1)),
                  Options,
                  Help_Printer
                 );
            elsif Found_Instance_Count > 1 then
               raise AMBIGUOUS_OPTION with
                 "Option '" & Option_Only & "' is not unique; possible candidates are: " &
                 Concatenate_Options (Found_Instance_Count, Found_Instances, Options);
            elsif Found_Instance_Count = 0 then
               raise UNKNOWN_OPTION with
                 "unknown long option '" & Option_Only & "'";
            end if;
         end;
      end if;
   end;
   
   function Get_Options
     (
      Options : in out Option_Array;
      Help_Printer : access procedure := null
     ) return File_Index_Array
   is
      Index : Positive := 1;
      Files : File_Index_Array (1 .. Argument_Count + 1) := (others => 0);
      N_Files : Natural := 0;
      Argument_Idx : Positive := 1;
      No_More_Options : Boolean := False;
   begin
      while Argument_Idx <= Argument_Count loop
         declare
            Option_String : String := Argument (Argument_Idx);
         begin
            if not No_More_Options and then
              (Option_String (Option_String'First) = '-' or else 
                 (Package_Configuration.Leading_Plus_Starts_Short_Option and then
                    Option_String (Option_String'First) = '+'))
              and then 
              Option_String'Length > 1 then
               if Package_Configuration.Tread_Double_Dash_As_End_Of_Options and then 
                 Option_String = "--" then
                  No_More_Options := True;
               else
                  Get_Option (Option_String, Argument_Idx, Options, Help_Printer);
               end if;
            else
               N_Files := N_Files + 1;
               if Package_Configuration.Treat_Single_Dash_As_STDIN and then
                 Option_String = "-" then
                  Files (N_Files) := 0;
               else
                  Files (N_Files) := Argument_Idx;
               end if;
            end if;
         end;
         Argument_Idx := Argument_Idx + 1;
      end loop;
      if Package_Configuration.Read_STDIN_If_No_Files and then N_Files = 0 then
         N_Files := 1;
         Files (N_Files) := 0;
      end if;
      return Files (1 .. N_Files);
   end;
   
   function Get_Options
     (
      Options : in out Option_Array;
      Read_STDIN_If_No_Files : Boolean;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True;
      Help_Printer : access procedure := null
     ) return File_Index_Array
   is
      Old_Package_Configuration : Configuration_Type := Package_Configuration;
   begin
      Package_Configuration.Read_STDIN_If_No_Files := Read_STDIN_If_No_Files;
      Package_Configuration.Treat_Single_Dash_As_STDIN := Treat_Single_Dash_As_STDIN;
      Package_Configuration.Tread_Double_Dash_As_End_Of_Options := Tread_Double_Dash_As_End_Of_Options;
      Package_Configuration.Leading_Plus_Starts_Short_Option := Leading_Plus_Starts_Short_Option;

      declare
         File_Indices : File_Index_Array := Get_Options (Options, Help_Printer);
      begin
         Package_Configuration := Old_Package_Configuration;
         return File_Indices;
      end;
   end;
   
   -- ------------------------------------------------------------------------
   
   File_Indices : File_Index_Array_Access;
       
   procedure Free is new Ada.Unchecked_Deallocation
     (File_Index_Array, File_Index_Array_Access);
       
   function File_Name_Count return Natural is
   begin
      if File_Indices = null then
         return 0;
      else
         return File_Indices.all'Length;
      end if;
   end;
   
   function Get_File_Name (N : Natural) return String is
   begin
      if File_Indices = null then
         return "";
      elsif File_Indices (N) = 0 then
         return "-";
      else
         return Argument (File_Indices (N));
      end if;
   end;
   
   function Get_File_Argument_Index (N : Natural) return Natural is
   begin
      if File_Indices = null then
         return 0;
      elsif N = 0 then
         return 0;
      else
         return File_Indices (N);
      end if;
   end;
   
   function Get_File_Indices return File_Index_Array is
   begin
      if File_Indices = null then
         return (1..0 => 0);
      else
         return File_Indices.all;
      end if;
   end;
   
   procedure Process_Options
     (
      Options : in out Option_Array;
      Help_Printer : access procedure := null
     ) is
   begin
      Free (File_Indices);
      File_Indices := new File_Index_Array'(Get_Options (Options, Help_Printer));
   end;
   
   procedure Process_Options
     (
      Options : in out Option_Array;
      Read_STDIN_If_No_Files : Boolean;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True;
      Leading_Plus_Starts_Short_Option : Boolean := True;
      Help_Printer : access procedure := null
     ) is
   begin
      Free (File_Indices);
      File_Indices := new File_Index_Array'
        (
         Get_Options
           (
            Options,
            Read_STDIN_If_No_Files,
            Treat_Single_Dash_As_STDIN,
            Tread_Double_Dash_As_End_Of_Options,
            Leading_Plus_Starts_Short_Option,
            Help_Printer
           )
        );
   end;
   
   -- -------------------------------------------------------------------------
   
   function Get_Option_Value
     (
      Option_Name : String;
      Options : Option_Array
     ) return Option_Value_Type
   is
   begin
      for Opt of Options loop
         if Opt.Long_Option.all = Option_Name then
            return Opt.Value.all;
         end if;
      end loop;
      raise UNKNOWN_OPTION with
        "value requested for unknown option " &
        "'" & Option_Name & "'";
   end;
   
   function Get_String_Option_Value
     (
      Option_Name : String;
      Options : Option_Array
     ) return String is
   begin
      return
        Option_Value_As_String (Get_Option_Value (Option_Name, Options));
   end;
   
   function Get_Integer_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Integer
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Integer_Value;
   end;
   
   function Get_Float_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Float
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Float_Value;
   end;
   
   function Get_Double_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Long_Float
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Double_Value;
   end;
   
   function Get_Natural_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Natural
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Natural_Value;
   end;
   
   function Get_Positive_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Positive
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Positive_Value;
   end;
   
   function Get_Character_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Character
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Character_Value;
   end;
   
   function Get_Boolean_Option_Value 
     (
      Option_Name : String;
      Options : Option_Array
     ) return Boolean
   is
   begin
      return
        Get_Option_Value (Option_Name, Options).Boolean_Value;
   end;
   
end Option_Processor;
