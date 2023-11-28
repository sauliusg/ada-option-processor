with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Command_Line;  use Ada.Command_Line;

with Ada.Unchecked_Deallocation;

package body Option_Processor is
   
   procedure Configure
     (
      Read_STDIN_If_No_Files : Boolean := True;
      Treat_Single_Dash_As_STDIN : Boolean := True;
      Tread_Double_Dash_As_End_Of_Options : Boolean := True
     ) is
   begin
      Package_Configuration.Read_STDIN_If_No_Files := Read_STDIN_If_No_Files;
      Package_Configuration.Treat_Single_Dash_As_STDIN := Treat_Single_Dash_As_STDIN;
      Package_Configuration.Tread_Double_Dash_As_End_Of_Options := Tread_Double_Dash_As_End_Of_Options;
   end;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Option_Kind : Option_Value_Kind;
      Value_Ref : Option_Value_Access
     ) return Option_Type is
   begin
      return 
        (
         (if Short_Option'Length > 1
            then Short_Option (Short_Option'First + 1)
            else Default_Short_Option_Value),
         (if Short_Option'Length > 2
            then Short_Option (Short_Option'First + 2)
            else Default_Short_Option_Suffix),
         new String'(Long_Option),
         Option_Kind,
         Value_Ref,
         others => <>
        );
   end;

   function Option
     (
      Short_Option, Long_Option : String;
      Option_Kind : Option_Value_Kind
     ) return Option_Type is
   begin
      return Option
        (
         Short_Option, Long_Option, Option_Kind,
         new Option_Value_Type (Option_Kind)
        );
   end;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Value_Ref : Option_Value_Access
     ) return Option_Type is
   begin
      return Option
        (
         Short_Option, Long_Option,
         Value_Ref.Option_Kind, Value_Ref
        );
   end;
   
   function Option
     (
      Short_Option, Long_Option : String;
      Processor : access procedure
        (Option_String : String; Position : in out Positive)
     ) return Option_Type is
   begin
      return Option
        (
         Short_Option, Long_Option,
         FUNCTION_OPT,
         new Option_Value_Type'
           (
            Option_Kind => FUNCTION_OPT,
            Process => Processor
           )
        );
   end;
   
   -- -------------------------------------------------------------------------
   
   procedure Free is new Ada.Unchecked_Deallocation
     (String, Option_Value_String);
   
   procedure Process_Option 
     (
      Cmd_Option : String;
      Option_Index : in out Integer;
      Option : in out Option_Type
     ) is
   begin
      case Option.Option_Kind is
         when STRING_OPT =>
            Option_Index := Option_Index + 1;
            Free (Option.Value.String_Value);
            Option.Value.String_Value := new String'(Argument (Option_Index));
         when INTEGER_OPT =>
            Option_Index := Option_Index + 1;
            Option.Value.Integer_Value := Integer'Value (Argument (Option_Index));
         when FLOAT_OPT =>
            Option_Index := Option_Index + 1;
            Option.Value.Float_Value := Float'Value (Argument (Option_Index));
         when DOUBLE_OPT =>
            Option_Index := Option_Index + 1;
            Option.Value.Double_Value := Long_Float'Value (Argument (Option_Index));
         when NATURAL_OPT =>
            Option_Index := Option_Index + 1;
            Option.Value.Natural_Value := Natural'Value (Argument (Option_Index));
         when POSITIVE_OPT =>
            Option_Index := Option_Index + 1;
            Option.Value.Positive_Value := Positive'Value (Argument (Option_Index));
         when CHARACTER_OPT =>
            Option_Index := Option_Index + 1;
            Option.Value.Character_Value := Argument (Option_Index)(1);
         when BOOLEAN_TRUE_OPT =>
            Option.Value.Boolean_Value := True;
         when BOOLEAN_FALSE_OPT =>
            Option.Value.Boolean_Value := False;
         when FUNCTION_OPT =>
            Option.Value.Process (Option.Long_Option.all, Option_Index);
         when others =>
            raise UNKNOWN_OPTION with
              "INTERNAL ERROR -- unknown option kind '" &
              Option.Option_Kind'Image & "' for option " &
              Cmd_Option & "'";
      end case;
      Option.Is_Present := True;
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
      Options : in out Option_Array
     )
   is
      Found_Instance_Count : Integer := 0;
      Found_Instances : Index_List_Type (1 .. Options'Length);
   begin
      
      if Option_String (Option_String'First) = '-' and then 
        Option_String'Length > 1 and then
        Option_String (Option_String'First + 1) /= '-'
      then
         for I in Options'Range loop
            declare
               Option_Letter_Idx : constant Integer := Option_String'First + 1;
            begin
               if Option_String (Option_Letter_Idx) = Options (I).Short_Option and then
                 ((Option_String'Length = 2 and then
                     Options (I).Short_Option_Suffix = Default_Short_Option_Suffix)
                  or else
                    (Option_String'Length = 3 and then 
                       Option_String (Option_Letter_Idx + 1) = Options (I).Short_Option_suffix))
               then
                  Process_Option (
                     Option_String,
                     Option_Index,
                     Options (I)
                    );
                  return;
               end if;
            end;
         end loop;
         raise UNKNOWN_OPTION with
           "unknown short option '" & Option_String & "'";
      else
         for I in Options'Range loop
            if Index (Options (I).Long_Option.all, Option_String) = 1 then
               Found_Instance_Count := Found_Instance_Count + 1;
               Found_Instances (Found_Instance_Count) := I;
               if Found_Instance_Count = 1 then
                  Process_Option
                    (
                     Option_String,
                     Option_Index,
                     Options (I)
                    );
               end if;
            end if;
         end loop;
         if Found_Instance_Count > 1 then
            raise AMBIGUOUS_OPTION with
              "Option '" & Option_String & "' is not unique; possible candidates are: " &
              Concatenate_Options (Found_Instance_Count, Found_Instances, Options);
         elsif Found_Instance_Count = 0 then
            raise UNKNOWN_OPTION with
              "unknown long option '" & Option_String & "'";
         end if;
      end if;
   end;
   
   function Get_Options (Options : in out Option_Array) return File_Index_Array
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
              Option_String (Option_String'First) = '-' and then 
              Option_String'Length > 1 then
               if Package_Configuration.Tread_Double_Dash_As_End_Of_Options and then 
                 Option_String = "--" then
                  No_More_Options := True;
               else
                  Get_Option (Option_String, Argument_Idx, Options);
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
      Tread_Double_Dash_As_End_Of_Options : Boolean := True
     ) return File_Index_Array
   is
      Old_Package_Configuration : Configuration_Type := Package_Configuration;
   begin
      Package_Configuration.Read_STDIN_If_No_Files := Read_STDIN_If_No_Files;
      Package_Configuration.Treat_Single_Dash_As_STDIN := Treat_Single_Dash_As_STDIN;
      Package_Configuration.Tread_Double_Dash_As_End_Of_Options := Treat_Single_Dash_As_STDIN;
      
      declare
         File_Indices : File_Index_Array := Get_Options (Options);
      begin
         Package_Configuration := Old_Package_Configuration;
         return File_Indices;
      end;
   end;
   
end Option_Processor;
