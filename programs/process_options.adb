pragma Ada_2022;

with Text_IO;          use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Option_Processor; use Option_Processor;
with File_Selector;    use File_Selector;

with Ada.Directories;

procedure Process_Options is
   
   -- To check whether the library or the main program have memory
   -- leaks, compile and run with the following GNAT options:
   
   -- gnatmake -Isrc -D .obj/ \
   --     -o bin/process_options \
   --     programs/process_options.adb \
   --     -cargs -fsanitize=address -largs -fsanitize=address
   
   String_Parameter : Option_Value_Access :=
     new Option_Value_Type'
     (
      STRING_OPT,
      String_Value => new String'("string value")
     );
   
   Integer_Parameter : Option_Value_Access :=
     new Option_Value_Type (INTEGER_OPT);
   
   Float_Parameter   : Option_Value_Access :=
     new Option_Value_Type'(FLOAT_OPT, Float_Value => 3.14);
   
   Boolean_Affirmative_Switch : Option_Value_Access := 
     new Option_Value_Type'(BOOLEAN_TRUE_OPT, Boolean_Value => True);
   
   Boolean_Negative_Switch : Option_Value_Access := 
     new Option_Value_Type (BOOLEAN_FALSE_OPT);
   
   Option_Selector : Option_Value_Access :=
     new Option_Value_Type'(FLAG_OPT, new String'("--one"));
   
   procedure Help is
   begin
      Put_Line ("This is a help from the main program ...");
   end;
   
   type Flag_Type is (ONE, TWO, THREE);
   
   Flag : Flag_Type := ONE;
   
   WRONG_FLAG_OPTION : exception;
   
   procedure Set_Flag (Option_String : String) is
   begin
      if Option_String = "--one" then
         Flag := ONE;
      elsif Option_String = "--two" then
         Flag := TWO;
      elsif Option_String = "--tree" then
         Flag := THREE;
      else
         raise Wrong_Flag_Option with 
           "wrong option '" & Option_String &
           "' passed to the flag setter routine.";
      end if;
   end;
   
   Options : Option_Array :=
     (
      Help_Option ("-h", "--help"),
      Option ("-S", "--string",    String_Parameter),
      Option ("-1", "--one",       Option_Selector, Help => "Set selection to ONE"),
      Option ("-2", "--two",       Option_Selector, Help => "Set selection to TWO"),
      Option ("-3", "--tree",      Option_Selector, Help => "Set selection to THREE"),
      Option ("-x", "--xstrange",  DOUBLE_OPT),
      Option ("-i", "--int",       Integer_Parameter),
      Option ("-f", "--float",     Float_Parameter),
      Option ("-d", "--double",    DOUBLE_OPT),
      Option ("-N", "--natural",   NATURAL_OPT),
      Option ("-p", "--positive",  POSITIVE_OPT),
      Option ("-b", "--bool",      BOOLEAN_TRUE_OPT),
      Option ("-b+","",            BOOLEAN_TRUE_OPT),
      Option ("-b-","--no-bool",   BOOLEAN_FALSE_OPT),
      Option ("",   "--only-long", INTEGER_OPT),
      Option ("-c", "--char",      CHARACTER_OPT),
      
      Option ("-q ","--positive",  BOOLEAN_TRUE_OPT,  Boolean_Negative_Switch),
      Option ("-q+","",            BOOLEAN_TRUE_OPT,  Boolean_Negative_Switch),
      Option ("-q-","--negative",  BOOLEAN_FALSE_OPT, Boolean_Negative_Switch),
      Option ("+q", "",            BOOLEAN_FALSE_OPT, Boolean_Negative_Switch),
      
      Option ("-s" ,"--affirmative",    BOOLEAN_TRUE_OPT,  Boolean_Affirmative_Switch),
      Option ("-s+","",                 BOOLEAN_TRUE_OPT,  Boolean_Affirmative_Switch),
      Option ("-s-","--no-affirmative", BOOLEAN_FALSE_OPT, Boolean_Affirmative_Switch),
      Option ("+s", "",                 BOOLEAN_FALSE_OPT, Boolean_Affirmative_Switch)
     );   
   
   procedure Put_Option_Value (Option : Option_Type) is
   begin
      Option_Processor.Put (Option.Value);
   end;
   
begin
   
   declare
      File_Indices : File_Index_Array := Get_Options 
        (
         Options,
         Read_STDIN_If_No_Files =>
           (if Ada.Directories.Simple_Name (Command_Name) = "process_options_no_stdin"
              then False else True),
         Help_Printer => Help'Access
        );
   begin
      
      Set_Flag (Option_Selector.Flag_Value.all);
      
      Put_Line ("This program ('" & Command_Name & "') recognises the following options:");
      for O of Options loop
         Put (O.Short_Option_Prefix'Image);
         Put (" " & O.Short_Option'Image);
         Put (" " & O.Short_Option_Suffix'Image);
         Put (ASCII.HT & O.Option_Kind'Image);
         Set_Col(32);
         Put (ASCII.HT & "Is_Present = " & O.Is_Present'Image);
         Put( ASCII.HT & """" & O.Long_Option.all & """");
         New_Line;
      end loop;
      
      New_Line;
      Put_Line ("The following options were processed on the command line:");
      
      for O of Options loop
         if O.Is_Present then
            Put (O.Short_Option_Prefix'Image);
            Put (" " & O.Short_Option'Image);
            Put (" " & O.Short_Option_Suffix'Image);
            Put (ASCII.HT & O.Option_Kind'Image);
            Set_Col(34);
            Put_Option_Value (O);
            New_Line;
         end if;
      end loop;   
      
      New_Line;
      Put_Line ("The following parameter values are current:");
      
      Put_Line ("Integer_Parameter : " & Integer_Parameter.Integer_Value'Image);
      Put_Line ("Float_Parameter   : " & Float_Parameter.Float_Value'Image);
      Put_Line ("Flag              : " & Flag'Image);
      Put_Line ("Positive toggle   : " & Boolean_Affirmative_Switch.Boolean_Value'Image);
      Put_Line ("Negative toggle   : " & Boolean_Negative_Switch.Boolean_Value'Image);
      
      New_Line;
      Put_Line ("Explicitly requesting option values:");
      
      Put_Line ("--int      : " & Get_Integer_Option_Value ("--int", Options)'Image);      
      Put_Line ("--float    : " & Get_Float_Option_Value ("--float", Options)'Image);
      Put_Line ("double     : " & Get_Double_Option_Value ("--xstrange", Options)'Image);
      Put_Line ("--double   : " & Get_Double_Option_Value ("--double", Options)'Image);
      Put_Line ("--positive : " & Get_Positive_Option_Value ("--positive", Options)'Image);
      Put_Line ("--natural  : " & Get_Natural_Option_Value ("--natural", Options)'Image);
      Put_Line ("--bool     : " & Get_Boolean_Option_Value ("--bool", Options)'Image);
      Put_Line ("--char     : " & Get_Character_Option_Value ("--char", Options)'Image);
      
      if File_Indices'Length > 0 then
         New_Line;
         Put_Line ("The following command line arguments are recognised as files:");
         
         for F in File_Indices'Range loop
            Put (F'Image);
            Put (ASCII.HT & File_Indices (F)'Image);
            if File_Indices (F) /= 0 then
               Put (ASCII.HT & """" & Argument (File_Indices (F)) & """");
            else
               Put (ASCII.HT & """-"" (STDIN)");
            end if;
            New_Line;
         end loop;
         
         New_Line;
         Put_Line ("File contents:");
         
         for F in File_Indices'Range loop
            declare
               Input_File : File_Selector.File_Access := Select_File (File_Indices, F);
               Line_Count : Integer := 0;
               File_Name : String :=
                 (if File_Indices (F) /= 0 then Argument (File_Indices (F)) else "STDIN");
            begin
               Put_Line ("=== """ & File_Name & """ ===");            
               while not End_Of_File (Input_File.all) loop
                  declare
                     Line : String := Get_Line (Input_File.all);
                  begin
                     Line_Count := Line_Count + 1;
                     Put (Line_Count'Image & ASCII.HT);
                     Put_Line (Line);
                  end;
               end loop;
               Close (Input_File);
            end;
        end loop;
   
      end if;

   end;

   exception
      when HELP_PRINTED => null;
         
end;
