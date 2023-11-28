pragma Ada_2022;

with Text_IO;          use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Option_Processor; use Option_Processor;
with File_Selector;    use File_Selector;

with Ada.Directories;

procedure Process_Options is
   
   HELP_PRINTED : exception;
   
   -- To check whether the library or the main program have memory
   -- leaks, compile and run with the following GNAT options:
   
   -- gnatmake -Isrc -D .obj/ \
   --     -o bin/process_options \
   --     programs/process_options.adb \
   --     -cargs -fsanitize=address -largs -fsanitize=address
   
   Integer_Parameter : Option_Value_Access := new Option_Value_Type (INTEGER_OPT);
   Float_Parameter   : Option_Value_Access := new Option_Value_Type (FLOAT_OPT);
   
   procedure Help (Option_String : String; Pos : in out Positive) is
   begin
      Put_Line ("This is a help from the main program ...");
      raise HELP_PRINTED;
   end;
   
   type Flag_Type is (ONE, TWO, THREE);
   
   Flag : Flag_Type := ONE;
   
   WRONG_FLAG_OPTION : exception;
   
   procedure Set_Flag (Option_String : String; Pos : in out Positive) is
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
      Option ("-h", "--help",     Help'Access),
      Option ("-1", "--one",      Set_Flag'Access),
      Option ("-2", "--two",      Set_Flag'Access),
      Option ("-3", "--tree",     Set_Flag'Access),
      Option ("-x", "--xstrange", DOUBLE_OPT),
      Option ("-i", "--int",      Integer_Parameter),
      Option ("-f", "--float",    Float_Parameter),
      Option ("-b", "--bool",     BOOLEAN_TRUE_OPT),
      Option ("-b+","--bool",     BOOLEAN_TRUE_OPT),
      Option ("-b-","--no-bool",  BOOLEAN_FALSE_OPT)
     );   
   
   procedure Put_Option_Value (Option : Option_Type) is
   begin
      case Option.Option_Kind is
         when STRING_OPT =>
            Put (Option.Value.String_Value.all);
         when INTEGER_OPT =>
            Put (Option.Value.Integer_Value'Image);
         when FLOAT_OPT =>
            Put (Option.Value.Float_Value'Image);
         when DOUBLE_OPT =>
            Put (Option.Value.Double_Value'Image);
         when NATURAL_OPT =>
            Put (Option.Value.Natural_Value'Image);
         when POSITIVE_OPT =>
            Put (Option.Value.Positive_Value'Image);
         when BOOLEAN_TRUE_OPT | BOOLEAN_FALSE_OPT =>
            Put (Option.Value.Boolean_Value'Image);
         when FUNCTION_OPT =>
            Put (Option.Value.Process'Image & " called");
         when others =>
            raise UNKNOWN_OPTION with
              "INTERNAL ERROR -- unknown option kind '" &
              Option.Option_Kind'Image & "' for option " &
              Option.Long_Option.all & "' in Put_Option_Value";
      end case;
   end;
   
begin
   
   declare
      File_Indices : File_Index_Array := Get_Options 
        (
         Options,
         Read_STDIN_If_No_Files =>
           (if Ada.Directories.Simple_Name (Command_Name) = "process_options_no_stdin"
              then False else True)
        );
   begin
      
      -- if Ada.Directories.Simple_Name (Command_Name) = "process_options_no_stdin" then
      --    Put_Line ("Configuring '" & Command_Name & "' to not process STDIN if not files are given...");
      --    Option_Processor.Configure
      --      (
      --       Read_STDIN_If_No_Files => False
      --      );
      -- end if;
      --
      -- Process_Options (Options);
      
      Put_Line ("This program ('" & Command_Name & "') recognises the following options:");
      for O of Options loop
         Put (O.Short_Option'Image);
         Put (" " & O.Short_Option_Suffix'Image);
         Put (ASCII.HT & O.Option_Kind'Image);
         Set_Col(28);
         Put (ASCII.HT & "Is_Present = " & O.Is_Present'Image);
         Put( ASCII.HT & """" & O.Long_Option.all & """");
         New_Line;
      end loop;
      
      New_Line;
      Put_Line ("The following options were processed on the command line:");
      
      for O of Options loop
         if O.Is_Present then
            Put (O.Short_Option'Image);
            Put (" " & O.Short_Option_Suffix'Image);
            Put (ASCII.HT & O.Option_Kind'Image);
            Set_Col(30);
            Put_Option_Value (O);
            New_Line;
         end if;
      end loop;   
      
      New_Line;
      Put_Line ("The following parameter values are current:");
      
      Put_Line ("Integer_Parameter : " & Integer_Parameter.Integer_Value'Image);
      Put_Line ("Float_Parameter   : " & Float_Parameter.Float_Value'Image);
      Put_Line ("Flag              : " & Flag'Image);
      
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
   
   
