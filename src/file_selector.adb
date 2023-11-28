with Ada.Command_Line; use Ada.Command_Line;

package body File_Selector is
   
   function Select_File (File_Indices : File_Index_Array; Idx : Positive) 
                        return File_Access
   is
      Return_File : File_Access;
   begin
      if File_Indices (Idx) = 0 then
         Return_File := new File_Type'(Standard_Input);
      else
         Return_File := new File_Type;
         Open (Return_File.all, In_File, Argument (File_Indices (Idx)));
      end if;
      return Return_File;
   exception
      when STORAGE_ERROR =>
         raise STORAGE_ERROR with
           "package 'File_Selector': storage for selected files exhausted -- " &
           "you should use Free() " &
           "after processing each file returned by Select_File()";
   end;
   
end File_Selector;
