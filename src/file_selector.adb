with Ada.Command_Line; use Ada.Command_Line;

with Ada.Unchecked_Deallocation;

package body File_Selector is
   
   procedure Free is new Ada.Unchecked_Deallocation
     (File_Type, File_Access);
   
   procedure Close (File : in out File_Access) is
   begin
      if File /= null then
         if Is_Open (File.all) then
            Close (File.all);
         end if;
         Free (File);
      end if;
   end;
   
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
           "you should use Close () " &
           "after processing each file returned by Select_File()";
   end;
   
   function Select_File (Idx : Positive) return File_Access is
      File_Name : String := Get_File_Name (Idx);
      Return_File : File_Access;
   begin
      if File_Name = "" then
         return null;
      elsif File_Name = "-" then
         Return_File := new File_Type'(Standard_Input);
      else
         Return_File := new File_Type;
         Open (Return_File.all, In_File, File_Name);
      end if;
      return Return_File;
   exception
      when STORAGE_ERROR =>
         raise STORAGE_ERROR with
           "package 'File_Selector': storage for selected files exhausted -- " &
           "you should use Close () " &
           "after processing each file returned by Select_File()";
   end;
   
   procedure Select_File
     (
      File_Indices : File_Index_Array;
      Idx : Positive;
      Selected_File : in out File_Access
     ) is
   begin
      if Selected_File /= null then
         Close (Selected_File);
      end if;
      Selected_File := Select_File (File_Indices, Idx);
   end;
   
   procedure Select_File
     (
      Idx : Positive;
      Selected_File : in out File_Access
     ) is
   begin
      if Selected_File /= null then
         Close (Selected_File);
      end if;
      Selected_File := Select_File (Idx);      
   end;
   
end File_Selector;
