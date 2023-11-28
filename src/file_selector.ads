with Text_IO; use Text_IO;
with Option_Processor; use Option_Processor;

with System.Pool_Local;

package File_Selector is
   
   Reclaiming_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   
   type File_Access is access File_Type;
   for File_Access'Storage_size use File_Type'Size;
   
   function Select_File (File_Indices : File_Index_Array; Idx : Positive) 
                        return File_Access;
   
   function Select_File (Idx : Positive) return File_Access;
   
   procedure Close (File : in out File_Access);
   
end File_Selector;
