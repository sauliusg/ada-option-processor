with Text_IO; use Text_IO;
with Option_Processor; use Option_Processor;

with System.Pool_Local;
with Ada.Unchecked_Deallocation;

package File_Selector is
   
   Reclaiming_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;
   
   type File_Access is access File_Type;
   for File_Access'Storage_size use File_Type'Size;
   
   function Select_File (File_Indices : File_Index_Array; Idx : Positive) 
                        return File_Access;
   
   procedure Free is new Ada.Unchecked_Deallocation
     (File_Type, File_Access);
   
end File_Selector;
