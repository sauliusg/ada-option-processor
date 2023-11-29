with Ada.Unchecked_Deallocation;

package body Free_Options is
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Option_Value_Type, Option_Value_Access);
   
   procedure Free is new Ada.Unchecked_Deallocation
     (String, Option_Value_String);
   
   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Access);
   
   procedure Free_Option (Option : in out Option_Type) is
   begin
      Free (Option.Long_Option);
      Free (Option.Help);
   end;
   
   -- -------------------------------------------------------------------------
   
   type Deallocated_Block_Array is array (Positive range <>) of Option_Value_Access;
   
   procedure Add_Dealocated_Block
     (
      Block : Option_Value_Access;
      DB : in out Deallocated_Block_Array;
      Count : in out Natural
     ) is
   begin
      Count := Count + 1;
      DB (Count) := Block;
   end;
   
   function Is_Deallocated
     (
      Block : Option_Value_Access;
      DB : in out Deallocated_Block_Array;
      Count : in out Natural
     ) return Boolean is
   begin
      for I in reverse 1 .. Count loop
         if Block = DB (I) then
            return True;
         end if;
      end loop;
      return False;
   end;
   
   procedure Free_Options (OA : in out Option_Array) is
      Deallocated_Blocks : Deallocated_Block_Array (1 .. OA'Length);
      Deallocated_Block_Count : Natural := 0;
   begin
      for Option of OA loop
         if not Is_Deallocated (Option.Value, Deallocated_Blocks, Deallocated_Block_Count) then
            Add_Dealocated_Block (Option.Value, Deallocated_Blocks, Deallocated_Block_Count);
            if Option.Value /= null then
               if Option.Value.Option_Kind = STRING_OPT then
                  Free (Option.Value.String_Value);
               end if;
            end if;
            Free (Option.Value);
         end if;
         Free_Option (Option);
      end loop;
   end;
   
end Free_Options;
