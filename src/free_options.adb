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
      if Option.Value /= null then
         if Option.Value.Option_Kind = STRING_OPT then
            Free (Option.Value.String_Value);
         end if;
      end if;
      Free (Option.Value);
      Free (Option.Long_Option);
   end;
   
   -- -------------------------------------------------------------------------
   
   procedure Free_Options (OA : in out Option_Array) is
   begin
      for Option of OA loop
         Free_Option (Option);
      end loop;
   end;
   
end Free_Options;
