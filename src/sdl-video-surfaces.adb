package body SDL.Video.Surfaces is
   --  Private subprograms for accessing the internal value.
   function Get_Address (Self : in Surface) return System.Address is
   begin
      return Self.Internal;
   end Get_Address;

   procedure Set_Address (Self : in out Surface; A : in System.Address) is
   begin
      Self.Internal := A;
   end Set_Address;
end SDL.Video.Surfaces;
