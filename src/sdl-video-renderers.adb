package body SDL.Video.Renderers is
   function Get_Address (Self : in Renderer) return System.Address is
   begin
      return Self.Internal;
   end Get_Address;
end SDL.Video.Renderers;
