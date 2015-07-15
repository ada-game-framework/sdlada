package body SDL.Video.Surfaces.Makers is
   function Make (S : in SDL.C_Pointers.Surface_Pointer; Owns : in Boolean) return Surface is
   begin
      return R : Surface := (Ada.Finalization.Limited_Controlled with Internal => S, Owns => Owns) do
         null;
      end return;
   end Make;
end SDL.Video.Surfaces.Makers;
