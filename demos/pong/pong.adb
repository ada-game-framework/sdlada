--  Pong-Demo for SDLAda, root package for graphics objects.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

package body Pong is

   ---------------------------------------------------------------------
   --  Position
   ---------------------------------------------------------------------
   function Position (This : in Display_Object'Class) return SDL.Coordinates is
   begin
      return SDL.Coordinates'(X => SDL.Dimension (This.New_Pos.X),
                              Y => SDL.Dimension (This.New_Pos.Y));
   end Position;

end Pong;
