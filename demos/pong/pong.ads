--  Pong-Demo for SDLAda, root package for graphics objects.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Interfaces.C;
with SDL.Video.Rectangles,
     SDL.Video.Renderers;

package Pong is

   type Display_Object is abstract tagged private;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   procedure Draw (This     : in out Display_Object;
                   Renderer : in out SDL.Video.Renderers.Renderer) is abstract;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   procedure Move (This    : in out Display_Object;
                   Clipped :    out Boolean) is abstract;

   ---------------------------------------------------------------------
   --  X_Position
   ---------------------------------------------------------------------
   function X_Position (This : in Display_Object'Class) return Interfaces.C.int;
   pragma Inline (X_Position);

   ---------------------------------------------------------------------
   --  Y_Position
   ---------------------------------------------------------------------
   function Y_Position (This : in Display_Object'Class) return Interfaces.C.int;
   pragma Inline (Y_Position);

private

   type Display_Object is abstract tagged
      record
         Old_Pos : SDL.Coordinates;
         New_Pos : SDL.Coordinates;
         Size    : SDL.Sizes;
         Bounds  : SDL.Video.Rectangles.Rectangle;
      end record;

end Pong;
