--  Pong-Demo for SDLAda, ball actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with SDL.Video.Palettes;

package Pong.Balls is

   type Ball is new Display_Object with private;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Speed   : in Interfaces.C.int) return Ball;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Ball;
                   Renderer : in out SDL.Video.Renderers.Renderer);

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Ball;
                   Clipped :    out Boolean);

   ---------------------------------------------------------------------
   --  Warp
   ---------------------------------------------------------------------
   procedure Warp (This    : in out Ball;
                   Initial : in     SDL.Coordinates);

   ---------------------------------------------------------------------
   --  Collides
   ---------------------------------------------------------------------
   function Collides (This : in Ball;
                      That : in Display_Object'Class) return Boolean;

   ---------------------------------------------------------------------
   --  Change_Dir
   ---------------------------------------------------------------------
   procedure Change_Dir (This : in out Ball;
                         X    : in     Boolean;
                         Y    : in     Boolean);

private

   type Ball is new Display_Object with
      record
         Speed     : Interfaces.C.int;
         Direction : SDL.Coordinates; --  Actual moving vector.
         Black     : SDL.Video.Palettes.Colour;
         White     : SDL.Video.Palettes.Colour;
      end record;

end Pong.Balls;
