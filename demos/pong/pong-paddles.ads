--  Pong-Demo for SDLAda, paddle actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with SDL.Video.Palettes;

package Pong.Paddles is

   use type Interfaces.C.int;

   type Paddle is new Pong.Display_Object with private;

   subtype Velocity is Interfaces.C.int range -1 .. +1;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Speed   : in Interfaces.C.int) return Paddle;

   ---------------------------------------------------------------------
   --  Set_Velocity
   ---------------------------------------------------------------------
   not overriding
   procedure Set_Velocity (This : in out Paddle;
                           Vel  : in     Velocity);

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Paddle;
                   Clipped :    out Boolean);

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Paddle;
                   Renderer : in out SDL.Video.Renderers.Renderer);

private

   type Paddle is new Pong.Display_Object with
      record
         Max_Speed : Interfaces.C.int;
         Velocity  : Pong.Paddles.Velocity;
         Black     : SDL.Video.Palettes.Colour;
         White     : SDL.Video.Palettes.Colour;
      end record;

end Pong.Paddles;
