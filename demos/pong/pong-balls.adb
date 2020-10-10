--  Pong-Demo for SDLAda, ball actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Ada.Numerics.Float_Random;

package body Pong.Balls is

   use type SDL.Dimension;

   Random : Ada.Numerics.Float_Random.Generator;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Colour  : in SDL.Video.Palettes.Colour;
                    Speed   : in Float) return Ball
   is
   begin
      return Result : Ball do
         Result :=
           Ball'(Old_Pos   => Smooth_Coordinates'(X => Float (Initial.X),
                                                  Y => Float (Initial.Y)),
                 New_Pos   => Smooth_Coordinates'(X => Float (Initial.X),
                                                  Y => Float (Initial.Y)),
                 Size      => SDL.Sizes'(Width  => Initial.Width,
                                         Height => Initial.Height),
                 Bounds    =>
                   Smooth_Bounds'(Min => (X      => Float (Bounds.X),
                                          Y      => Float (Bounds.Y)),
                                  Max => (X => Float (Bounds.X + Bounds.Width - Initial.Width),
                                          Y => Float (Bounds.Y + Bounds.Height - Initial.Height))),
                 Direction => Smooth_Coordinates'(X => -1.0,
                                                  Y => -1.0),
                 Colour    => Colour,
                 Speed     => Speed);
         Result.Warp (To_Position => SDL.Coordinates'(X => Initial.X,
                                                      Y => Initial.Y));
      end return;
   end Create;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Ball;
                   Renderer : in out SDL.Video.Renderers.Renderer)
   is
      Draw_At : constant SDL.Video.Rectangles.Rectangle :=
        SDL.Video.Rectangles.Rectangle'(X      => SDL.Dimension (This.New_Pos.X),
                                        Y      => SDL.Dimension (This.New_Pos.Y),
                                        Width  => This.Size.Width,
                                        Height => This.Size.Height);
   begin
      Renderer.Set_Draw_Colour (Colour => This.Colour);
      Renderer.Fill (Rectangle => Draw_At);

      This.Old_Pos := This.New_Pos;
   end Draw;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Ball;
                   Clipped :    out Boolean) is
   begin
      This.New_Pos.X := This.Old_Pos.X + This.Direction.X * This.Speed;
      This.New_Pos.Y := This.Old_Pos.Y + This.Direction.Y * This.Speed;

      Clipped := False;

      --  Check bounds.
      if This.New_Pos.X not in This.Bounds.Min.X .. This.Bounds.Max.X then
         Clipped := True;

         if This.New_Pos.X < This.Bounds.Min.X then
            This.New_Pos.X := This.Bounds.Min.X;
         else
            This.New_Pos.X := This.Bounds.Max.X;
         end if;

         Change_Dir (This => This,
                     X    => True,
                     Y    => False);
      end if;

      if This.New_Pos.Y not in This.Bounds.Min.Y .. This.Bounds.Max.Y then
         Clipped := True;

         if This.New_Pos.Y < This.Bounds.Min.Y then
            This.New_Pos.Y := This.Bounds.Min.Y;
         else
            This.New_Pos.Y := This.Bounds.Max.Y;
         end if;

         Change_Dir (This => This,
                     X    => False,
                     Y    => True);
      end if;
   end Move;

   ---------------------------------------------------------------------
   --  Warp
   ---------------------------------------------------------------------
   procedure Warp (This        : in out Ball;
                   To_Position : in     SDL.Coordinates) is
   begin
      This.New_Pos := Smooth_Coordinates'(X => Float (To_Position.X),
                                          Y => Float (To_Position.Y));

      --  Slightly randomize the velocity vector by changing the direction twice
      --  (which in effect means no direction change at all).  This
      --  randomization avoids getting stuck in a repeat loop if the ball has a
      --  velocity vector that doesn't let the computer catch it in the corners
      --  after it's been placed on the playing field.
      This.Change_Dir (X => True,
                       Y => True);
      This.Change_Dir (X => True,
                       Y => True);
   end Warp;

   ---------------------------------------------------------------------
   --  Collides
   ---------------------------------------------------------------------
   function Collides (This : in Ball;
                      That : in Display_Object'Class) return Boolean is
   begin
      return
        SDL.Video.Rectangles.Has_Intersected
          (A => SDL.Video.Rectangles.Rectangle'(X      => This.Position.X,
                                                Y      => This.Position.Y,
                                                Width  => This.Size.Width,
                                                Height => This.Size.Height),
           B => SDL.Video.Rectangles.Rectangle'(X      => That.Position.X,
                                                Y      => That.Position.Y,
                                                Width  => That.Size.Width,
                                                Height => That.Size.Height));
   end Collides;

   ---------------------------------------------------------------------
   --  Change_Dir
   ---------------------------------------------------------------------
   procedure Change_Dir (This : in out Ball;
                         X    : in     Boolean;
                         Y    : in     Boolean) is
   begin
      if X then
         This.Direction.X := -This.Direction.X;
      end if;

      if Y then
         This.Direction.Y := -This.Direction.Y;
      end if;

      --  After a direction change, add a minor fluctuation in direction vector.
      if X or Y then
         declare
            Fluctuation : constant Float :=
              (Ada.Numerics.Float_Random.Random (Gen => Random) - 0.5) / 10.0;
            --  Get a random number between -0.05 and 0.05
         begin
            This.Direction.X := (1.0 + Fluctuation) * This.Direction.X;
            This.Direction.Y := (1.0 - Fluctuation) * This.Direction.Y;
         end;
      end if;
   end Change_Dir;

begin
   Ada.Numerics.Float_Random.Reset (Gen => Random);
end Pong.Balls;
