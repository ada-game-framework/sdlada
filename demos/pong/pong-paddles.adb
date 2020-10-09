--  Pong-Demo for SDLAda, paddle actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

package body Pong.Paddles is

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Speed   : in Interfaces.C.int) return Paddle is
   begin
      return Paddle'(Old_Pos  => SDL.Coordinates'(X => Initial.X,
                                                  Y => Initial.Y),
                     New_Pos  => SDL.Coordinates'(X => Initial.X,
                                                  Y => Initial.Y),
                     Size     => SDL.Sizes'(Width  => Initial.Width,
                                            Height => Initial.Height),
                     Bounds   =>
                       SDL.Video.Rectangles.Rectangle'(X      => Bounds.X,
                                                       Y      => Bounds.Y,
                                                       Width  => Bounds.Width - Initial.Width,
                                                       Height => Bounds.Height - Initial.Height),
                     Black    => SDL.Video.Palettes.Colour'(Red   => 16#00#,
                                                            Green => 16#00#,
                                                            Blue  => 16#00#,
                                                            Alpha => 16#FF#),
                     White    => SDL.Video.Palettes.Colour'(Red   => 16#FF#,
                                                            Green => 16#FF#,
                                                            Blue  => 16#FF#,
                                                            Alpha => 16#FF#),
                     Max_Speed => Speed,
                     Velocity  => 0);
   end Create;

   ---------------------------------------------------------------------
   --  Set_Velocity
   ---------------------------------------------------------------------
   not overriding
   procedure Set_Velocity (This : in out Paddle;
                           Vel  : in     Velocity) is
   begin
      This.Velocity := Vel;
   end Set_Velocity;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Paddle;
                   Clipped :    out Boolean)
   is
      Max_Y : constant Interfaces.C.int := This.Bounds.Y + This.Bounds.Height;
   begin
      This.New_Pos.Y := This.Old_Pos.Y + This.Velocity * This.Max_Speed;

      if
        This.New_Pos.Y > Max_Y
      then
         Clipped := True;
         This.New_Pos.Y := Max_Y;
      elsif
        This.New_Pos.Y < This.Bounds.Y
      then
         Clipped := True;
         This.New_Pos.Y := This.Bounds.Y;
      else
         Clipped := False;
      end if;
   end Move;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Paddle;
                   Renderer : in out SDL.Video.Renderers.Renderer)
   is
      Draw_At  : constant SDL.Video.Rectangles.Rectangle :=
        SDL.Video.Rectangles.Rectangle'(X      => This.New_Pos.X,
                                        Y      => This.New_Pos.Y,
                                        Width  => This.Size.Width,
                                        Height => This.Size.Height);
   begin
      Renderer.Set_Draw_Colour (Colour => This.White);
      Renderer.Fill (Rectangle => Draw_At);

      This.Old_Pos := This.New_Pos;
   end Draw;

end Pong.Paddles;
