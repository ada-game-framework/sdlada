--  Pong-Demo for SDLAda, ball actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

package body Pong.Balls is

   use type Interfaces.C.int;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Speed   : in Interfaces.C.int) return Ball is
   begin
      return Ball'(Old_Pos   => SDL.Coordinates'(X => Initial.X,
                                                 Y => Initial.Y),
                   New_Pos   => SDL.Coordinates'(X => Initial.X,
                                                 Y => Initial.Y),
                   Size      => SDL.Sizes'(Width  => Initial.Width,
                                           Height => Initial.Height),
                   Bounds    =>
                     SDL.Video.Rectangles.Rectangle'(X      => Bounds.X,
                                                     Y      => Bounds.Y,
                                                     Width  => Bounds.Width - Initial.Width,
                                                     Height => Bounds.Height - Initial.Height),
                   Direction => SDL.Coordinates'(X => -1,
                                                 Y => -1),
                   Black     => SDL.Video.Palettes.Colour'(Red   => 16#00#,
                                                           Green => 16#00#,
                                                           Blue  => 16#00#,
                                                           Alpha => 16#FF#),
                   White     => SDL.Video.Palettes.Colour'(Red   => 16#FF#,
                                                           Green => 16#FF#,
                                                           Blue  => 16#FF#,
                                                           Alpha => 16#FF#),
                   Speed     => Speed);
   end Create;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Ball;
                   Renderer : in out SDL.Video.Renderers.Renderer)
   is
      Draw_At : constant SDL.Video.Rectangles.Rectangle :=
        SDL.Video.Rectangles.Rectangle'(X      => This.New_Pos.X,
                                        Y      => This.New_Pos.Y,
                                        Width  => This.Size.Width,
                                        Height => This.Size.Height);
   begin
      Renderer.Set_Draw_Colour (Colour => This.White);
      Renderer.Fill (Rectangle => Draw_At);

      This.Old_Pos := This.New_Pos;
   end Draw;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Ball;
                   Clipped :    out Boolean)
   is
      Max_X : constant Interfaces.C.int := This.Bounds.X + This.Bounds.Width;
      Max_Y : constant Interfaces.C.int := This.Bounds.Y + This.Bounds.Height;
   begin
      This.New_Pos.X := This.Old_Pos.X + This.Direction.X * This.Speed;
      This.New_Pos.Y := This.Old_Pos.Y + This.Direction.Y * This.Speed;

      Clipped := False;

      --  Check bounds.
      if
        This.New_Pos.X not in This.Bounds.X .. Max_X
      then
         Clipped := True;

         Change_Dir (This => This,
                     X    => True,
                     Y    => False);
      end if;

      if
        This.New_Pos.Y not in This.Bounds.Y .. Max_Y
      then
         Clipped := True;

         Change_Dir (This => This,
                     X    => False,
                     Y    => True);
      end if;
   end Move;

   ---------------------------------------------------------------------
   --  Warp
   ---------------------------------------------------------------------
   procedure Warp (This    : in out Ball;
                   Initial : in     SDL.Coordinates) is
   begin
      This.New_Pos := Initial;
   end Warp;

   ---------------------------------------------------------------------
   --  Collides
   ---------------------------------------------------------------------
   function Collides (This : in Ball;
                      That : in Display_Object'Class) return Boolean is
   begin
      return not ((This.New_Pos.X               >= That.New_Pos.X + That.Size.Width) or -- I.Left   >= O.Right
                  (This.New_Pos.Y               >= That.New_Pos.Y + That.Size.Height) or -- I.Top    >= O.Bottom
                  (This.New_Pos.X + This.Size.Width <= That.New_Pos.X) or               -- I.Right  <= O.Left
                  (This.New_Pos.Y + This.Size.Height <= That.New_Pos.Y)                  -- I.Bottom <= O.Top
                );
   end Collides;

   ---------------------------------------------------------------------
   --  Change_Dir
   ---------------------------------------------------------------------
   procedure Change_Dir (This : in out Ball;
                         X    : in     Boolean;
                         Y    : in     Boolean)
   is
      Clipped : Boolean;
   begin
      if X then
         This.Direction.X := -This.Direction.X;
      end if;

      if Y then
         This.Direction.Y := -This.Direction.Y;
      end if;

      if X or Y then
         Move (This    => This,
               Clipped => Clipped);
      end if;
   end Change_Dir;

end Pong.Balls;
