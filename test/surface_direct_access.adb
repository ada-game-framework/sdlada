--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;
with Interfaces.C.Pointers;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Log;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Rectangles;
with SDL.Video.Windows.Makers;
with Ada.Numerics.Long_Complex_Types;
use  Ada.Numerics.Long_Complex_Types;

procedure Surface_Direct_Access is
   W : SDL.Video.Windows.Window;
   package Sprite is
      subtype Pixel is Interfaces.Unsigned_16;
      type Image_Type is array (Integer range <>, Integer range <>) of aliased Pixel;

      T : constant := 16#0000#;
      R : constant := 16#F00F#;
      W : constant := 16#FFFF#;
      Image : aliased Image_Type := (
         (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T),
         (T, T, T, R, R, R, T, T, T, R, R, R, T, T, T, T),
         (T, T, R, W, W, R, R, T, R, W, W, R, R, T, T, T),
         (T, R, W, R, R, R, R, R, W, R, R, R, R, R, T, T),
         (R, W, R, R, R, R, R, R, R, R, R, R, R, R, R, T),
         (R, W, R, R, R, R, R, R, R, R, R, R, R, R, R, T),
         (R, R, R, R, R, R, R, R, R, R, R, R, R, R, R, T),
         (T, R, R, R, R, R, R, R, R, R, R, R, R, R, T, T),
         (T, R, R, R, R, R, R, R, R, R, R, R, R, R, T, T),
         (T, T, R, R, R, R, R, R, R, R, R, R, R, T, T, T),
         (T, T, T, R, R, R, R, R, R, R, R, R, T, T, T, T),
         (T, T, T, T, R, R, R, R, R, R, R, T, T, T, T, T),
         (T, T, T, T, T, R, R, R, R, R, T, T, T, T, T, T),
         (T, T, T, T, T, T, R, R, R, T, T, T, T, T, T, T),
         (T, T, T, T, T, T, T, R, T, T, T, T, T, T, T, T),
         (T, T, T, T, T, T, T, R, T, T, T, T, T, T, T, T)
      );
      S : SDL.Video.Surfaces.Surface;
      procedure Create_From is new SDL.Video.Surfaces.Makers.Create_From_Array (
         Element => Pixel,
         Index   => Integer,
         Element_Array => Image_Type);
   end Sprite;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise (Flags => SDL.Enable_Screen) = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Surface direct access: Julia set interactive view (Esc to exit)",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => SDL.Positive_Sizes'(640, 640),
                                       Flags    => SDL.Video.Windows.Resizable);

      Sprite.Create_From (Self       => Sprite.S,
                          Pixels     => Sprite.Image'Access,
                          Red_Mask   => 16#000F#,
                          Green_Mask => 16#00F0#,
                          Blue_Mask  => 16#0F00#,
                          Alpha_Mask => 16#F000#);

      --  Main loop.
      declare
         Pixel_Depth      : constant := 16;
         --  Not all Pixel_Depth values are displayed correctly.
         --  Actual data layout may differ than implied here.
         type Pixel      is mod 2**Pixel_Depth;
         type Pixel_Array is array (Integer range <>) of aliased Pixel;
         package Pixel_Pointers is new Interfaces.C.Pointers (Index              => Integer,
                                                              Element            => Pixel,
                                                              Element_Array      => Pixel_Array,
                                                              Default_Terminator => 0);
         use type Pixel_Pointers.Pointer;

         S                : SDL.Video.Surfaces.Surface;

         package Pixel_Data is new SDL.Video.Surfaces.Pixel_Data (Element         => Pixel,
                                                                  Element_Pointer => Pixel_Pointers.Pointer);
         --  This procedure writes individual pixel in the surface (no blending or masking)
         procedure Write_Pixel (X, Y : Integer; Colour : Pixel) is
            Row_Ptr  : constant Pixel_Pointers.Pointer  := Pixel_Data.Get_Row (S, SDL.Coordinate (Y));
            Ptr      : constant Pixel_Pointers.Pointer  := Row_Ptr + Interfaces.C.ptrdiff_t (X);
         begin
            Ptr.all := Colour;
         end Write_Pixel;

         Cursor : SDL.Natural_Coordinates;

         N_Max : constant := 63; --  Maximum number of iterations for Julia set

         --  Precalculated map of colours
         False_Colour : Pixel_Array (0 .. N_Max);

         procedure Make_False_Colour is
            Z : Complex;
            A : constant Complex := Compose_From_Polar (1.0, 1.0, 3.0);
            R, G, B : Integer;
         begin
            for I in 0 .. N_Max loop
               case I is
                  when 0 .. N_Max - 1 =>
                     Z := Compose_From_Polar
                          (127.0, Long_Float (I), Long_Float (N_Max));
                     B := 127 + Integer (Re (Z));
                     R := 127 + Integer (Re (Z * A));
                     G := 127 + Integer (Re (Z * A * A));
                  when others => -- last iteration means we did'nt reach condition
                     R := 0;
                     G := 0;
                     B := 0;
               end case;
               False_Colour (I) := Pixel (SDL.Video.Pixel_Formats.To_Pixel
                                 (Format => S.Pixel_Format,
                                  Red    => SDL.Video.Palettes.Colour_Component (R),
                                  Green  => SDL.Video.Palettes.Colour_Component (G),
                                  Blue   => SDL.Video.Palettes.Colour_Component (B)));
            end loop;
         end Make_False_Colour;

         procedure Render is
            --  A constant for Julia set iteration
            C : constant Complex := (
                (Long_Float (Cursor.X) / 640.0) * 4.0 - 2.0,
                (Long_Float (Cursor.Y) / 640.0) * 4.0 - 2.0);
            --  Julia set iteration variable
            Z : Complex;
            --  Julia set iteration counter
            N : Integer;
         begin
            S.Lock;
            for X in 0 .. 639 loop
               for Y in 0 .. 639 loop
                  --  Julia set iteration
                  Z := ((Long_Float (X) / 640.0) * 4.0 - 2.0,
                        (Long_Float (Y) / 640.0) * 4.0 - 2.0);
                  N := 0;
                  --  Julia set iteration
                  while Re (Z)**2 + Im (Z)**2 < 4.0 and N < N_Max loop
                     N := N + 1;
                     Z := Z**2 + C;
                  end loop;

                  Write_Pixel (X, Y, False_Colour (N));
               end loop;
            end loop;
            S.Unlock;

            declare
               TR, SR : SDL.Video.Rectangles.Rectangle := (X => 0,
                                                           Y => 0,
                                                           Width  => Sprite.Image'Length (2),
                                                           Height => Sprite.Image'Length (1));
            begin
               TR.X := 20;
               TR.Y := 20;
               S.Blit (TR,
                       Sprite.S,
                       SR);
            end;
         end Render;

         Window_Surface   : SDL.Video.Surfaces.Surface;
         Event            : SDL.Events.Events.Events;
         Finished         : Boolean := False;

         Loop_Start_Time_Goal : Ada.Real_Time.Time;

         Frame_Duration : constant Ada.Real_Time.Time_Span :=
           Ada.Real_Time.Microseconds (16_667);
         --  60 Hz refresh rate (set to anything you like)

         use type SDL.Events.Keyboards.Key_Codes;
      begin
         SDL.Video.Surfaces.Makers.Create (Self => S,
                                           Size => (640, 640),
                                           BPP => Pixel_Depth,
                                           Red_Mask => 0,
                                           Blue_Mask => 0,
                                           Green_Mask => 0,
                                           Alpha_Mask => 0);  --  a surface with known pixel depth

         Make_False_Colour;
         Render;

         Window_Surface := W.Get_Surface;
         Window_Surface.Blit (S);
         W.Update_Surface;

         --  Set next frame delay target using monotonic clock time
         Loop_Start_Time_Goal := Ada.Real_Time.Clock;

         loop
            --  Limit event loop to 60 Hz using realtime "delay until"
            Loop_Start_Time_Goal := Loop_Start_Time_Goal + Frame_Duration;
            delay until Loop_Start_Time_Goal;

            while SDL.Events.Events.Poll (Event) loop
               case Event.Common.Event_Type is
                  when SDL.Events.Quit =>
                     Finished := True;

                  when SDL.Events.Keyboards.Key_Down =>
                     if Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape then
                        Finished := True;
                     end if;

                  when SDL.Events.Mice.Motion =>
                     Cursor := (X => Event.Mouse_Motion.X, Y => Event.Mouse_Motion.Y);
                     Render;
                     Window_Surface := W.Get_Surface;
                     Window_Surface.Blit (S);
                     W.Update_Surface;

                  when others =>
                     null;
               end case;
            end loop;

            exit when Finished;
         end loop;
      end;

      W.Finalize;
      SDL.Finalise;
   end if;
end Surface_Direct_Access;
