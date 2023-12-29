--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Moose;
with SDL;
with SDL.Log;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with System;

procedure Stream is
   use type SDL.Dimension;
   use type SDL.Positive_Sizes;

   W        : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Texture  : SDL.Video.Textures.Texture;
   Pixels   : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

   procedure Lock is new SDL.Video.Textures.Lock (Pixel_Pointer_Type => SDL.Video.Pixels.ARGB_8888_Access.Pointer);

   use type SDL.Video.Pixels.ARGB_8888_Access.Pointer;

   --  This uses the same algorithm as the original teststream.c. It copies 1 pixel at a time, indexing into the moose
   --  palette using the data from moose.dat.
   procedure Update_Texture_1 (Pointer : in SDL.Video.Pixels.ARGB_8888_Access.Pointer) is
      Start_Time       : Ada.Real_Time.Time;
      End_Time         : Ada.Real_Time.Time;
      Colour           : SDL.Video.Palettes.RGB_Colour;

      use type Ada.Real_Time.Time;
   begin
      Start_Time := Ada.Real_Time.Clock;

      for Y in 1 .. Moose.Moose_Size.Height loop
         declare
            Dest : SDL.Video.Pixels.ARGB_8888_Access.Pointer :=
              Pointer + Interfaces.C.ptrdiff_t ((Y - 1) * Moose.Moose_Size.Width);
         begin
            for X in 1 .. Moose.Moose_Size.Width loop
               Colour := Moose.Moose_Palette (Moose.Moose_Frame_Data
                 (Moose.Moose_Frame, ((Y - 1) * Moose.Moose_Size.Width) + (X - 1)));

               Dest.all := SDL.Video.Pixels.ARGB_8888'(Red   => Colour.Red,
                                                       Green => Colour.Green,
                                                       Blue  => Colour.Blue,
                                                       Alpha => SDL.Video.Palettes.Colour_Component'Last);

               SDL.Video.Pixels.ARGB_8888_Access.Increment (Dest);
            end loop;
         end;
      end loop;

      End_Time := Ada.Real_Time.Clock;
      SDL.Log.Put_Debug ("Update_Texture_1 took " &
        Ada.Real_Time.To_Duration (End_Time - Start_Time)'Img & " seconds.");
   end Update_Texture_1;

   package Texture_2D is new SDL.Video.Pixels.Texture_Data
     (Index              => SDL.Dimension,
      Element            => SDL.Video.Pixels.ARGB_8888,
      Element_Array_1D   => SDL.Video.Pixels.ARGB_8888_Array,
      Element_Array_2D   => Moose.Texture_2D_Array,
      Default_Terminator => SDL.Video.Pixels.ARGB_8888'(others => SDL.Video.Palettes.Colour_Component'First));

   procedure Update_Texture_2 (Pointer : in Texture_2D.Pointer) is
      pragma Unreferenced (Pointer);  --  TODO: Fix me!

      function To_Address is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
                                                           Target => System.Address);

      Start_Time       : Ada.Real_Time.Time;
      End_Time         : Ada.Real_Time.Time;
      Colour           : SDL.Video.Palettes.RGB_Colour;
      Actual_Pixels    : Moose.Texture_2D_Array (1 .. Moose.Moose_Size.Height, 1 .. Moose.Moose_Size.Width) with
        Address => To_Address (Pixels);

      use type Ada.Real_Time.Time;
   begin
      Start_Time := Ada.Real_Time.Clock;

      for Y in 1 .. Moose.Moose_Size.Height loop
         for X in 1 .. Moose.Moose_Size.Width loop
            Colour := Moose.Moose_Palette
              (Moose.Moose_Frame_Data (Moose.Moose_Frame, ((Y - 1) * Moose.Moose_Size.Width) + (X - 1)));

            Actual_Pixels (Y, X) := SDL.Video.Pixels.ARGB_8888'(Red   => Colour.Red,
                                                                Green => Colour.Green,
                                                                Blue  => Colour.Blue,
                                                                Alpha => SDL.Video.Palettes.Colour_Component'Last);
         end loop;
      end loop;

      End_Time := Ada.Real_Time.Clock;
      SDL.Log.Put_Debug ("Update_Texture_2 took " &
        Ada.Real_Time.To_Duration (End_Time - Start_Time)'Img & " seconds.");
   end Update_Texture_2;

   use type Moose.Moose_Frames;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   Moose.Load_Moose_Data (Data => Moose.Moose_Frame_Data);

   Moose.Cache_Moose (Moose.Cache, Moose.Moose_Frame_Data, Moose.Moose_Palette);

   if SDL.Initialise = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Stream (Moose animation)",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => Moose.Moose_Size * 4,
                                       Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                        Renderer => Renderer,
                                        Format   => SDL.Video.Pixel_Formats.Pixel_Format_ARGB_8888,
                                        Kind     => SDL.Video.Textures.Streaming,
                                        Size     => Moose.Moose_Size);

      --  W.Set_Mode (SDL.Video.Windows.Full_Screen);

      --  First test.
      for Index in 1 .. 10 loop
         Lock (Texture, Pixels);

         --  The block makes things a bit clearer.
         begin
            Update_Texture_1 (Pixels);
         end;

         Texture.Unlock;

         Renderer.Clear;
         Renderer.Copy (Texture);
         Renderer.Present;

         Moose.Moose_Frame := Moose.Moose_Frame + 1;

         --           if Moose_Frame = Moose_Frame'Last then
         --              Pixel := Data.Element_Array (0)'Access;
         --           end if;

         delay 0.05;
      end loop;

      Renderer.Clear;
      Renderer.Present;

      delay 0.7;

      SDL.Log.Put_Debug ("");

      --  Second test.
      for Index in 1 .. 10 loop
         Lock (Texture, Pixels);

         --  The block makes things a bit clearer.
         begin
            Update_Texture_2 (Texture_2D.Pointer (Pixels));
         end;

         Texture.Unlock;

         Renderer.Clear;
         Renderer.Copy (Texture);
         Renderer.Present;

         Moose.Moose_Frame := Moose.Moose_Frame + 1;

         --           if Moose_Frame = Moose_Frame'Last then
         --              Pixel := Data.Element_Array (0)'Access;
         --           end if;

         delay 0.05;
      end loop;

      Renderer.Clear;
      Renderer.Present;

      delay 0.7;

      SDL.Log.Put_Debug ("");

      --  Third test.
      for Index in 1 .. 100 loop
         Lock (Texture, Pixels);

         --  The block makes things a bit clearer.
         Update_Texture_3 : declare
            function To_Address is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
                                                                 Target => System.Address);

            Start_Time    : Ada.Real_Time.Time;
            End_Time      : Ada.Real_Time.Time;
            Actual_Pixels : Moose.Texture_2D_Array (1 .. Moose.Moose_Size.Height, 1 .. Moose.Moose_Size.Width) with
              Address => To_Address (Pixels);

            use type Ada.Real_Time.Time;
         begin
            Start_Time := Ada.Real_Time.Clock;

            Actual_Pixels := Moose.Cache (Moose.Moose_Frame);

            End_Time := Ada.Real_Time.Clock;
            SDL.Log.Put_Debug ("Update_Texture_3 took " &
              Ada.Real_Time.To_Duration (End_Time - Start_Time)'Img & " seconds.");
         end Update_Texture_3;

         Texture.Unlock;

         Renderer.Clear;
         Renderer.Copy (Texture);
         Renderer.Present;

         Moose.Moose_Frame := Moose.Moose_Frame + 1;

         --           if Moose_Frame = Moose_Frame'Last then
         --              Pixel := Data.Element_Array (0)'Access;
         --           end if;

         delay 0.05;
      end loop;

      W.Finalize;
      SDL.Finalise;
   end if;
end Stream;
