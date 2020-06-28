with Ada.Calendar;
with Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Conversion;
with Interfaces.C;
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

   type Moose_Frames is mod 10;
   type Moose_Colour_Index is range 1 .. 84;
   type Moose_Palette_Array is array (Moose_Colour_Index'Range) of SDL.Video.Palettes.RGB_Colour;

   W                : SDL.Video.Windows.Window;
   Moose_Size       : constant SDL.Positive_Sizes  := (64, 88);
   Moose_Frame_Size : constant SDL.Dimension       := (Moose_Size.Width * Moose_Size.Height) - 1;
   Moose_Frame      : Moose_Frames                 := Moose_Frames'First;
   Moose_Palette    : constant Moose_Palette_Array :=
     ((49, 49, 49),    (66, 24, 0),     (66, 33, 0),     (66, 66, 66),
      (66, 115, 49),   (74, 33, 0),     (74, 41, 16),    (82, 33, 8),
      (82, 41, 8),     (82, 49, 16),    (82, 82, 82),    (90, 41, 8),
      (90, 41, 16),    (90, 57, 24),    (99, 49, 16),    (99, 66, 24),
      (99, 66, 33),    (99, 74, 33),    (107, 57, 24),   (107, 82, 41),
      (115, 57, 33),   (115, 66, 33),   (115, 66, 41),   (115, 74, 0),
      (115, 90, 49),   (115, 115, 115), (123, 82, 0),    (123, 99, 57),
      (132, 66, 41),   (132, 74, 41),   (132, 90, 8),    (132, 99, 33),
      (132, 99, 66),   (132, 107, 66),  (140, 74, 49),   (140, 99, 16),
      (140, 107, 74),  (140, 115, 74),  (148, 107, 24),  (148, 115, 82),
      (148, 123, 74),  (148, 123, 90),  (156, 115, 33),  (156, 115, 90),
      (156, 123, 82),  (156, 132, 82),  (156, 132, 99),  (156, 156, 156),
      (165, 123, 49),  (165, 123, 90),  (165, 132, 82),  (165, 132, 90),
      (165, 132, 99),  (165, 140, 90),  (173, 132, 57),  (173, 132, 99),
      (173, 140, 107), (173, 140, 115), (173, 148, 99),  (173, 173, 173),
      (181, 140, 74),  (181, 148, 115), (181, 148, 123), (181, 156, 107),
      (189, 148, 123), (189, 156, 82),  (189, 156, 123), (189, 156, 132),
      (189, 189, 189), (198, 156, 123), (198, 165, 132), (206, 165, 99),
      (206, 165, 132), (206, 173, 140), (206, 206, 206), (214, 173, 115),
      (214, 173, 140), (222, 181, 148), (222, 189, 132), (222, 189, 156),
      (222, 222, 222), (231, 198, 165), (231, 231, 231), (239, 206, 173));

   type Moose_Frame_Data_Array is array (Moose_Frames'Range, 0 .. Moose_Frame_Size) of Moose_Colour_Index;

   Moose_Frame_Data : Moose_Frame_Data_Array;

   procedure Load_Moose_Data (Data : out Moose_Frame_Data_Array) is
      Actual_Name : constant String := "../../test/moose.dat";
      Data_File   : Ada.Text_IO.File_Type;
      Stream      : Ada.Text_IO.Text_Streams.Stream_Access := null;

      use type Ada.Text_IO.File_Mode;
   begin
      Ada.Text_IO.Open (File => Data_File, Mode => Ada.Text_IO.In_File, Name => Actual_Name);

      Stream := Ada.Text_IO.Text_Streams.Stream (File => Data_File);

      Moose_Frame_Data_Array'Read (Stream, Data);

      Ada.Text_IO.Close (File => Data_File);
   exception
      when others =>
         SDL.Log.Put_Error ("Error, reading source file, " & Actual_Name);

         raise;
   end Load_Moose_Data;

   Renderer         : SDL.Video.Renderers.Renderer;
   Texture          : SDL.Video.Textures.Texture;
   Pixels           : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

   procedure Lock is new SDL.Video.Textures.Lock (Pixel_Pointer_Type => SDL.Video.Pixels.ARGB_8888_Access.Pointer);

   use type SDL.Video.Pixels.ARGB_8888_Access.Pointer;
   use type Ada.Calendar.Time;

   --  This uses the same algorithm as the original teststream.c. It copies 1 pixel at a time, indexing into the moose
   --  palette using the data from moose.dat.
   procedure Update_Texture_1 (Pointer : in SDL.Video.Pixels.ARGB_8888_Access.Pointer) is
      Start_Time       : Ada.Calendar.Time;
      End_Time         : Ada.Calendar.Time;
      Colour           : SDL.Video.Palettes.RGB_Colour;
   begin
      Start_Time := Ada.Calendar.Clock;

      for Y in 1 .. Moose_Size.Height loop
         declare
            Dest : SDL.Video.Pixels.ARGB_8888_Access.Pointer :=
              Pointer + Interfaces.C.ptrdiff_t ((Y - 1) * Moose_Size.Width);
         begin
            for X in 1 .. Moose_Size.Width loop
               Colour := Moose_Palette (Moose_Frame_Data (Moose_Frame, ((Y - 1) * Moose_Size.Width) + (X - 1)));

               Dest.all := SDL.Video.Pixels.ARGB_8888'(Red   => Colour.Red,
                                                       Green => Colour.Green,
                                                       Blue  => Colour.Blue,
                                                       Alpha => SDL.Video.Palettes.Colour_Component'Last);

               SDL.Video.Pixels.ARGB_8888_Access.Increment (Dest);
            end loop;
         end;
      end loop;

      End_Time := Ada.Calendar.Clock;
      SDL.Log.Put_Debug ("Update_Texture_1 took " & Duration'Image (End_Time - Start_Time) & " seconds.");
   end Update_Texture_1;

   type Texture_2D_Array is array (SDL.Dimension range <>, SDL.Dimension range <>) of
     aliased SDL.Video.Pixels.ARGB_8888;

   package Texture_2D is new SDL.Video.Pixels.Texture_Data
     (Index              => SDL.Dimension,
      Element            => SDL.Video.Pixels.ARGB_8888,
      Element_Array_1D   => SDL.Video.Pixels.ARGB_8888_Array,
      Element_Array_2D   => Texture_2D_Array,
      Default_Terminator => SDL.Video.Pixels.ARGB_8888'(others => SDL.Video.Palettes.Colour_Component'First));

   procedure Update_Texture_2 (Pointer : in Texture_2D.Pointer) is
      pragma Unreferenced (Pointer);  --  TODO: Fix me!

      function To_Address is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
                                                           Target => System.Address);

      Start_Time       : Ada.Calendar.Time;
      End_Time         : Ada.Calendar.Time;
      Colour           : SDL.Video.Palettes.RGB_Colour;
      Actual_Pixels    : Texture_2D_Array (1 .. Moose_Size.Height, 1 .. Moose_Size.Width) with
        Address => To_Address (Pixels);
   begin
      Start_Time := Ada.Calendar.Clock;

      for Y in 1 .. Moose_Size.Height loop
         for X in 1 .. Moose_Size.Width loop
            Colour := Moose_Palette (Moose_Frame_Data (Moose_Frame, ((Y - 1) * Moose_Size.Width) + (X - 1)));

            Actual_Pixels (Y, X) := SDL.Video.Pixels.ARGB_8888'(Red   => Colour.Red,
                                                                Green => Colour.Green,
                                                                Blue  => Colour.Blue,
                                                                Alpha => SDL.Video.Palettes.Colour_Component'Last);
         end loop;
      end loop;

      End_Time := Ada.Calendar.Clock;
      SDL.Log.Put_Debug ("Update_Texture_2 took " & Duration'Image (End_Time - Start_Time) & " seconds.");
   end Update_Texture_2;

   type Cached_Moose_Frame_Array is array (Moose_Frames) of
     Texture_2D_Array (1 .. Moose_Size.Height, 1 .. Moose_Size.Width);

   procedure Cache_Moose (Cache   : in out Cached_Moose_Frame_Array;
                          Indices : in Moose_Frame_Data_Array;
                          Palette : in Moose_Palette_Array) is

      Colour : SDL.Video.Palettes.RGB_Colour;
   begin
      for Frame in Moose_Frames loop
         for Y in 1 .. Moose_Size.Height loop
            for X in 1 .. Moose_Size.Width loop
               Colour := Palette (Indices (Frame, ((Y - 1) * Moose_Size.Width) + (X - 1)));

               Cache (Frame) (Y, X) := SDL.Video.Pixels.ARGB_8888'(Red   => Colour.Red,
                                                                   Green => Colour.Green,
                                                                   Blue  => Colour.Blue,
                                                                   Alpha => SDL.Video.Palettes.Colour_Component'Last);
            end loop;
         end loop;
      end loop;
   end Cache_Moose;

   Cache : Cached_Moose_Frame_Array;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   Load_Moose_Data (Data => Moose_Frame_Data);

   Cache_Moose (Cache, Moose_Frame_Data, Moose_Palette);

   if SDL.Initialise = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Stream (Moose animation)",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => Moose_Size * 4,
                                       Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                        Renderer => Renderer,
                                        Format   => SDL.Video.Pixel_Formats.Pixel_Format_ARGB_8888,
                                        Kind     => SDL.Video.Textures.Streaming,
                                        Size     => Moose_Size);

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

         Moose_Frame := Moose_Frame + 1;

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

         Moose_Frame := Moose_Frame + 1;

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

            Start_Time    : Ada.Calendar.Time;
            End_Time      : Ada.Calendar.Time;
            Actual_Pixels : Texture_2D_Array (1 .. Moose_Size.Height, 1 .. Moose_Size.Width) with
              Address => To_Address (Pixels);
         begin
            Start_Time := Ada.Calendar.Clock;

            Actual_Pixels := Cache (Moose_Frame);

            End_Time := Ada.Calendar.Clock;
            SDL.Log.Put_Debug ("Update_Texture_3 took " & Duration'Image (End_Time - Start_Time) & " seconds.");
         end Update_Texture_3;

         Texture.Unlock;

         Renderer.Clear;
         Renderer.Copy (Texture);
         Renderer.Present;

         Moose_Frame := Moose_Frame + 1;

         --           if Moose_Frame = Moose_Frame'Last then
         --              Pixel := Data.Element_Array (0)'Access;
         --           end if;

         delay 0.05;
      end loop;

      W.Finalize;
      SDL.Finalise;
   end if;
end Stream;
