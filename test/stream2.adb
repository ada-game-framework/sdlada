with Ada.Calendar;
with Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Conversion;
with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Log;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with System;

procedure Stream2 is
   use type SDL.Dimension;
   use type SDL.Positive_Sizes;

   type Moose_Frames is mod 10;
   --     type Moose_Frames is range 1 .. 10;
   type Moose_Colour_Index is range 1 .. 84;
   type Moose_Palette_Array is array (Moose_Colour_Index'Range) of SDL.Video.Palettes.RGB_Colour;

   W                : SDL.Video.Windows.Window;
   Moose_Size       : constant SDL.Positive_Sizes     := (64, 88);
   Moose_Frame_Size : constant SDL.Positive_Dimension := (Moose_Size.Width * Moose_Size.Height) - 1;
   Moose_Frame      : Moose_Frames                    := Moose_Frames'First;
   Moose_Palette    : constant Moose_Palette_Array    :=
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

   type Texture_2D_Array is array (SDL.Natural_Dimension range <>, SDL.Natural_Dimension range <>) of
     aliased SDL.Video.Pixels.ARGB_8888;

   type Cached_Moose_Frame_Array is array (Moose_Frames) of
     Texture_2D_Array (1 .. Moose_Size.Height, 1 .. Moose_Size.Width);

   procedure Cache_Moose (Cache   : in out Cached_Moose_Frame_Array;
                          Indices : in Moose_Frame_Data_Array;
                          Palette : Moose_Palette_Array) is

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

      --  Main loop.
      declare
         Event    : SDL.Events.Events.Events;
         Finished : Boolean := False;

         use type SDL.Events.Keyboards.Key_Codes;
         use type SDL.Events.Keyboards.Scan_Codes;
      begin
         loop
            while SDL.Events.Events.Poll (Event) loop
               case Event.Common.Event_Type is
                  when SDL.Events.Quit =>
                     Finished := True;

                  when SDL.Events.Keyboards.Key_Down =>
                     if Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape then
                        Finished := True;
                        --  elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
                        --     if Moose_Frame = Moose_Frames'First then
                        --        Moose_Frame := Moose_Frames'Last;
                        --     else
                        --        Moose_Frame := Moose_Frame - 1;
                        --     end if;
                        --  elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
                        --     if Moose_Frame = Moose_Frames'Last then
                        --        Moose_Frame := Moose_Frames'First;
                        --     else
                        --        Moose_Frame := Moose_Frame + 1;
                        --     end if;
                     elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
                        Moose_Frame := Moose_Frame - 1;
                     elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
                        Moose_Frame := Moose_Frame + 1;
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            Lock (Texture, Pixels);

            declare
               function To_Address is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
                                                                    Target => System.Address);

               Start_Time       : Ada.Calendar.Time;
               End_Time         : Ada.Calendar.Time;
               Actual_Pixels    : Texture_2D_Array (1 .. Moose_Size.Height, 1 .. Moose_Size.Width) with
                 Address => To_Address (Pixels);
            begin
               Start_Time := Ada.Calendar.Clock;

               Actual_Pixels := Cache (Moose_Frame);

               End_Time := Ada.Calendar.Clock;
            end;

            Texture.Unlock;

            Renderer.Clear;
            Renderer.Copy (Texture);
            Renderer.Present;

            exit when Finished;
         end loop;
      end;

      W.Finalize;
      SDL.Finalise;
   end if;
end Stream2;
