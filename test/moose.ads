with SDL;
with SDL.Video.Palettes;
with SDL.Video.Pixels;

package Moose is
   use type SDL.Dimension;

   type Moose_Frames is mod 10;
   type Moose_Colour_Index is range 1 .. 84;
   type Moose_Palette_Array is array (Moose_Colour_Index'Range) of SDL.Video.Palettes.RGB_Colour;

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

   type Texture_2D_Array is array (SDL.Dimension range <>, SDL.Dimension range <>) of
     aliased SDL.Video.Pixels.ARGB_8888;

   type Cached_Moose_Frame_Array is array (Moose_Frames) of
     Texture_2D_Array (1 .. Moose_Size.Height, 1 .. Moose_Size.Width);

   Cache : Cached_Moose_Frame_Array;

   procedure Load_Moose_Data (Data : out Moose_Frame_Data_Array);

   procedure Cache_Moose (Cache   : in out Cached_Moose_Frame_Array;
                          Indices : in Moose_Frame_Data_Array;
                          Palette : in Moose_Palette_Array);
end Moose;
