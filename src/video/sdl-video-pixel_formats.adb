--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;

package body SDL.Video.Pixel_Formats is
   function Image (Format : in Pixel_Format_Names) return String is
      function Convert is new Ada.Unchecked_Conversion (Source => Pixel_Format_Names, Target => Interfaces.Unsigned_32);

      --  Format was Pixel_Format_Names, but it returning the unknown value EVERY time.
      function SDL_Get_Pixel_Format_Name (Format : in Interfaces.Unsigned_32) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPixelFormatName";

      --  TODO: Now, it's not, don't know why.
      Converted : Interfaces.Unsigned_32 := Convert (Format);
   begin
      return C.Strings.Value (SDL_Get_Pixel_Format_Name (Converted));
   end Image;


   function To_Colour (Pixel : in Interfaces.Unsigned_32; Format : in Pixel_Format_Access) return Palettes.Colour is
      C : Palettes.Colour;
   begin
      To_Components (Pixel  => Pixel,
                     Format => Format,
                     Red    => C.Red,
                     Green  => C.Green,
                     Blue   => C.Blue,
                     Alpha  => C.Alpha);

      return C;
   end To_Colour;

   function To_Pixel (Colour : in Palettes.Colour; Format : in Pixel_Format_Access) return Interfaces.Unsigned_32 is
   begin
      return To_Pixel (Format => Format,
                       Red    => Colour.Red,
                       Green  => Colour.Green,
                       Blue   => Colour.Blue,
                       Alpha  => Colour.Alpha);
   end To_Pixel;

   function To_Masks
     (Format     : in  Pixel_Format_Names;
      Bits       : out Bits_Per_Pixels;
      Red_Mask   : out Colour_Mask;
      Green_Mask : out Colour_Mask;
      Blue_Mask  : out Colour_Mask;
      Alpha_Mask : out Colour_Mask) return Boolean is

      function SDL_Pixel_Format_Enum_To_Masks
        (Format     : in  Pixel_Format_Names;
         Bits       : out Bits_Per_Pixels;
         Red_Mask   : out Colour_Mask;
         Green_Mask : out Colour_Mask;
         Blue_Mask  : out Colour_Mask;
         Alpha_Mask : out Colour_Mask) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_PixelFormatEnumToMasks";

      Error : constant C.int := SDL_Pixel_Format_Enum_To_Masks
        (Format,
         Bits,
         Red_Mask,
         Green_Mask,
         Blue_Mask,
         Alpha_Mask);
   begin
      return Error = 1;
      --  TODO: This causes http://gcc.gnu.org/bugzilla/show_bug.cgi?id=58573
      --
      --  return (if SDL_Pixel_Format_Enum_To_Masks
      --    (Format,
      --     Bits,
      --     Red_Mask,
      --     Green_Mask,
      --     Blue_Mask,
      --     Alpha_Mask) = 1 then True else False);
      --  or:
      --  return (SDL_Pixel_Format_Enum_To_Masks
      --    (Format,
      --     Bits,
      --     Red_Mask,
      --     Green_Mask,
      --     Blue_Mask,
      --     Alpha_Mask) = 1);
   end To_Masks;
end SDL.Video.Pixel_Formats;
