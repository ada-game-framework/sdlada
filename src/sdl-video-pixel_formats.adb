with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Video.Pixel_Formats is
   use type C.int;

   function Image (Format : in Pixel_Format_Names) return String is
      function SDL_Get_Pixel_Format_Name (Format : in Pixel_Format_Names) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPixelFormatName";

      C_Str : C.Strings.chars_ptr := SDL_Get_Pixel_Format_Name (Format);
   begin
      return C.Strings.Value (C_Str);
   end Image;

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

      Error : C.int := SDL_Pixel_Format_Enum_To_Masks
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
