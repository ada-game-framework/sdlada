--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Images
--------------------------------------------------------------------------------------------------------------------

package body SDL.Images is
   use type C.char_array;

   Format_Strings : constant Format_String_Arrays :=
     ("TGA"  & C.char'Val (0) & C.char'Val (0),
      "CUR"  & C.char'Val (0) & C.char'Val (0),
      "ICO"  & C.char'Val (0) & C.char'Val (0),
      "BMP"  & C.char'Val (0) & C.char'Val (0),
      "GIF"  & C.char'Val (0) & C.char'Val (0),
      "JPG"  & C.char'Val (0) & C.char'Val (0),
      "LBM"  & C.char'Val (0) & C.char'Val (0),
      "PCX"  & C.char'Val (0) & C.char'Val (0),
      "PNG"  & C.char'Val (0) & C.char'Val (0),
      "PNM"  & C.char'Val (0) & C.char'Val (0),
      "TIF"  & C.char'Val (0) & C.char'Val (0),
      "XCF"  & C.char'Val (0) & C.char'Val (0),
      "XPM"  & C.char'Val (0) & C.char'Val (0),
      "XV"   & C.char'Val (0) & C.char'Val (0) & C.char'Val (0),
      "WEBP" & C.char'Val (0));

   overriding
   function Initialise (Flags : in Init_Image_Flags := Enable_Everything) return Boolean is
      function IMG_Init (Flags : in Init_Image_Flags) return Init_Image_Flags with
        Import        => True,
        Convention    => C,
        External_Name => "IMG_Init";

      Result : constant Init_Image_Flags := IMG_Init (Flags);
   begin
      if Result = 0 then
         return False;
      end if;

      return True;
   end Initialise;

   function Format_String (Format : in Formats) return Format_String_Names is
   begin
      return Format_Strings (Format);
   end Format_String;
end SDL.Images;
