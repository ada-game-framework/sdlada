--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Images
--
--  Root package implementing the binding to SDL2_mage.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Images is
   pragma Pure;

   package C renames Interfaces.C;

   Image_Error : exception;

   type Init_Image_Flags is new SDL.Init_Flags;

   Enable_JPG        : constant Init_Image_Flags := 16#0000_0001#;
   Enable_PNG        : constant Init_Image_Flags := 16#0000_0002#;
   Enable_TIFF       : constant Init_Image_Flags := 16#0000_0004#;
   Enable_WEBP       : constant Init_Image_Flags := 16#0000_0008#;
   Enable_Everything : constant Init_Image_Flags := Enable_JPG or Enable_PNG or Enable_TIFF or Enable_WEBP;

   type Formats is (Targa, Cursor, Icon, BMP, GIF, JPG, LBM, PCX, PNG, PNM, TIFF, XCF, XPM, XV, WEBP);

   overriding
   function Initialise (Flags : in Init_Image_Flags := Enable_Everything) return Boolean;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "IMG_Quit";
private
   subtype Format_String_Names is C.char_array (1 .. 5);
   type Format_String_Arrays is array (Formats) of Format_String_Names;

   function Format_String (Format : in Formats) return Format_String_Names with
     Inline_Always => True;
end SDL.Images;
