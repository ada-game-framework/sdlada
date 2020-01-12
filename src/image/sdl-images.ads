--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Images
--
--  Root package implementing the binding to SDL2_mage.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Images is
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
