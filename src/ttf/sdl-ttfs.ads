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
--  SDL.TTFs
--
--  Root package implementing the binding to SDL2_ttf.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with Ada.Strings.UTF_Encoding;
with Interfaces.C;
with SDL.Video.Palettes;
with SDL.Video.Surfaces;

package SDL.TTFs is
   package UTF_Strings renames Ada.Strings.UTF_Encoding;
   package C renames Interfaces.C;

   TTF_Error : exception;

   function Initialise return Boolean with
     Inline_Always => True;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "TTF_Quit";

   --  Fonts.
   type Point_Sizes is new C.int;
   type Font_Faces is range 0 .. C.long'Last with
     Size       => C.long'Size,
     Convention => C;

   type Font_Styles is mod 2 ** 32 with
     Convention => C;

   Style_Normal         : constant Font_Styles := 16#0000_0000#;
   Style_Bold           : constant Font_Styles := 16#0000_0001#;
   Style_Italic         : constant Font_Styles := 16#0000_0002#;
   Style_Underline      : constant Font_Styles := 16#0000_0004#;
   Style_Strike_Through : constant Font_Styles := 16#0000_0008#;

   type Font_Outlines is range 0 .. C.int'Last with
     Size       => C.int'Size,
     Convention => C;

   Outlines_Off : constant Font_Outlines := Font_Outlines'First;

   type Font_Hints is (Normal, Light, Mono, None) with
     Convention => C;

   type Font_Measurements is range 0 .. C.int'Last with
     Size       => C.int'Size,
     Convention => C;

   type Fonts is new Ada.Finalization.Controlled with private;

   Null_Font : constant Fonts;

   overriding
   procedure Finalize (Self : in out Fonts);

   function Style (Self : in Fonts) return Font_Styles with
     Inline => True;

   procedure Set_Style (Self : in out Fonts; Now : in Font_Styles) with
     Inline => True;

   function Outline (Self : in Fonts) return Font_Outlines with
     Inline => True;

   procedure Set_Outline (Self : in out Fonts; Now : in Font_Outlines := Outlines_Off) with
     Inline => True;

   function Hinting (Self : in Fonts) return Font_Hints with
     Inline => True;

   procedure Set_Hinting (Self : in out Fonts; Now : in Font_Hints := Normal) with
     Inline => True;

   function Kerning (Self : in Fonts) return Boolean with
     Inline => True;

   procedure Set_Kerning (Self : in out Fonts; Now : in Boolean) with
     Inline => True;

   function Height (Self : in Fonts) return Font_Measurements with
     Inline => True;

   function Ascent (Self : in Fonts) return Font_Measurements with
     Inline => True;

   function Descent (Self : in Fonts) return Font_Measurements with
     Inline => True;

   function Line_Skip (Self : in Fonts) return Font_Measurements with
     Inline => True;

   function Faces (Self : in Fonts) return Font_Faces with
     Inline => True;

   function Is_Face_Fixed_Width (Self : in Fonts) return Boolean with
     Inline => True;

   function Face_Family_Name (Self : in Fonts) return String with
     Inline => True;

   function Face_Style_Name (Self : in Fonts) return String with
     Inline => True;

   function Size_Latin_1 (Self : in Fonts; Text : in String) return SDL.Sizes with
     Inline => True;

   function Size_UTF_8 (Self : in Fonts; Text : in UTF_Strings.UTF_8_String) return SDL.Sizes with
     Inline => True;

   function Render_Solid (Self   : in Fonts;
                          Text   : in String;
                          Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface;

   function Render_Shaded (Self              : in Fonts;
                           Text              : in String;
                           Colour            : in SDL.Video.Palettes.Colour;
                           Background_Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface;

   function Render_Blended (Self   : in Fonts;
                            Text   : in String;
                            Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface;

   function Render_UTF_8_Solid (Self     : in Fonts;
                                Text     : in UTF_Strings.UTF_8_String;
                                Colour   : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface;

   function Render_UTF_8_Shaded (Self              : in Fonts;
                                 Text              : in UTF_Strings.UTF_8_String;
                                 Colour            : in SDL.Video.Palettes.Colour;
                                 Background_Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface;

   function Render_UTF_8_Blended (Self              : in Fonts;
                                  Text              : in UTF_Strings.UTF_8_String;
                                  Colour            : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface;
private
   type Internal_Fonts is null record;
   type Fonts_Pointer is access all Internal_Fonts with
     Convention => C;
   subtype Fonts_Ref is not null Fonts_Pointer;

   type Fonts is new Ada.Finalization.Controlled with
      record
         Internal     : Fonts_Pointer := null;
         Source_Freed : Boolean       := False;  -- Whether the Makers.* subprogram has already closed the font.
      end record;

   Null_Font : constant Fonts := (Ada.Finalization.Controlled with others => <>);
end SDL.TTFs;
