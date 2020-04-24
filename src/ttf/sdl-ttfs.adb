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
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;
with SDL.Error;

package body SDL.TTFs is
   use type C.char_array;
   use type C.int;

   function Initialise return Boolean is
      function TTF_Init return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_Init";

      Result : C.int := TTF_Init;
   begin
      return (Result = Success);
   end Initialise;

   overriding
   procedure Finalize (Self : in out Fonts) is
      procedure TTF_Close_Font (Font : in Fonts_Ref) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_CloseFont";

      procedure TTF_Quit with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_Quit";
   begin
      if Self.Internal /= null then
         if Self.Source_Freed = False then
            TTF_Close_Font (Self.Internal);
         end if;

         Self.Internal := null;

         TTF_Quit;
      end if;
   end Finalize;

   function Style (Self : in Fonts) return Font_Styles is
      function TTF_Get_Font_Style (Font : in Fonts_Ref) return Font_Styles with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontStyle";
   begin
      return TTF_Get_Font_Style (Self.Internal);
   end Style;

   procedure Set_Style (Self : in out Fonts; Now : in Font_Styles) is
      procedure TTF_Set_Font_Style (Font : in Fonts_Ref; Now : in Font_Styles) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontStyle";
   begin
      TTF_Set_Font_Style (Self.Internal, Now);
   end Set_Style;

   function Outline (Self : in Fonts) return Font_Outlines is
      function TTF_Get_Font_Outline (Font : in Fonts_Ref) return Font_Outlines with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontOutline";
   begin
      return TTF_Get_Font_Outline (Self.Internal);
   end Outline;

   procedure Set_Outline (Self : in out Fonts; Now : in Font_Outlines := Outlines_Off) is
      procedure TTF_Set_Font_Outline (Font : in Fonts_Ref; Now : in Font_Outlines) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontOutline";
   begin
      TTF_Set_Font_Outline (Self.Internal, Now);
   end Set_Outline;

   function Hinting (Self : in Fonts) return Font_Hints is
      function TTF_Get_Font_Hinting (Font : in Fonts_Ref) return Font_Hints with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontHinting";
   begin
      return TTF_Get_Font_Hinting (Self.Internal);
   end Hinting;

   procedure Set_Hinting (Self : in out Fonts; Now : in Font_Hints := Normal) is
      procedure TTF_Set_Font_Hinting (Font : in Fonts_Ref; Now : in Font_Hints) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontHinting";
   begin
      TTF_Set_Font_Hinting (Self.Internal, Now);
   end Set_Hinting;

   function Kerning (Self : in Fonts) return Boolean is
      function TTF_Get_Font_Kerning (Font : in Fonts_Ref) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_GetFontKerning";

      Enabled : C.int := TTF_Get_Font_Kerning (Self.Internal);
   begin
      return (if Enabled = 0 then False else True);
   end Kerning;

   procedure Set_Kerning (Self : in out Fonts; Now : in Boolean) is
      procedure TTF_Set_Font_Kerning (Font : in Fonts_Ref; Now : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SetFontKerning";
   begin
      TTF_Set_Font_Kerning (Font => Self.Internal,
                            Now  => (if Now = True then 1 else 0));
   end Set_Kerning;

   function Height (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Height (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontHeight";
   begin
      return TTF_Font_Height (Self.Internal);
   end Height;

   function Ascent (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Ascent (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontAscent";
   begin
      return TTF_Font_Ascent (Self.Internal);
   end Ascent;

   function Descent (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Descent (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontDescent";
   begin
      return TTF_Font_Descent (Self.Internal);
   end Descent;

   function Line_Skip (Self : in Fonts) return Font_Measurements is
      function TTF_Font_Line_Skip (Font : in Fonts_Ref) return Font_Measurements with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontLineSkip";
   begin
      return TTF_Font_Line_Skip (Self.Internal);
   end Line_Skip;

   function Faces (Self : in Fonts) return Font_Faces is
      function TTF_Font_Faces (Font : in Fonts_Ref) return Font_Faces with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaces";
   begin
      return TTF_Font_Faces (Self.Internal);
   end Faces;

   function Is_Face_Fixed_Width (Self : in Fonts) return Boolean is
      function TTF_Font_Face_Is_Fixed_Width (Font : in Fonts_Ref) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaceIsFixedWidth";

      Result : C.int := TTF_Font_Face_Is_Fixed_Width (Self.Internal);
   begin
      return (if Result > 0 then True else False);
   end Is_Face_Fixed_Width;

   function Face_Family_Name (Self : in Fonts) return String is
      function TTF_Font_Face_Family_Name (Font : in Fonts_Ref) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaceFamilyName";
   begin
      return C.Strings.Value (TTF_Font_Face_Family_Name (Self.Internal));
   end Face_Family_Name;

   function Face_Style_Name (Self : in Fonts) return String is
      function TTF_Font_Face_Style_Name (Font : in Fonts_Ref) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_FontFaceStyleName";
   begin
      return C.Strings.Value (TTF_Font_Face_Style_Name (Self.Internal));
   end Face_Style_Name;

   function Size_Latin_1 (Self : in Fonts; Text : in String) return SDL.Sizes is
      function TTF_Size_Text (Font : in Fonts_Ref;
                              Text : in C.Strings.chars_ptr;
                              W    : out Dimension;
                              H    : out Dimension) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SizeText";

      Size   : SDL.Sizes           := SDL.Zero_Size;
      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
      Result : C.int               := TTF_Size_Text (Self.Internal, C_Text, Size.Width, Size.Height);
   begin
      C.Strings.Free (C_Text);

      return Size;
   end Size_Latin_1;

   function Size_UTF_8 (Self : in Fonts; Text : in UTF_Strings.UTF_8_String) return SDL.Sizes is
      function TTF_Size_UTF_8 (Font : in Fonts_Ref;
                               Text : in C.Strings.chars_ptr;
                               W    : out Dimension;
                               H    : out Dimension) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_SizeUTF8";

      Size   : SDL.Sizes           := SDL.Zero_Size;
      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
      Result : C.int               := TTF_Size_UTF_8 (Self.Internal, C_Text, Size.Width, Size.Height);
   begin
      return Size;
   end Size_UTF_8;

   function Make_Surface_From_Pointer (S    : in Video.Surfaces.Internal_Surface_Pointer;
                                       Owns : in Boolean := False) return Video.Surfaces.Surface with
     Import     => True,
     Convention => Ada;

   function Render_Solid (Self   : in Fonts;
                          Text   : in String;
                          Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_Text_Solid (Font   : in Fonts_Ref;
                                      Text   : in C.Strings.chars_ptr;
                                      Colour : in SDL.Video.Palettes.Colour)
                                      return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderText_Solid";

      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
   begin
      return S : SDL.Video.Surfaces.Surface :=
        Make_Surface_From_Pointer (S    => TTF_Render_Text_Solid (Self.Internal, C_Text, Colour),
                                   Owns => True)
      do
         C.Strings.Free (C_Text);
      end return;
   end Render_Solid;

   function Render_Shaded (Self              : in Fonts;
                           Text              : in String;
                           Colour            : in SDL.Video.Palettes.Colour;
                           Background_Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_Text_Shaded (Font              : in Fonts_Ref;
                                       Text              : in C.Strings.chars_ptr;
                                       Colour            : in SDL.Video.Palettes.Colour;
                                       Background_Colour : in SDL.Video.Palettes.Colour)
                                       return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderText_Shaded";

      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
   begin
      return S : SDL.Video.Surfaces.Surface :=
        Make_Surface_From_Pointer (S    => TTF_Render_Text_Shaded (Self.Internal, C_Text, Colour, Background_Colour),
                                   Owns => True)
      do
         C.Strings.Free (C_Text);
      end return;
   end Render_Shaded;

   function Render_Blended (Self   : in Fonts;
                          Text   : in String;
                          Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_Text_Blended (Font   : in Fonts_Ref;
                                        Text   : in C.Strings.chars_ptr;
                                        Colour : in SDL.Video.Palettes.Colour)
                                        return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderText_Blended";

      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
   begin
      return S : SDL.Video.Surfaces.Surface :=
        Make_Surface_From_Pointer (S    => TTF_Render_Text_Blended (Self.Internal, C_Text, Colour),
                                   Owns => True)
      do
         C.Strings.Free (C_Text);
      end return;
   end Render_Blended;

   function Render_UTF_8_Solid (Self     : in Fonts;
                                Text     : in UTF_Strings.UTF_8_String;
                                Colour   : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_UTF_8_Solid (Font    : in Fonts_Ref;
                                       Text    : in C.Strings.chars_ptr;
                                       Colour  : in SDL.Video.Palettes.Colour)
                                       return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderUTF8_Solid";

      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
   begin
      return S : SDL.Video.Surfaces.Surface :=
        Make_Surface_From_Pointer (S    => TTF_Render_UTF_8_Solid (Self.Internal, C_Text, Colour),
                                   Owns => True)
      do
         C.Strings.Free (C_Text);
      end return;
   end Render_UTF_8_Solid;

   function Render_UTF_8_Shaded (Self              : in Fonts;
                                 Text              : in UTF_Strings.UTF_8_String;
                                 Colour            : in SDL.Video.Palettes.Colour;
                                 Background_Colour : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_UTF_8_Shaded (Font              : in Fonts_Ref;
                                        Text              : in C.Strings.chars_ptr;
                                        Colour            : in SDL.Video.Palettes.Colour;
                                        Background_Colour : in SDL.Video.Palettes.Colour)
                                        return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderUTF8_Shaded";

      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
   begin
      return S : SDL.Video.Surfaces.Surface :=
        Make_Surface_From_Pointer (S    => TTF_Render_UTF_8_Shaded (Self.Internal, C_Text, Colour, Background_Colour),
                                   Owns => True)
      do
         C.Strings.Free (C_Text);
      end return;
   end Render_UTF_8_Shaded;

   function Render_UTF_8_Blended (Self              : in Fonts;
                                  Text              : in UTF_Strings.UTF_8_String;
                                  Colour            : in SDL.Video.Palettes.Colour) return SDL.Video.Surfaces.Surface is
      function TTF_Render_UTF_8_Blended (Font   : in Fonts_Ref;
                                         Text   : in C.Strings.chars_ptr;
                                         Colour : in SDL.Video.Palettes.Colour)
                                         return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_RenderUTF8_Blended";

      C_Text : C.Strings.chars_ptr := C.Strings.New_String (Text);
   begin
      return S : SDL.Video.Surfaces.Surface :=
        Make_Surface_From_Pointer (S    => TTF_Render_UTF_8_Blended (Self.Internal, C_Text, Colour),
                                   Owns => True)
      do
         C.Strings.Free (C_Text);
      end return;
   end Render_UTF_8_Blended;
end SDL.TTFs;
