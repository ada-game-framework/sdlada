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
with Interfaces.C;
with System;
with Ada.Unchecked_Conversion;
with SDL.Error;

package body SDL.Video.Textures is
   package C renames Interfaces.C;

   use type SDL.C_Pointers.Texture_Pointer;

   procedure Destroy (Self : in out Texture) is
      procedure SDL_Destroy_Texture (T : in SDL.C_Pointers.Texture_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyTexture";
   begin
      SDL_Destroy_Texture (Self.Internal);

      Self.Internal := null;
   end Destroy;

   function Get_Alpha (Self : in Texture) return SDL.Video.Palettes.Colour_Component is
      function SDL_Get_Texture_Alpha_Mod (T     : in SDL.C_Pointers.Texture_Pointer;
                                          Alpha : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureAlphaMod";

      Data   : SDL.Video.Palettes.Colour_Component;
      Result : constant C.int := SDL_Get_Texture_Alpha_Mod (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Alpha;

   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL.Video.Palettes.Colour_Component) is
      function SDL_Set_Texture_Alpha_Mod (T     : in SDL.C_Pointers.Texture_Pointer;
                                          Alpha : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureAlphaMod";

      Result : constant C.int := SDL_Set_Texture_Alpha_Mod (Self.Internal, Alpha);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Alpha;

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes is
      function SDL_Get_Texture_Blend_Mode (T     : in SDL.C_Pointers.Texture_Pointer;
                                           Blend : out Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureBlendMode";

      Data   : Blend_Modes;
      Result : constant C.int := SDL_Get_Texture_Blend_Mode (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes) is
      function SDL_Set_Texture_Blend_Mode (T    : in SDL.C_Pointers.Texture_Pointer;
                                           Mode : in Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureBlendMode";

      Result : constant C.int := SDL_Set_Texture_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Modulate_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour is
      function SDL_Get_Texture_Color_Mod (T       : in SDL.C_Pointers.Texture_Pointer;
                                          R, G, B : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureColorMod";

      Data   : SDL.Video.Palettes.RGB_Colour;
      Result : constant C.int := SDL_Get_Texture_Color_Mod (Self.Internal, Data.Red, Data.Green, Data.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Modulate_Colour;

   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour) is
      function SDL_Set_Texture_Color_Mod (T       : in SDL.C_Pointers.Texture_Pointer;
                                          R, G, B : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureColorMod";

      Result : constant C.int := SDL_Set_Texture_Color_Mod (Self.Internal, Colour.Red, Colour.Green, Colour.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Modulate_Colour;

   --     procedure Lock_Texture (Self   : in out Texture;
   --                             Pixels : out SDL.Video.Pixels.Pixel_ARGB_8888_Array_Access) is
   --        type Int_Ptr is access C.int with
   --          Convention => C;
   --
   --        function SDL_Lock_Texture (T      : in System.Address;
   --                                   Area   : in System.Address;
   --                                   Pixels : out SDL.Video.Pixels.C_Pixel_Ptr;
   --                                   Pitch  : out Int_Ptr) return C.int with
   --          Import        => True,
   --          Convention    => C,
   --          External_Name => "SDL_LockTexture";
   --
   --        C_Pixels : SDL.Video.Pixels.C_Pixel_Ptr := null;
   --        C_Pitch  : Int_Ptr        := null;
   --        Result   : C.int          := SDL_Lock_Texture (Self.Internal, System.Null_Address, C_Pixels, C_Pitch);
   --     begin
   --        if Result /= Success then
   --           raise Texture_Error with SDL.Error.Get;
   --        end if;
   --
   --        Self.Locked := True;
   --
   --        Pixels := SDL.Video.Pixels.Create (C_Pixels, C_Pitch.all, Self.Size);
   --     end Lock_Texture;

   procedure Lock (Self   : in out Texture;
                   Pixels : out Pixel_Pointer_Type) is
      function SDL_Lock_Texture (T      : in SDL.C_Pointers.Texture_Pointer;
                                 Area   : in System.Address;
                                 Pixels : out Pixel_Pointer_Type;
                                 Pitch  : out SDL.Video.Pixels.Pitches) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LockTexture";

      --  Don't care about Pitch value.
      Dummy  : SDL.Video.Pixels.Pitches := 0;
      Result : constant C.int := SDL_Lock_Texture (Self.Internal, System.Null_Address, Pixels, Dummy);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      Self.Locked := True;
   end Lock;

   procedure Lock_Area (Self   : in out Texture;
                        Area   : in SDL.Video.Rectangles.Rectangle;
                        Pixels : out Pixel_Pointer_Type;
                        Pitch  : out SDL.Video.Pixels.Pitches) is
      function SDL_Lock_Texture (T      : in SDL.C_Pointers.Texture_Pointer;
                                 Area   : in SDL.Video.Rectangles.Rectangle;
                                 Pixels : out Pixel_Pointer_Type;
                                 Pitch  : out SDL.Video.Pixels.Pitches) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LockTexture";

      Result : constant C.int := SDL_Lock_Texture (Self.Internal, Area, Pixels, Pitch);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      Self.Locked := True;
   end Lock_Area;

   procedure Unlock (Self : in out Texture) is
      procedure SDL_Unlock_Texture (T : in SDL.C_Pointers.Texture_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UnlockTexture";
   begin
      if Self.Locked then
         SDL_Unlock_Texture (Self.Internal);

         Self.Locked := False;
      end if;
   end Unlock;

   procedure Query (Self              : in Texture;
                    Pixel_Format_Name : out SDL.Video.Pixel_Formats.Pixel_Format_Names;
                    Kind              : out Kinds;
                    Size              : out SDL.Sizes) is
      function SDL_Query_Texture (T                 : in SDL.C_Pointers.Texture_Pointer;
                                  Pixel_Format_Name : out SDL.Video.Pixel_Formats.Pixel_Format_Names;
                                  Kind              : out Kinds;
                                  Width             : out SDL.Dimension;
                                  Height            : out SDL.Dimension) return C.int with
        import        => True,
        Convention    => C,
        External_Name => "SDL_QueryTexture";

      W      : SDL.Dimension  := 0;
      H      : SDL.Dimension  := 0;
      Result : constant C.int := SDL_Query_Texture (Self.Internal, Pixel_Format_Name, Kind, W, H);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      Size := SDL.Sizes'(W, H);
   end Query;

   function Get_Pixel_Format (Self : in Texture) return SDL.Video.Pixel_Formats.Pixel_Format_Names is
      function SDL_Query_Texture (T                 : in SDL.C_Pointers.Texture_Pointer;
                                  Pixel_Format_Name : out SDL.Video.Pixel_Formats.Pixel_Format_Names;
                                  Kind              : in System.Address := System.Null_Address;
                                  Width             : in System.Address := System.Null_Address;
                                  Height            : in System.Address := System.Null_Address) return C.int with
        import        => True,
        Convention    => C,
        External_Name => "SDL_QueryTexture";

      Format_Name : SDL.Video.Pixel_Formats.Pixel_Format_Names;
      Result      : constant C.int := SDL_Query_Texture (T => Self.Internal, Pixel_Format_Name => Format_Name);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Format_Name;
   end Get_Pixel_Format;

   function Get_Kind (Self : in Texture) return Kinds is
      function SDL_Query_Texture (T                 : in SDL.C_Pointers.Texture_Pointer;
                                  Pixel_Format_Name : in System.Address := System.Null_Address;
                                  Kind              : out Kinds;
                                  Width             : in System.Address := System.Null_Address;
                                  Height            : in System.Address := System.Null_Address) return C.int with
        import        => True,
        Convention    => C,
        External_Name => "SDL_QueryTexture";

      Kind   : Kinds;
      Result : constant C.int := SDL_Query_Texture (T => Self.Internal, Kind => Kind);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Kind;
   end Get_Kind;

   function Get_Size (Self : in Texture) return SDL.Sizes is
      function SDL_Query_Texture (T                 : in SDL.C_Pointers.Texture_Pointer;
                                  Pixel_Format_Name : in System.Address := System.Null_Address;
                                  Kind              : in System.Address := System.Null_Address;
                                  Width             : out SDL.Dimension;
                                  Height            : out SDL.Dimension) return C.int with
        import        => True,
        Convention    => C,
        External_Name => "SDL_QueryTexture";

      Size   : SDL.Sizes      := SDL.Zero_Size;
      Result : constant C.int := SDL_Query_Texture (T => Self.Internal, Width => Size.Width, Height => Size.Height);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Size;
   end Get_Size;

   overriding
   procedure Finalize (Self : in out Texture) is
   begin
      if Self.Internal /= null and then Self.Owns then
         Destroy (Self);
      end if;
   end Finalize;

   function Get_Internal_Texture (Self : in Texture) return SDL.C_Pointers.Texture_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Texture;
end SDL.Video.Textures;
