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
with Ada.Unchecked_Conversion;
with SDL.Error;

package body SDL.Video.Surfaces is

   function Pixel_Format (Self : in Surface) return Pixel_Formats.Pixel_Format_Access is
   begin
      return Self.Internal.Pixel_Format;
   end Pixel_Format;

   function Size (Self : in Surface) return SDL.Sizes is
   begin
      return SDL.Sizes'(Self.Internal.Width, Self.Internal.Height);
   end Size;

   function Pixels (Self : in Surface) return System.Address is
   begin
      if Must_Lock (Self) and then Self.Internal.Locked <= 0 then
         raise Surface_Error with "Surface must be locked before access can be gained to the pixel data.";
      end if;

      return Self.Internal.Pixels;
   end Pixels;

   package body User_Data is
      function Convert is new Ada.Unchecked_Conversion (Source => Data_Pointer,
                                                        Target => User_Data_Pointer);

      function Convert is new Ada.Unchecked_Conversion (Source => User_Data_Pointer,
                                                        Target => Data_Pointer);

      function Get (Self : in Surface) return Data_Pointer is
      begin
         return Convert (Self.Internal.User_Data);
      end Get;

      procedure Set (Self : in out Surface; Data : in Data_Pointer) is
      begin
         Self.Internal.User_Data := Convert (Data);
      end Set;
   end User_Data;

   procedure Blit (Self        : in out Surface;
                   Source      : in Surface) is
      function SDL_Blit_Surface (S  : in Internal_Surface_Pointer;
                                 SR : access Rectangles.Rectangle;
                                 D  : in Internal_Surface_Pointer;
                                 DR : access Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpperBlit";  --  SDL_BlitSurface is a macro in SDL_surface.h

      Result : constant C.int := SDL_Blit_Surface (Source.Internal, null, Self.Internal, null);
   begin
      if Result /= SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Blit;

   procedure Blit (Self        : in out Surface;
                   Self_Area   : in out Rectangles.Rectangle;
                   Source      : in Surface;
                   Source_Area : in out Rectangles.Rectangle) is
      function SDL_Blit_Surface (S  : in Internal_Surface_Pointer;
                                 SR : access Rectangles.Rectangle;
                                 D  : in Internal_Surface_Pointer;
                                 DR : access Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpperBlit";  --  SDL_BlitSurface is a macro in SDL_surface.h
      use type Rectangles.Rectangle;

      Result    : C.int := 0;
      Src_Area  : aliased Rectangles.Rectangle := Source_Area;
      Dest_Area : aliased Rectangles.Rectangle := Self_Area;
   begin
      if Dest_Area = Rectangles.Null_Rectangle then
         if Src_Area = Rectangles.Null_Rectangle then
            Result := SDL_Blit_Surface (Source.Internal, null, Self.Internal, null);
         else
            Result := SDL_Blit_Surface (Source.Internal, Src_Area'Access, Self.Internal, null);

            Source_Area := Src_Area;
         end if;
      else
         if Src_Area = Rectangles.Null_Rectangle then
            Result := SDL_Blit_Surface (Source.Internal, null, Self.Internal, Dest_Area'Access);
         else
            Result := SDL_Blit_Surface (Source.Internal, Src_Area'Access, Self.Internal, Dest_Area'Access);

            Source_Area := Src_Area;
         end if;

         Self_Area := Dest_Area;
      end if;

      if Result /= SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Blit;

   procedure Blit_Scaled (Self        : in out Surface;
                          Source      : in Surface) is
      function SDL_Blit_Scaled (S  : in Internal_Surface_Pointer;
                                SR : access Rectangles.Rectangle;
                                D  : in Internal_Surface_Pointer;
                                DR : access Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpperBlitScaled";  --  SDL_BlitScaled is a macro in SDL_surface.h

      Result : constant C.int := SDL_Blit_Scaled (Source.Internal, null, Self.Internal, null);
   begin
      if Result /= SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Blit_Scaled;

   --  Blit_Scaled
   --
   --  Self        : The destination surface to blit onto.
   --  Self_Area   : The coordinates and size of the area to blit into.
   --  Source      : The surface to blit onto Self.
   --  Source_Area : The coordinates and size of the area to blit from.
   procedure Blit_Scaled (Self        : in out Surface;
                          Self_Area   : in out Rectangles.Rectangle;
                          Source      : in Surface;
                          Source_Area : in Rectangles.Rectangle := Rectangles.Null_Rectangle) is
      function SDL_Blit_Scaled (S  : in Internal_Surface_Pointer;
                                SR : access Rectangles.Rectangle;
                                D  : in Internal_Surface_Pointer;
                                DR : access Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpperBlitScaled";  --  SDL_BlitScaled is a macro in SDL_surface.h
      use type Rectangles.Rectangle;

      Result   : C.int                        := 0;
      Area     : aliased Rectangles.Rectangle := Self_Area;
      Src_Area : aliased Rectangles.Rectangle := Source_Area;
   begin
      if Self_Area = Rectangles.Null_Rectangle then
         if Source_Area = Rectangles.Null_Rectangle then
            Result := SDL_Blit_Scaled (Source.Internal, null, Self.Internal, null);
         else
            Result := SDL_Blit_Scaled (Source.Internal, Src_Area'Access, Self.Internal, null);
         end if;
      else
         if Source_Area = Rectangles.Null_Rectangle then
            Result := SDL_Blit_Scaled (Source.Internal, null, Self.Internal, Area'Access);
         else
            Result := SDL_Blit_Scaled (Source.Internal, Src_Area'Access, Self.Internal, Area'Access);
         end if;

         Self_Area := Area;
      end if;

      if Result /= SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Blit_Scaled;

   procedure Lower_Blit (Self        : in out Surface;
                         Self_Area   : in Rectangles.Rectangle;
                         Source      : in Surface;
                         Source_Area : in Rectangles.Rectangle) is
      function SDL_Lower_Blit (S  : in Internal_Surface_Pointer;
                               SR : in Rectangles.Rectangle;
                               D  : in Internal_Surface_Pointer;
                               DR : in Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LowerBlit";

      Result : constant C.int := SDL_Lower_Blit (Source.Internal, Source_Area, Self.Internal, Self_Area);
   begin
      if Result /= SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Lower_Blit;

   procedure Lower_Blit_Scaled (Self        : in out Surface;
                                Self_Area   : in Rectangles.Rectangle;
                                Source      : in Surface;
                                Source_Area : in Rectangles.Rectangle) is
      function SDL_Lower_Blit_Scaled (S  : in Internal_Surface_Pointer;
                                      SR : in Rectangles.Rectangle;
                                      D  : in Internal_Surface_Pointer;
                                      DR : in Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LowerBlitScaled";

      Result : constant C.int := SDL_Lower_Blit_Scaled (Source.Internal, Source_Area, Self.Internal, Self_Area);
   begin
      if Result /= SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Lower_Blit_Scaled;

   procedure Fill (Self   : in out Surface;
                   Area   : in Rectangles.Rectangle;
                   Colour : in Interfaces.Unsigned_32) is
      function SDL_Fill_Rect (S      : in Internal_Surface_Pointer;
                              Rect   : in Rectangles.Rectangle;
                              Colour : in Interfaces.Unsigned_32) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_FillRect";
      Result : constant C.int := SDL_Fill_Rect (Self.Internal, Area, Colour);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Fill;

   procedure Fill (Self   : in out Surface;
                   Areas  : in Rectangles.Rectangle_Arrays;
                   Colour : in Interfaces.Unsigned_32) is
      function SDL_Fill_Rects (S      : in Internal_Surface_Pointer;
                               Rects  : in Rectangles.Rectangle_Arrays;
                               Count  : in C.int;
                               Colour : in Interfaces.Unsigned_32) return C.int with
        Import => True,
        Convention => C,
        External_Name => "SDL_FillRects";

      Result : constant C.int := SDL_Fill_Rects (Self.Internal, Areas, Areas'Length, Colour);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Fill;

   function Clip_Rectangle (Self : in Surface) return Rectangles.Rectangle is
      procedure SDL_Get_Clip_Rect (S : in Internal_Surface_Pointer;
                                   R : out Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetClipRect";
   begin
      return Result : Rectangles.Rectangle := Rectangles.Null_Rectangle do
         SDL_Get_Clip_Rect (Self.Internal, Result);
      end return;
   end Clip_Rectangle;

   procedure Set_Clip_Rectangle (Self : in out Surface; Now : in Rectangles.Rectangle) is
      function SDL_Set_Clip_Rect (S : in Internal_Surface_Pointer;
                                  R : in Rectangles.Rectangle) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetClipRect";

      Result : constant SDL_Bool := SDL_Set_Clip_Rect (S => Self.Internal, R => Now);
   begin
      if Result = SDL_False then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Set_Clip_Rectangle;

   function Colour_Key (Self : in Surface) return Palettes.Colour is
      function SDL_Get_Color_Key (S : in Internal_Surface_Pointer;
                                  K : out Interfaces.Unsigned_32) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetColorKey";

      Key    : Interfaces.Unsigned_32;
      Result : constant C.int := SDL_Get_Color_Key (Self.Internal, Key);
   begin
      if Result < SDL.Success then
         --  TODO: The SDL source does not set an error message, see https://bugzilla.libsdl.org/show_bug.cgi?id=3992
         raise Surface_Error with "No colour key set for this surface."; --  with SDL.Error.Get;
      end if;

      return Pixel_Formats.To_Colour (Pixel  => Key,
                                      Format => Self.Pixel_Format);
   end Colour_Key;

   procedure Set_Colour_Key (Self : in out Surface; Now : in Palettes.Colour; Enable : in Boolean := True) is
      --  TODO: This can work as an "in out Internal_Surface" as the compiler will pass the object as a reference.
      --        Should the entire API use this? For review!
      function SDL_Set_Color_Key (S : in Internal_Surface_Pointer;
                                  F : in C.int;
                                  K : in Interfaces.Unsigned_32) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetColorKey";

      Result : constant C.int := SDL_Set_Color_Key (S => Self.Internal,
                                                    F => (if Enable then 1 else 0),
                                                    K => Pixel_Formats.To_Pixel (Colour => Now,
                                                                                 Format => Self.Pixel_Format));
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Set_Colour_Key;

   function Alpha_Blend (Self : in Surface) return Palettes.Colour_Component is
      function SDL_Get_Surface_Alpha_Mod (S : in Internal_Surface_Pointer;
                                          A : out Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetSurfaceAlphaMod";

      Alpha  : Palettes.Colour_Component;
      Result : constant C.int := SDL_Get_Surface_Alpha_Mod (S => Self.Internal, A => Alpha);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;

      return Alpha;
   end Alpha_Blend;

   procedure Set_Alpha_Blend (Self : in out Surface; Now : in Palettes.Colour_Component) is
      function SDL_Set_Surface_Alpha_Mod (S : in Internal_Surface_Pointer;
                                          A : in Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetSurfaceAlphaMod";

      Result : constant C.int := SDL_Set_Surface_Alpha_Mod (S => Self.Internal, A => Now);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Set_Alpha_Blend;

   function Blend_Mode (Self : in Surface) return Blend_Modes is
      function SDL_Get_Surface_Blend_Mode (S : in Internal_Surface_Pointer;
                                           B : out Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetSurfaceAlphaMod";

      Blend_Mode : Blend_Modes;
      Result     : constant C.int := SDL_Get_Surface_Blend_Mode (S => Self.Internal, B => Blend_Mode);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;

      return Blend_Mode;
   end Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Surface; Now : in Blend_Modes) is
      function SDL_Set_Surface_Blend_Mode (S : in Internal_Surface_Pointer;
                                           B : in Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetSurfaceBlendMode";

      Result : constant C.int := SDL_Set_Surface_Blend_Mode (S => Self.Internal, B => Now);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Colour (Self : in Surface) return Palettes.RGB_Colour is
      function SDL_Get_Surface_Color_Mod (S : in Internal_Surface_Pointer;
                                          R : out Palettes.Colour_Component;
                                          G : out Palettes.Colour_Component;
                                          B : out Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetSurfaceColorMod";

      Red    : Palettes.Colour_Component;
      Green  : Palettes.Colour_Component;
      Blue   : Palettes.Colour_Component;
      Result : constant C.int := SDL_Get_Surface_Color_Mod (S => Self.Internal, R => Red, G => Green, B => Blue);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;

      return (Red, Green, Blue);
   end Colour;

   procedure Set_Colour (Self : in out Surface; Now : in Palettes.RGB_Colour) is
      function SDL_Set_Surface_Color_Mod (S : in Internal_Surface_Pointer;
                                          R : in Palettes.Colour_Component;
                                          G : in Palettes.Colour_Component;
                                          B : in Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetSurfaceColorMod";

      Result : constant C.int := SDL_Set_Surface_Color_Mod (S => Self.Internal,
                                                            R => Now.Red,
                                                            G => Now.Green,
                                                            B => Now.Blue);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Set_Colour;

   procedure Lock (Self : in out Surface) is
      function SDL_Lock_Surface (Self : in Internal_Surface_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LockSurface";

      Result : constant C.int := SDL_Lock_Surface (Self.Internal);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Lock;

   procedure Unlock (Self : in out Surface) is
      procedure SDL_Unlock_Surface (Self : in Internal_Surface_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UnlockSurface";
   begin
      SDL_Unlock_Surface (Self.Internal);
   end Unlock;

   procedure Set_RLE (Self : in out Surface; Enabled : in Boolean) is
      function SDL_Set_Surface_RLE (Self : in Internal_Surface_Pointer; Enabled : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetSurfaceRLE";

      Result : constant C.int := SDL_Set_Surface_RLE (Self.Internal, C.int (if Enabled then 1 else 0));
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Set_RLE;

   --  This is equivalent to the macro SDL_MUSTLOCK in SDL_surface.h.
   function Must_Lock (Self : in Surface) return Boolean is
   begin
      return (Self.Internal.Flags and RLE_Encoded) = RLE_Encoded;
   end Must_Lock;

   overriding
   procedure Adjust (Self : in out Surface) is
   begin
      --        if Self.Internal.Flags and Dont_Free = Dont_Free then
      --        end if;

      if Self.Internal /= null and Self.Owns then
         Self.Internal.Reference_Count := Self.Internal.Reference_Count + 1;
      end if;
   end Adjust;

   overriding
   procedure Finalize (Self : in out Surface) is
      procedure SDL_Free_Surface (S : in Internal_Surface_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_FreeSurface";
   begin
      if Self.Internal /= null and then Self.Owns then
         SDL_Free_Surface (Self.Internal);
      end if;
   end Finalize;
end SDL.Video.Surfaces;
