--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2016 Luke A. Guest
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
   use type C.int;

   function Pixel_Format (Self : in Surface) return SDL.Video.Pixel_Formats.Pixel_Format_Access is
   begin
      return Self.Internal.Pixel_Format;
   end Pixel_Format;

   function Size (Self : in Surface) return SDL.Video.Sizes is
   begin
      return SDL.Video.Sizes'(Positive (Self.Internal.Width), Positive (Self.Internal.Height));
   end Size;

   function Pixels (Self : in Surface) return System.Address is
      use type C.int;
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

   function Clip_Rectangle (Self : in Surface) return SDL.Video.Rectangles.Rectangle is
   begin
      return Self.Internal.Clip_Rectangle;
   end Clip_Rectangle;

   --  This is equivalent to the macro SDL_MUSTLOCK in SDL_surface.h.
   function Must_Lock (Self : in Surface) return Boolean is
   begin
      return (Self.Internal.Flags and RLE_Encoded) = RLE_Encoded;
   end Must_Lock;

   procedure Fill (Self   : in out Surface;
                   Area   : in SDL.Video.Rectangles.Rectangle;
                   Colour : in Interfaces.Unsigned_32) is
      function SDL_Fill_Rect (S      : in Internal_Surface_Pointer;
                              Rect   : in SDL.Video.Rectangles.Rectangle;
                              Colour : in Interfaces.Unsigned_32) return C.int with
        Import => True,
        Convention => C,
        External_Name => "SDL_FillRect";
      Result : C.int := SDL_Fill_Rect (Self.Internal, Area, Colour);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Fill;

   procedure Fill (Self   : in out Surface;
                   Areas  : in SDL.Video.Rectangles.Rectangle_Arrays;
                   Colour : in Interfaces.Unsigned_32) is
      function SDL_Fill_Rects (S      : in Internal_Surface_Pointer;
                               Rects  : in SDL.Video.Rectangles.Rectangle_Arrays;
                               Count  : in C.int;
                               Colour : in Interfaces.Unsigned_32) return C.int with
        Import => True,
        Convention => C,
        External_Name => "SDL_FillRects";
      Result : C.int := SDL_Fill_Rects (Self.Internal, Areas, Areas'Length, Colour);
   begin
      if Result < SDL.Success then
         raise Surface_Error with SDL.Error.Get;
      end if;
   end Fill;

   overriding
   procedure Adjust (Self : in out Surface) is
   begin
      --        if Self.Internal.Flags and Dont_Free = Dont_Free then
      --        end if;

      if Self.Owns then
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
         Self.Internal.Reference_Count := Self.Internal.Reference_Count - 1;

         --  The last remaining copy of the surface should always have a reference count of 1 and therefore the
         --  SDL_Free_Surface procedure  will destroy it.
         pragma Assert (Check   => Self.Internal.Reference_Count /= 0,
                        Message => "[Surface.Finalize] Internal reference count should never be zero!");

         --  Only if we created surface using Makers.Create, do we ever destroy the surface.
         if Self.Internal.Reference_Count = 1 then
            SDL_Free_Surface (Self.Internal);

            Self.Internal := null;
         end if;
      end if;
   end Finalize;
end SDL.Video.Surfaces;
