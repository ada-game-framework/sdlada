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
--  SDL.Video.Surfaces
--
--  Render surface abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with Interfaces.C;
with System;
--  private with SDL.C_Pointers;
with SDL.Video;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;

package SDL.Video.Surfaces is
   pragma Preelaborate;

   package C renames Interfaces.C;

   Surface_Error : exception;

   --  Pixel depths.
   type Pixel_Depths is new Positive with
     Convention       => C,
     Static_Predicate => Pixel_Depths in 1 | 2 | 4 | 8 | 12 | 15 | 16 | 24 | 32;

   --  For defining RGBA colour masks.
   type Colour_Masks is mod 2 ** 32 with
     Convention => C;

   type Internal_Surface is private;
   type Internal_Surface_Pointer is access Internal_Surface with
     Convention => C;

   type Surface is new Ada.Finalization.Controlled with private;

   Null_Surface : constant Surface;

   --  Operations to get hold of the various fields in the internal record.
   function Pixel_Format (Self : in Surface) return Pixel_Formats.Pixel_Format_Access with
     Inline => True;

   --  Get the dimensions of this surface.
   function Size (Self : in Surface) return SDL.Sizes with
     Inline => True;

   --  TODO: Make generic so that we can get access to specific arrays which are mapped onto the Pixels address.
   function Pixels (Self : in Surface) return System.Address with
     Inline => True;

   generic
      type Data is private;
      type Data_Pointer is access all Data;
   package User_Data is
      function Get (Self : in Surface) return Data_Pointer;

      procedure Set (Self : in out Surface; Data : in Data_Pointer);
   end User_Data;

   --  Other operations.

   --  Blit Source Surface onto Self Surface.
   procedure Blit (Self        : in out Surface;
                   Source      : in Surface);

   procedure Blit (Self        : in out Surface;
                   Self_Area   : in out Rectangles.Rectangle;
                   Source      : in Surface;
                   Source_Area : in out Rectangles.Rectangle);

   --  Blit Source Surface onto Self Surface with scaling.
   procedure Blit_Scaled (Self        : in out Surface;
                          Source      : in Surface);

   --  As above, but takes in rectangles to define the areas of blitting. Updates the Self_Area with the final area
   --  used to blit into.
   procedure Blit_Scaled (Self        : in out Surface;
                          Self_Area   : in out Rectangles.Rectangle;
                          Source      : in Surface;
                          Source_Area : in Rectangles.Rectangle := Rectangles.Null_Rectangle);

   --  The next two functions require that the rectangles already be clipped, if they're not, use the above.
   procedure Lower_Blit (Self        : in out Surface;
                         Self_Area   : in Rectangles.Rectangle;
                         Source      : in Surface;
                         Source_Area : in Rectangles.Rectangle);

   procedure Lower_Blit_Scaled (Self        : in out Surface;
                                Self_Area   : in Rectangles.Rectangle;
                                Source      : in Surface;
                                Source_Area : in Rectangles.Rectangle);
   --  TODO: SDL_ConvertPixels

   procedure Fill (Self   : in out Surface;
                   Area   : in Rectangles.Rectangle;
                   Colour : in Interfaces.Unsigned_32);

   procedure Fill (Self   : in out Surface;
                   Areas  : in Rectangles.Rectangle_Arrays;
                   Colour : in Interfaces.Unsigned_32);

   function Clip_Rectangle (Self : in Surface) return Rectangles.Rectangle with
     Inline => True;

   procedure Set_Clip_Rectangle (Self : in out Surface; Now : in Rectangles.Rectangle) with
     Inline => True;

   function Colour_Key (Self : in Surface) return Palettes.Colour with
     Inline => True;

   procedure Set_Colour_Key (Self : in out Surface; Now : in Palettes.Colour; Enable : in Boolean := True) with
     Inline => True;

   function Alpha_Blend (Self : in Surface) return Palettes.Colour_Component with
     Inline => True;

   procedure Set_Alpha_Blend (Self : in out Surface; Now : in Palettes.Colour_Component) with
     Inline => True;

   function Blend_Mode (Self : in Surface) return Blend_Modes with
     Inline => True;

   procedure Set_Blend_Mode (Self : in out Surface; Now : in Blend_Modes) with
     Inline => True;

   function Colour (Self : in Surface) return Palettes.RGB_Colour with
     Inline => True;

   procedure Set_Colour (Self : in out Surface; Now : in Palettes.RGB_Colour) with
     Inline => True;

   --  TODO: SDL_LoadBMP_RW
   --  TODO: SDL_SaveBMP
   --  TODO: SDL_SaveBMP_RW

   procedure Lock (Self : in out Surface) with
     Inline => True;

   procedure Unlock (Self : in out Surface) with
     Inline => True;

   function Must_Lock (Self : in Surface) return Boolean with
     Inline => True;

   --  TODO: SDL_SetSurfacePalette

   procedure Set_RLE (Self : in out Surface; Enabled : in Boolean) with
     Inline => True;

   overriding
   procedure Initialize (Self : in out Surface) renames Adjust;

   overriding
   procedure Adjust (Self : in out Surface);

   overriding
   procedure Finalize (Self : in out Surface);
private
   type Surface_Flags is mod 2 ** 32 with
     Convention => C;

   SW_Surface   : constant Surface_Flags := Surface_Flags'First;
   Preallocated : constant Surface_Flags := 16#0000_0001#;
   RLE_Encoded  : constant Surface_Flags := 16#0000_0002#;
   Dont_Free    : constant Surface_Flags := 16#0000_0004#;

   type Test_Address is access all C.int with
     Convention => C;

   type User_Data_Pointer is access all C.int with
     Convention => C;

   --  Internal_Surfaces are allowed to be manipulated by the user, but this is protected behind the following inlines.
   type Internal_Surface is
      record
         Flags           : Surface_Flags;           --  Internal, don't touch.
         Pixel_Format    : Pixel_Formats.Pixel_Format_Access;
         Width           : SDL.Dimension;
         Height          : SDL.Dimension;
         Pitch           : C.int;
         Pixels          : System.Address;          --  Pixel data.
         User_Data       : User_Data_Pointer;
         Locked          : C.int;                   --  Internal, don't touch.
         Lock_Data       : System.Address;          --  Internal, don't touch.
         Clip_Rectangle  : Rectangles.Rectangle;
         Blit_Map        : System.Address;          --  Internal, don't touch.
         Reference_Count : C.int;
      end record with
     Convention => C;

   Null_Internal_Surface : constant Internal_Surface := (others => <>);

   --  If the Dont_Free flag is set on the Internal_Surface, then calling SDL_FreeSurface won't decrease the reference
   --  count. This is set when getting a window's surface.
   type Surface is new Ada.Finalization.Controlled with
      record
         Internal : Internal_Surface_Pointer := null;
         Owns     : Boolean                  := True;  --  Only if created by Surface.Makers.
      end record;

   Null_Surface : constant Surface := (Ada.Finalization.Controlled with
                                       Internal => null,
                                       Owns     => True);
end SDL.Video.Surfaces;
