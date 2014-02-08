--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
--  SDL.Video.GL
--
--  Extended OpenGL functionality.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with System;
with SDL.Video.Windows;
with SDL.Video.Textures;

package SDL.Video.GL is
   SDL_GL_Error : exception;

   type Colour_Bit_Size is range 0 .. 8 with
     Size => 8;

   --  TODO: Do I need 15 bits here?
   --  This is the size of the various Colour_Bit_Sizes for red, blue, green and alpha.
   type Buffer_Sizes is range 8 .. 32 with
     Static_Predicate => Buffer_Sizes in 8 | 16 | 24 | 32;

   type Depth_Buffer_Sizes is range 16 .. 32 with
     Static_Predicate => Depth_Buffer_Sizes in 16 | 24 | 32;

   type Stencil_Buffer_Sizes is range 0 .. 32 with
     Static_Predicate => Stencil_Buffer_Sizes in 0 | 8 | 16 | 24 | 32;

   type Multisample_Samples is range 0 .. 16;

   --  The curent values allowed for a GL version, whether lower versions will work
   --  with SDL is anothing matter.
   type Major_Versions is range 1 .. 4;

   type Minor_Versions is range 0 .. 4;

   type Profiles is (Core, Compatibility, ES) with
     Convention => C;

   for Profiles use (Core          => 16#0000_0001#,
                     Compatibility => 16#0000_0002#,
                     ES            => 16#0000_0004#);

   type Flags is mod 2 ** 8 with
     Convention => C,
     Size       => 8;

   Context_Debug              : constant Flags := 16#0000_0001#;
   Context_Forward_Compatible : constant Flags := 16#0000_0002#;
   Context_Robust_Access      : constant Flags := 16#0000_0004#;
   Context_Reset_Isolation    : constant Flags := 16#0000_0008#;

   function Red_Size return Colour_Bit_Size;
   procedure Set_Red_Size (Size : in Colour_Bit_Size);

   function Green_Size return Colour_Bit_Size;
   procedure Set_Green_Size (Size : in Colour_Bit_Size);

   function Blue_Size return Colour_Bit_Size;
   procedure Set_Blue_Size (Size : in Colour_Bit_Size);

   function Alpha_Size return Colour_Bit_Size;
   procedure Set_Alpha_Size (Size : in Colour_Bit_Size);

   function Buffer_Size return Buffer_Sizes;
   procedure Set_Buffer_Size (Size : in Buffer_Sizes);

   function Is_Double_Buffered return Boolean;
   procedure Set_Double_Buffer (On : in Boolean);

   function Depth_Buffer_Size return Depth_Buffer_Sizes;
   procedure Set_Depth_Buffer_Size (Size : in Depth_Buffer_Sizes);

   function Stencil_Buffer_Size return Stencil_Buffer_Sizes;
   procedure Set_Stencil_Buffer_Size (Size : in Stencil_Buffer_Sizes);

   function Accumulator_Red_Size return Colour_Bit_Size;
   procedure Set_Accumulator_Red_Size (Size : in Colour_Bit_Size);

   function Accumulator_Green_Size return Colour_Bit_Size;
   procedure Set_Accumulator_Green_Size (Size : in Colour_Bit_Size);

   function Accumulator_Blue_Size return Colour_Bit_Size;
   procedure Set_Accumulator_Blue_Size (Size : in Colour_Bit_Size);

   function Accumulator_Alpha_Size return Colour_Bit_Size;
   procedure Set_Accumulator_Alpha_Size (Size : in Colour_Bit_Size);

   function Is_Stereo return Boolean;
   procedure Set_Stereo (On : in Boolean);

   function Is_Multisampled return Boolean;
   procedure Set_Multisampling (On : in Boolean);

   function Multisampling_Samples return Multisample_Samples;
   procedure Set_Multisampling_Samples (Samples : in Multisample_Samples);

   function Is_Accelerated return Boolean;
   procedure Set_Accelerated (On : in Boolean);

   function Context_Major_Version return Major_Versions;
   procedure Set_Context_Major_Version (Version : Major_Versions);

   function Context_Minor_Version return Minor_Versions;
   procedure Set_Context_Minor_Version (Version : Minor_Versions);

   function Is_Context_EGL return Boolean;
   procedure Set_Context_EGL (On : in Boolean);

   function Context_Flags return Flags;
   procedure Set_Context_Flags (Context_Flags : in Flags);

   function Context_Profile return Profiles;
   procedure Set_Context_Profile (Profile : in Profiles);

   function Is_Sharing_With_Current_Context return Boolean;
   procedure Set_Share_With_Current_Context (On : in Boolean);

   --  The GL context.
   type Contexts is new Ada.Finalization.Limited_Controlled with private;

   procedure Create (Self : in out Contexts; From : in SDL.Video.Windows.Window);
   procedure Finalize (Self : in out Contexts);

   function Get_Current return Contexts;
   procedure Set_Current (Self : in Contexts; To : in SDL.Video.Windows.Window);

   --  TODO: Finish this.
--  SDL_GL_BindTexture
--  SDL_GL_UnbindTexture

   function Supports (Extension : in String) return Boolean;

--  SDL_GL_GetProcAddress

   type Swap_Intervals is (Not_Supported, Not_Synchronised, Synchronised) with
     Convention => C;

   for Swap_Intervals use (Not_Supported => -1, Not_Synchronised => 0, Synchronised => 1);

   subtype Allowed_Swap_Intervals is Swap_Intervals range Not_Synchronised .. Synchronised;

   function Get_Swap_Interval return Swap_Intervals;

   --  Returns False if setting this is not supported.
   function Set_Swap_Interval (Interval : in Allowed_Swap_Intervals; Late_Swap_Tear : in Boolean) return Boolean;

   procedure Swap (Window : in out SDL.Video.Windows.Window) with
     Inline => True;

   --  TODO: Why do we need these?
--  SDL_GL_LoadLibrary
--  SDL_GL_UnloadLibrary
private
   type Contexts is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address;
         Own      : Boolean;  --  TODO: Move to a base type?
      end record;
end SDL.Video.GL;
