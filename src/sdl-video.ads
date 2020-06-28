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
--  SDL.Video
--
--  Common display and video driver functionality.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Video is
   pragma Preelaborate;

   package C renames Interfaces.C;

   Video_Error : exception;

   --  subtype Coordinate is C.int;

   type Blend_Modes is (None, Alpha_Blend, Additive, Colour_Modulate) with
     Convention => C;

   for Blend_Modes use
     (None            => 16#0000_0000#,
      Alpha_Blend     => 16#0000_0001#,
      Additive        => 16#0000_0002#,
      Colour_Modulate => 16#0000_0004#);

   --  Screen saver information.
   procedure Enable_Screen_Saver with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_EnableScreenSaver";

   procedure Disable_Screen_Saver with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_DisableScreenSaver";

   function Is_Screen_Saver_Enabled return Boolean with
     Inline => True;

   --  Video drivers.
   function Initialise (Name : in String) return Boolean;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_VideoQuit";

   function Total_Drivers return Positive;

   function Driver_Name (Index : in Positive) return String;

   function Current_Driver_Name return String;
end SDL.Video;
