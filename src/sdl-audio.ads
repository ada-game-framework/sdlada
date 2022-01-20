--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2021, Eduard Llamosí
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
--  SDL.Audio
--
--  Common audio mixer and driver functionality.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Audio is
   pragma Preelaborate;

   package C renames Interfaces.C;

   Audio_Error : exception;

   --  Audio drivers.
   function Initialise (Name : in String := "") return Boolean;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_AudioQuit";

   function Total_Drivers return Positive;

   function Driver_Name (Index : in Positive) return String;

   function Current_Driver_Name return String;
end SDL.Audio;
