-------------------------------------------------------------------------------
--  Copyright (C) 2018 Pierre-Marie de Rodat <pmderodat@kawie.fr>
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
-------------------------------------------------------------------------------
--  SDL.Timers
--
--  SDL time management routines and data types.

with Interfaces;

package SDL.Timers is
   pragma Preelaborate;

   type Milliseconds is new Interfaces.Unsigned_32;

   --  Return the number of milliseconds since the SDL library initialization.
   function Ticks return Milliseconds with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetTicks";

   --  Wait a specified number of milliseconds before returning.
   procedure Wait_Delay (MS : Milliseconds) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Delay";

end SDL.Timers;
