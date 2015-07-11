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
with Interfaces.C;

package body SDL.Events.Events is
   function Poll (Event : out Events) return Boolean is
      --  int SDL_PollEvent(SDL_Event* event)

      use type Interfaces.C.int;

      function SDL_Poll_Event (Event : out Events) return Interfaces.C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_PollEvent";
   begin
      if SDL_Poll_Event (Event) = 1 then
         return True;
      end if;

      return False;
   end Poll;
end SDL.Events.Events;
