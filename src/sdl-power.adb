--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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

package body SDL.Power is
   package C renames Interfaces.C;

   use type C.int;

   procedure Info (Data : in out Battery_Info) is
      function SDL_GetPowerInfo (Seconds, Percent : out C.int) return State with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPowerInfo";

      Seconds, Percent : C.int;

   begin
      Data.Power_State := SDL_GetPowerInfo (Seconds, Percent);

      if Seconds = -1 then
         Data.Time_Valid := False;
      else
         Data.Time_Valid := True;
         Data.Time       := SDL.Power.Seconds (Seconds);
      end if;

      if Percent = -1 then
         Data.Percentage_Valid := False;
      else
         Data.Percentage_Valid := True;
         Data.Percent          := Percentage (Percent);
      end if;
   end Info;
end SDL.Power;
