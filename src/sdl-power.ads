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
--  SDL.Power
--
--  Battery access on the target platform.
--------------------------------------------------------------------------------------------------------------------
package SDL.Power is
   type State is
     (Unknown,    --  Cannot determine power status.
      Battery,    --  Not plugged in, running on the battery.
      No_Battery, --  Plugged in, no battery available.
      Charging,   --  Plugged in, charging battery.
      Charged     --  Plugged in, battery charged.
     ) with
     Convention => C;

   type Seconds is range 0 .. Integer'Last;
   type Percentage is range 0 .. 100;

   type Battery_Info is
      record
         Power_State      : State;

         Time_Valid       : Boolean;
         Time             : Seconds;

         Percentage_Valid : Boolean;
         Percent          : Percentage;
      end record;

   procedure Info (Data : in out Battery_Info);
end SDL.Power;
