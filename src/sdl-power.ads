--                              -*- Mode: Ada -*-
--  Filename        : sdl-power.ads
--  Description     : Subprograms for accessing the battery on the target platform.
--  Author          : Luke A. Guest
--  Created On      : Thu Sep 26 15:24:07 2013
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
