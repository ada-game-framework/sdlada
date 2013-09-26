with Interfaces.C;
with System;

package body SDL.Power is
   package C renames Interfaces.C;

   use type C.int;

   procedure Info (Data : in out Battery_Info) is
      function SDL_GetPowerInfo (Seconds, Percent : in System.Address) return State with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPowerInfo";

      Seconds, Percent : C.int;

   begin
      Data.Power_State := SDL_GetPowerInfo (Seconds'Address, Percent'Address);

      if Seconds = -1 then
         Data.Time_Valid := Error;
      else
         Data.Time_Valid := Valid;
         Data.Time       := SDL.Power.Seconds (Seconds);
      end if;

      if Percent = -1 then
         Data.Percentage_Valid := Error;
      else
         Data.Percentage_Valid := Valid;
         Data.Percent          := Percentage (Percent);
      end if;
   end Info;
end SDL.Power;
