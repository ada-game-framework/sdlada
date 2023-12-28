--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package body SDL.Power is
   package C renames Interfaces.C;

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
