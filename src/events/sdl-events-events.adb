--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with SDL.Error;

package body SDL.Events.Events is

   function Poll (Event : out Events) return Boolean is
      function SDL_Poll_Event (Event : out Events) return Interfaces.C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_PollEvent";
   begin
      return SDL_Poll_Event (Event) = 1;
   end Poll;

   procedure Wait (Event : out Events) is
      function SDL_Wait_Event (Event : out Events) return Interfaces.C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WaitEvent";
   begin
      if SDL_Wait_Event (Event) = 0 then
         raise Event_Error with SDL.Error.Get;
      end if;
   end Wait;
end SDL.Events.Events;
