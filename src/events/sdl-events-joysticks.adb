--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package body SDL.Events.Joysticks is
   package C renames Interfaces.C;

   Query  : constant C.int := -1;
   Ignore : constant C.int :=  0;
   Enable : constant C.int :=  1;

   function SDL_Joystick_Event_State (State : in C.int) return C.int with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_JoystickEventState";

   procedure SDL_Joystick_Event_State (State : in C.int) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_JoystickEventState";

   function Is_Polling_Enabled return Boolean is
   begin
      return SDL_Joystick_Event_State (Query) = Enable;
   end Is_Polling_Enabled;

   procedure Enable_Polling is
   begin
      SDL_Joystick_Event_State (Enable);
   end Enable_Polling;

   procedure Disable_Polling is
   begin
      SDL_Joystick_Event_State (Ignore);
   end Disable_Polling;
end SDL.Events.Joysticks;
