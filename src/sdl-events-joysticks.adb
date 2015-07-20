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
package body SDL.Events.Joysticks is
   type Event_States is (Query,
                         Ignore,
                         Enable) with
     Convention => C;

   for Event_States use (Query  => -1,
                         Ignore => 0,
                         Enable => 1);

   function SDL_Joystick_Event_State (State : in Event_States) return SDL_Bool with
     Import => True,
     Convention => C,
     External_Name => "SDL_JoystickEventState";

   procedure SDL_Joystick_Event_State (State : in Event_States) with
     Import => True,
     Convention => C,
     External_Name => "SDL_JoystickEventState";

   function Is_Polling_Enabled return Boolean is
      Result : SDL_Bool := SDL_Joystick_Event_State (Query);
   begin
      if Result = SDL_True then
         return True;
      end if;

      return False;
   end Is_Polling_Enabled;

   procedure Enable_Polling is
   begin
      SDL_Joystick_Event_State (Enable);
   end Enable_Polling;

   procedure Disable_Polling is
   begin
      SDL_Joystick_Event_State (Enable);
   end Disable_Polling;
end SDL.Events.Joysticks;
