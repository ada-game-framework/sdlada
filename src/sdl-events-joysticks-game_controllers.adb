--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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
with Interfaces;

package body SDL.Events.Joysticks.Game_Controllers is
   package C renames Interfaces.C;

   Query  : constant C.int := -1;
   Ignore : constant C.int :=  0;
   Enable : constant C.int :=  1;

   function SDL_Game_Controller_Event_State (State : in C.int) return C.int with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GameControllerEventState";

   procedure SDL_Game_Controller_Event_State (State : in C.int) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GameControllerEventState";

   function Is_Polling_Enabled return Boolean is
   begin
      return SDL_Game_Controller_Event_State (Query) = Enable;
   end Is_Polling_Enabled;

   procedure Enable_Polling is
   begin
      SDL_Game_Controller_Event_State (Enable);
   end Enable_Polling;

   procedure Disable_Polling is
   begin
      SDL_Game_Controller_Event_State (Ignore);
   end Disable_Polling;
end SDL.Events.Joysticks.Game_Controllers;
