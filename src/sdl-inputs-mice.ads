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
--  SDL.Inputs.Mice
--------------------------------------------------------------------------------------------------------------------
with SDL.Events.Mice;
with SDL.Video.Windows;

package SDL.Inputs.Mice is
   Mice_Error : exception;

   type Cursor_Toggles is (Off, On);

   for Cursor_Toggles use (Off => 0, On => 1);

   type Supported is (Yes, No);

   --  TODO: Re-enable this when the library links against 2.0.4!
   --     function Capture (Enabled : in Boolean) return Supported;

   --  SDL_CreateColorCursor
   --  SDL_CreateCursor
   --  SDL_CreateSystemCursor
   --  SDL_FreeCursor
   --  SDL_GetCursor
   --  SDL_GetDefaultCursor

   --  TODO: Re-enable this when the library links against 2.0.4!
   --     function Get_Global_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
   --       SDL.Events.Mice.Button_Masks;

   --  SDL_GetMouseFocus

   function Get_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks;

   function In_Relative_Mode return Boolean;

   function Get_Relative_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks;

   --  SDL_SetCursor

   procedure Set_Relative_Mode (Enable : in Boolean);

   --  SDL_ShowCursor

   --  TODO: Re-enable this when the library links against 2.0.4!
   --  Move the mouse to (x, y) on the screen.
   --     procedure Warp (X, Y : in SDL.Events.Mice.Screen_Coordinates);
   --
   --  Move the mouse to (x, y) in the specified window.
   --  procedure Warp (Window : in SDL.Video.Windows.Window; X, Y : in SDL.Events.Mice.Window_Coordinates);
end SDL.Inputs.Mice;
