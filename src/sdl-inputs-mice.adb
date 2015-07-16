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
with Interfaces.C;
with SDL.Error;
with System;

package body SDL.Inputs.Mice is
   package C renames Interfaces.C;

   use type C.int;
   use type C.unsigned;

   --  TODO: Re-enable this when the library links against 2.0.4!
   --     function Capture (Enabled : in Boolean) return Supported is
   --        function SDL_Capture_Mouse (Enabled : in C.unsigned) return C.int with
   --          Import => True,
   --          Convention => C,
   --          External_Name => "SDL_CaptureMouse";
   --     begin
   --        if SDL_Capture_Mouse (if Enabled = True then 1 else 0) /= Success then
   --           return No;
   --        end if;
   --
   --        return Yes;
   --     end Capture;

   --  TODO: Re-enable this when the library links against 2.0.4!
   --     function Get_Global_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
   --       SDL.Events.Mice.Button_Masks is
   --
   --        function SDL_Get_Global_Mouse_State (X, Y : out C.int) return C.unsigned with
   --          Import => True,
   --          Convention => C,
   --          External_Name => "SDL_GetGlobalMouseState";
   --
   --        X, Y  : C.int;
   --        Masks : C.unsigned := SDL_Get_Global_Mouse_State (X, Y);
   --
   --        use SDL.Events.Mice;
   --     begin
   --        X_Relative := Movement_Values (X);
   --        Y_Relative := Movement_Values (Y);
   --
   --        return Button_Masks (Masks);
   --     end Get_Global_State;

   function Get_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks is

      function SDL_Get_Mouse_State (X, Y : out C.int) return C.unsigned with
        Import => True,
        Convention => C,
        External_Name => "SDL_GetMouseState";

      X, Y  : C.int;
      Masks : C.unsigned := SDL_Get_Mouse_State (X, Y);

      use SDL.Events.Mice;
   begin
      X_Relative := Movement_Values (X);
      Y_Relative := Movement_Values (Y);

      return Button_Masks (Masks);
   end Get_State;

   function In_Relative_Mode return Boolean is
      function SDL_Get_Relative_Mouse_Mode return SDL_Bool with
        Import => True,
        Convention => C,
        External_Name => "SDL_GetRelativeMouseMode";
   begin
      if SDL_Get_Relative_Mouse_Mode = SDL_True then
         return True;
      end if;

      return False;
   end In_Relative_Mode;

   function Get_Relative_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks is

      function SDL_Get_Relative_Mouse_State (X, Y : out C.int) return C.unsigned with
        Import => True,
        Convention => C,
        External_Name => "SDL_GetRelativeMouseState";

      X, Y  : C.int;
      Masks : C.unsigned := SDL_Get_Relative_Mouse_State (X, Y);

      use SDL.Events.Mice;
   begin
      X_Relative := Movement_Values (X);
      Y_Relative := Movement_Values (Y);

      return Button_Masks (Masks);
   end Get_Relative_State;

   procedure Set_Relative_Mode (Enable : in Boolean) is
      function SDL_Set_Relative_Mouse_Mode (Enable : in C.unsigned) return C.int with
        Import => True,
        Convention => C,
        External_Name => "SDL_SetRelativeMouseMode";
   begin
      if SDL_Set_Relative_Mouse_Mode (if Enable = True then 1 else 0) /= Success then
         raise Mice_Error with SDL.Error.Get;
      end if;
   end Set_Relative_Mode;

   --  TODO: Re-enable this when the library links against 2.0.4!
   --     procedure Warp (X, Y : in SDL.Events.Mice.Screen_Coordinates) is
   --        procedure SDL_Warp_Mouse_Global (X, Y : in C.int) with
   --          Import => True,
   --          Convention => C,
   --          External_Name => "SDL_WarpMouseGlobal";
   --     begin
   --        SDL_Warp_Mouse_Global (C.int (X), C.int (Y));
   --     end Warp;
   --
   --     procedure Warp (Window : in SDL.Video.Windows.Window; X, Y : in SDL.Events.Mice.Window_Coordinates) is
   --        function Get_Address (Self : in SDL.Video.Windows.Window) return System.Address with
   --          Import     => True,
   --          Convention => Ada;
   --
   --        procedure SDL_Warp_Mouse_In_Window (Window : in System.Address; X, Y : in C.int) with
   --          Import => True,
   --          Convention => C,
   --          External_Name => "SDL_WarpMouseInWindow";
   --     begin
   --        SDL_Warp_Mouse_In_Window (Get_Address (Window), C.int (X), C.int (Y));
   --     end Warp;
end SDL.Inputs.Mice;
