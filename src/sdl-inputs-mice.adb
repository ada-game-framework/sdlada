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
private with SDL.C_Pointers;
with SDL.Error;

package body SDL.Inputs.Mice is
   package C renames Interfaces.C;

   function Capture (Enabled : in Boolean) return Supported is
      function SDL_Capture_Mouse (Enabled : in C.unsigned) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CaptureMouse";
   begin
      if SDL_Capture_Mouse (if Enabled then 1 else 0) /= Success then
         return No;
      end if;

      return Yes;
   end Capture;

   function Get_Global_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks is

      function SDL_Get_Global_Mouse_State (X, Y : out C.int) return C.unsigned with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetGlobalMouseState";

      X, Y  : C.int;
      Masks : constant C.unsigned := SDL_Get_Global_Mouse_State (X, Y);

      use SDL.Events.Mice;
   begin
      X_Relative := Movement_Values (X);
      Y_Relative := Movement_Values (Y);

      return Button_Masks (Masks);
   end Get_Global_State;

   function Get_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks is

      function SDL_Get_Mouse_State (X, Y : out C.int) return C.unsigned with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetMouseState";

      X, Y  : C.int;
      Masks : constant C.unsigned := SDL_Get_Mouse_State (X, Y);

      use SDL.Events.Mice;
   begin
      X_Relative := Movement_Values (X);
      Y_Relative := Movement_Values (Y);

      return Button_Masks (Masks);
   end Get_State;

   function In_Relative_Mode return Boolean is
      function SDL_Get_Relative_Mouse_Mode return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRelativeMouseMode";
   begin
      return SDL_Get_Relative_Mouse_Mode = SDL_True;
   end In_Relative_Mode;

   function Get_Relative_State (X_Relative, Y_Relative : out SDL.Events.Mice.Movement_Values) return
     SDL.Events.Mice.Button_Masks is

      function SDL_Get_Relative_Mouse_State (X, Y : out C.int) return C.unsigned with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRelativeMouseState";

      X, Y  : C.int;
      Masks : constant C.unsigned := SDL_Get_Relative_Mouse_State (X, Y);

      use SDL.Events.Mice;
   begin
      X_Relative := Movement_Values (X);
      Y_Relative := Movement_Values (Y);

      return Button_Masks (Masks);
   end Get_Relative_State;

   procedure Set_Relative_Mode (Enable : in Boolean := True) is
      function SDL_Set_Relative_Mouse_Mode (Enable : in C.unsigned) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRelativeMouseMode";
   begin
      if SDL_Set_Relative_Mouse_Mode (if Enable then 1 else 0) /= Success then
         raise Mice_Error with SDL.Error.Get;
      end if;
   end Set_Relative_Mode;

   procedure Show_Cursor (Enable : in Boolean := True) is
      procedure SDL_Show_Cursor (Enable : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_ShowCursor";
   begin
      SDL_Show_Cursor (if Enable then SDL.SDL_Enable else SDL.SDL_Disable);
   end Show_Cursor;

   function Is_Cursor_Shown return Boolean is
      function SDL_Show_Cursor (Enable : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_ShowCursor";
   begin
      case SDL_Show_Cursor (SDL.SDL_Query) is
         when SDL.SDL_Enable =>
            return True;

         when SDL.SDL_Disable =>
            return False;

         when others =>
            raise Mice_Error with "SDL_Show_Cursor should never return any other value!";
      end case;
   end Is_Cursor_Shown;

   procedure Warp (To : in SDL.Coordinates) is
      function SDL_Warp_Mouse_Global (X, Y : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WarpMouseGlobal";
   begin
      if SDL_Warp_Mouse_Global (C.int (To.X), C.int (To.Y)) /= SDL.Success then
         raise Mice_Error with SDL.Error.Get;
      end if;
   end Warp;

   procedure Warp (Window : in SDL.Video.Windows.Window; To : in SDL.Coordinates) is
      function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
        Import     => True,
        Convention => Ada;

      procedure SDL_Warp_Mouse_In_Window (Window : in SDL.C_Pointers.Windows_Pointer; X, Y : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WarpMouseInWindow";
   begin
      SDL_Warp_Mouse_In_Window (Get_Internal_Window (Window), C.int (To.X), C.int (To.Y));
   end Warp;
end SDL.Inputs.Mice;
