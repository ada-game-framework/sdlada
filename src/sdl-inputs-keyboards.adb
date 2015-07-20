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
private with SDL.C_Pointers;

package body SDL.Inputs.Keyboards is
   package C renames Interfaces.C;

   function Get_Focus return SDL.Video.Windows.ID is
      function SDL_Get_Window_ID (W : in SDL.C_Pointers.Windows_Pointer) return SDL.Video.Windows.ID with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowID";

      function SDL_Get_Keyboard_Focus return SDL.C_Pointers.Windows_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetKeyboardFocus";
   begin
      return SDL_Get_Window_ID (SDL_Get_Keyboard_Focus);
   end Get_Focus;

   function Get_Modifiers return SDL.Events.Keyboards.Key_Modifiers is
      function SDL_Get_Mod_State return SDL.Events.Keyboards.Key_Modifiers with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetModState";
   begin
      return SDL_Get_Mod_State;
   end Get_Modifiers;

   procedure Set_Modifiers (Modifiers : in SDL.Events.Keyboards.Key_Modifiers) is
      procedure SDL_Set_Mod_State (Modifiers : in SDL.Events.Keyboards.Key_Modifiers) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetModState";
   begin
      SDL_Set_Mod_State (Modifiers);
   end Set_Modifiers;

   function Supports_Screen_Keyboard return Boolean is
      function SDL_Has_Screen_Keyboard_Support return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasScreenKeyboardSupport";

      Result : SDL_Bool := SDL_Has_Screen_Keyboard_Support;
   begin
      if Result = SDL_True then
         return True;
      end if;

      return False;
   end Supports_Screen_Keyboard;

   function Is_Screen_Keyboard_Visible (Window : in SDL.Video.Windows.Window) return Boolean is
      function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
        Convention => Ada,
        Import     => True;

      function SDL_Screen_Keyboard_Shown (Window : in SDL.C_Pointers.Windows_Pointer) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IsScreenKeyboardShown";

      Result : SDL_Bool := SDL_Screen_Keyboard_Shown (Get_Internal_Window (Window));
   begin
      if Result = SDL_True then
         return True;
      end if;

      return False;
   end Is_Screen_Keyboard_Visible;

   function Is_Text_Input_Enabled return Boolean is
      function SDL_Is_Text_Input_Active return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IsTextInputActive";

      Result : SDL_Bool := SDL_Is_Text_Input_Active;
   begin
      if Result = SDL_True then
         return True;
      end if;

      return False;
   end Is_Text_Input_Enabled;

   procedure Set_Text_Input_Rectangle (Rectangle : in SDL.Video.Rectangles.Rectangle) is
      procedure SDL_Set_Text_Input_Rect (Rectangle : in SDL.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextInputRect";
   begin
      SDL_Set_Text_Input_Rect (Rectangle);
   end Set_Text_Input_Rectangle;

   procedure Start_Text_Input is
      procedure SDL_Start_Text_Input with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_StartTextInput";
   begin
      SDL_Start_Text_Input;
   end Start_Text_Input;

   procedure Stop_Text_Input is
      procedure SDL_Stop_Text_Input with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_StopTextInput";
   begin
      SDL_Stop_Text_Input;
   end Stop_Text_Input;
end SDL.Inputs.Keyboards;
