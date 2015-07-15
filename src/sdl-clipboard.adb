--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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
with Interfaces.C.Strings;
with SDL;
with SDL.Error;
with SDL.Video.Windows;

package body SDL.Clipboard is
   package C renames Interfaces.C;

   use type C.int;

   procedure Check_For_Window is
      Init_Value : constant SDL.Init_Flags := SDL.Was_Initialised and SDL.Enable_Screen;
   begin
      if Init_Value /= SDL.Enable_Screen then
         raise Clipboard_Error with "SDL screen subsystem has not been initialised.";
      end if;

      if SDL.Video.Windows.Exist = False then
         raise Clipboard_Error with "No windows have been created.";
      end if;
   end Check_For_Window;

   function Get return Ada.Strings.UTF_Encoding.UTF_8_String is
      function SDL_Get_Clipboard_Text return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetClipboardText";
   begin
      Check_For_Window;

      return C.Strings.Value (SDL_Get_Clipboard_Text);
   end Get;

   function Is_Empty return Boolean is
      function SDL_Has_Clipboard_Text return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasClipboardText";
   begin
      Check_For_Window;

      return (if SDL_Has_Clipboard_Text = SDL_True then False else True);
   end Is_Empty;

   procedure Set (Text : in Ada.Strings.UTF_Encoding.UTF_8_String) is
      function SDL_Set_Clipboard_Text (C_Str : in C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetClipboardText";
   begin
      Check_For_Window;

      if SDL_Set_Clipboard_Text (C.To_C (Text)) /= Success then
         raise Clipboard_Error with SDL.Error.Get;
      end if;
   end Set;
end SDL.Clipboard;
