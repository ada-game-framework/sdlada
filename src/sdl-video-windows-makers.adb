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
with Interfaces.C;
with Interfaces.C.Strings;
private with SDL.C_Pointers;
with SDL.Error;

package body SDL.Video.Windows.Makers is
   package C renames Interfaces.C;

   use type SDL.C_Pointers.Windows_Pointer;

   procedure Create
     (Win    : in out Window;
      Title  : in Ada.Strings.UTF_Encoding.UTF_8_String;
      X      : in Integer;
      Y      : in Integer;
      Width  : in Integer;
      Height : in Integer;
      Flags  : in Window_Flags := OpenGL) is

      function SDL_Create
        (Title      : C.Strings.chars_ptr;
         X, Y, W, H : in C.int;
         F          : in Window_Flags) return SDL.C_Pointers.Windows_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateWindow";

      C_Title_Str : C.Strings.chars_ptr := C.Strings.New_String (Title);
   begin
      Win.Internal := SDL_Create (C_Title_Str, C.int (X), C.int (Y), C.int (Width), C.int (Height), Flags);

      C.Strings.Free (C_Title_Str);

      if Win.Internal = null then
         raise Window_Error with SDL.Error.Get;
      end if;

      Increment_Windows;
   end Create;

   procedure Create (Win : in out Window; Native : in Native_Window) is
      function SDL_Create_Window_From (Native : Native_Window) return SDL.C_Pointers.Windows_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateWindowFrom";
   begin
      Win.Internal := SDL_Create_Window_From (Native);
      Win.Owns     := True;

      if Win.Internal = null then
         raise Window_Error with SDL.Error.Get;
      end if;

      Increment_Windows;
   end Create;
end SDL.Video.Windows.Makers;
