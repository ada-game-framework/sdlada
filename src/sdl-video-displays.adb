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
package body SDL.Video.Displays is
   use type C.int;

   function Closest_Mode (Display : in Natural; Wanted : in Mode; Target : out Mode) return Boolean is
      function SDL_Get_Closest_Display_Mode (D : C.int; W : in Mode; T : out Mode) return Access_Mode with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetClosestDisplayMode";

      Result : Access_Mode := SDL_Get_Closest_Display_Mode (C.int (Display), Wanted, Target);
   begin
      return (Result = null);
   end Closest_Mode;

   function Current_Mode (Display : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Current_Display_Mode (D : C.int; M : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCurrentDisplayMode";

      Result : C.int := SDL_Get_Current_Display_Mode (C.int (Display), Target);
   begin
      return (Result = Success);
   end Current_Mode;

   function Desktop_Mode (Display : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Desktop_Display_Mode (D : C.int; M : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDesktopDisplayMode";

      Result : C.int := SDL_Get_Desktop_Display_Mode (C.int (Display), Target);
   begin
      return (Result = Success);
   end Desktop_Mode;

   function Display_Mode (Display : in Natural; Index : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Display_Mode (D : in C.int; I : in C.int; T : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDisplayMode";

      Result : C.int := SDL_Get_Display_Mode (C.int (Display), C.int (Index), Target);
   begin
      return (Result = Success);
   end Display_Mode;

   function Total_Display_Modes (Display : in Natural; Total : out Positive) return Boolean is
      function SDL_Get_Num_Display_Modes (I : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumDisplayModes";

      Result : C.int := SDL_Get_Num_Display_Modes (C.int (Display));
   begin
      if Result >= 1 then
         Total := Positive (Result);

         return True;
      end if;

      return False;
   end Total_Display_Modes;

   function Display_Bounds (Display : in Natural; Bounds : out Rectangles.Rectangle) return Boolean is
      function SDL_Get_Display_Bounds (D : in C.int; B : out Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDisplayBounds";

      Result : C.int := SDL_Get_Display_Bounds (C.int (Display), Bounds);
   begin
      return (Result = Success);
   end Display_Bounds;
end SDL.Video.Displays;
