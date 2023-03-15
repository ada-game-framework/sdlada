--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2021, Eduard Llamosí
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
with SDL.Error;

package body SDL.Audio is

   function Initialise (Name : in String := "") return Boolean is
      function SDL_Audio_Init (C_Name : in C.Strings.chars_ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_AudioInit";

      C_Str  : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      Result : C.int;
   begin
      if Name /= "" then
         C_Str := C.Strings.New_String (Name);

         Result := SDL_Audio_Init (C_Name => C_Str);

         C.Strings.Free (C_Str);
      else
         Result := SDL_Audio_Init (C_Name => C.Strings.Null_Ptr);
      end if;

      return (Result = Success);
   end Initialise;

   function Total_Drivers return Positive is
      function SDL_Get_Num_Audio_Drivers return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumAudioDrivers";

      Num : constant C.int := SDL_Get_Num_Audio_Drivers;
   begin
      if Num < 0 then
         raise Audio_Error with SDL.Error.Get;
      end if;

      return Positive (Num);
   end Total_Drivers;

   function Driver_Name (Index : in Positive) return String is
      function SDL_Get_Audio_Driver (I : in C.int) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetAudioDriver";

      --  Index is zero based, so need to subtract 1 to correct it.
      C_Str : constant C.Strings.chars_ptr := SDL_Get_Audio_Driver (C.int (Index) - 1);
   begin
      return C.Strings.Value (C_Str);
   end Driver_Name;

   function Current_Driver_Name return String is
      function SDL_Get_Current_Audio_Driver return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCurrentAudioDriver";

      C_Str : constant C.Strings.chars_ptr := SDL_Get_Current_Audio_Driver;

      use type C.Strings.chars_ptr;
   begin
      if C_Str = C.Strings.Null_Ptr then
         raise Audio_Error with SDL.Error.Get;
      end if;

      return C.Strings.Value (C_Str);
   end Current_Driver_Name;
end SDL.Audio;
