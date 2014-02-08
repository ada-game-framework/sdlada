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
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with SDL.Error;

package body SDL.Libraries is
   package C renames Interfaces.C;

   use type System.Address;

   procedure Load (Self : out Handles; Name : in String) is
      function SDL_Load_Object (C_Str : in C.Strings.chars_Ptr) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LoadObject";

      C_Str : C.Strings.chars_ptr := C.Strings.New_String (Name);
   begin
      Self.Internal := SDL_Load_Object (C_Str);

      C.Strings.Free (C_Str);

      if Self.Internal = System.Null_Address then
         raise Library_Error with SDL.Error.Get;
      end if;
   end Load;

   procedure Unload (Self : in out Handles) is
      procedure SDL_Unload_Object (H : in System.Address) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UnloadObject";
   begin
      SDL_Unload_Object (Self.Internal);

      Self.Internal := System.Null_Address;
   end Unload;

   function Load_Sub_Program (From_Library : in Handles) return Access_To_Sub_Program is
      function To_Sub_Program is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Access_To_Sub_Program);

      function SDL_Load_Function (H : in System.Address; N : in C.Strings.chars_ptr) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LoadFunction";

      C_Str    : C.Strings.chars_ptr     := C.Strings.New_String (Name);
      Func_Ptr : constant System.Address := SDL_Load_Function (From_Library.Internal, C_Str);
   begin
      C.Strings.Free (C_Str);

      if Func_Ptr = System.Null_Address then
         raise Library_Error with SDL.Error.Get;
      end if;

      return To_Sub_Program (Func_Ptr);
   end Load_Sub_Program;

   procedure Finalize (Self : in out Handles) is
   begin
      --  In case the user has already called Unload or Finalize on a derived type.
      if Self.Internal /= System.Null_Address then
         Unload (Self);
      end if;
   end Finalize;
end SDL.Libraries;
