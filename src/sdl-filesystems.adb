--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;
with SDL.Error;
package body SDL.Filesystems is
   package C renames Interfaces.C;

   procedure SDL_Free (Mem : in C.Strings.chars_ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_free";


   function Base_Path return UTF_Strings.UTF_String is
      function SDL_Get_Base_Path return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetBasePath";

      C_Path : constant C.Strings.chars_ptr := SDL_Get_Base_Path;

      use type C.Strings.chars_ptr;
   begin
      if C_Path = C.Strings.Null_Ptr then
         raise Filesystems_Error with SDL.Error.Get;
      end if;

      declare
         Ada_Path : constant UTF_Strings.UTF_String := C.Strings.Value (C_Path);
      begin
         SDL_Free (C_Path);

         return Ada_Path;
      end;
   end Base_Path;


   function Preferences_Path (Organisation : in UTF_Strings.UTF_String;
                              Application  : in UTF_Strings.UTF_String) return UTF_Strings.UTF_String
   is
      function SDL_Get_Pref_Path (Organisation : in C.char_array;
                                  Application  : in C.char_array) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPrefPath";

      C_Path         : C.Strings.chars_ptr;

      use type C.Strings.chars_ptr;
   begin
      C_Path := SDL_Get_Pref_Path (Organisation => C.To_C (Organisation),
                                   Application  => C.To_C (Application));

      if C_Path = C.Strings.Null_Ptr then
         raise Filesystems_Error with SDL.Error.Get;
      end if;

      declare
         Ada_Path : constant UTF_Strings.UTF_String := C.Strings.Value (C_Path);
      begin
         SDL_Free (C_Path);

         return Ada_Path;
      end;
   end Preferences_Path;
end SDL.Filesystems;
