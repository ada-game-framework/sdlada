--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Error is
   package C renames Interfaces.C;

   procedure Set (S : in String) is
      procedure SDL_Set_Error (C_Str : in C.char_array) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetError";
   begin
      SDL_Set_Error (C.To_C (S));
   end Set;


   function Get return String is
      function SDL_Get_Error return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetError";
   begin
      return C.Strings.Value (SDL_Get_Error);
   end Get;


   procedure Get (Buffer : in out String) is
      function SDL_Get_Error_Msg (C_Str : in C.char_array; Max_Length : in C.int) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetErrorMsg";

      Buffer_Ptr : constant C.Strings.chars_ptr := SDL_Get_Error_Msg (C.To_C (Buffer), Buffer'Length);
   begin
      Buffer := C.Strings.Value (Buffer_Ptr);  --  Warning! 2 Copies here.
   end Get;
end SDL.Error;
