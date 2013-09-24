with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Error is
   package C renames Interfaces.C;

   procedure SDL_Set_Error (C_Str : in C.char_array) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_SetError";

   function SDL_Get_Error return C.Strings.chars_ptr with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetError";

   procedure Set (S : in String) is
   begin
      SDL_Set_Error (C.To_C (S));
   end Set;

   function Get return String is
      C_Str : C.Strings.chars_ptr := SDL_Get_Error;
   begin
      return C.Strings.Value (C_Str);
   end Get;
end SDL.Error;
