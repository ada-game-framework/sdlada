--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------

package body SDL.Inputs.Mice.Cursors is
   use type SDL.C_Pointers.Cursor_Pointer;

   procedure Create_System_Cursor (Self : in out Cursor; Cursor_Name : System_Cursors) is
      function SDL_Create_System_Cursor (Cursor_Name : in System_Cursors) return SDL.C_Pointers.Cursor_Pointer with
         Import        => True,
         Convention    => C,
         External_Name => "SDL_CreateSystemCursor";
   begin
      Self.Internal := SDL_Create_System_Cursor (Cursor_Name);
      Self.Owns := True;
   end Create_System_Cursor;

   procedure Get_Cursor (Self : in out Cursor) is
      function SDL_Get_Cursor return SDL.C_Pointers.Cursor_Pointer with
         Import        => True,
         Convention    => C,
         External_Name => "SDL_GetCursor";
   begin
      Self.Internal := SDL_Get_Cursor;
      Self.Owns := False;
   end Get_Cursor;

   procedure Set_Cursor (Self : in Cursor) is
      procedure SDL_Set_Cursor (C : in SDL.C_Pointers.Cursor_Pointer) with
         Import        => True,
         Convention    => C,
         External_Name => "SDL_SetCursor";
   begin
      SDL_Set_Cursor (Self.Internal);
   end Set_Cursor;

   overriding
   procedure Finalize (Self : in out Cursor) is
      procedure SDL_Free_Cursor (C : in SDL.C_Pointers.Cursor_Pointer) with
         Import        => True,
         Convention    => C,
         External_Name => "SDL_FreeCursor";
   begin
      if Self.Internal /= null and then Self.Owns then
         SDL_Free_Cursor (Self.Internal);
      end if;
   end Finalize;
end SDL.Inputs.Mice.Cursors;

