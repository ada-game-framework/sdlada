--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with SDL.Error;
with System;

package body SDL.Libraries is
   package C renames Interfaces.C;

   procedure Load (Self : out Handles; Name : in String) is
      function SDL_Load_Object (C_Str : in C.Strings.chars_ptr) return Internal_Handle_Access with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LoadObject";

      C_Str : C.Strings.chars_ptr := C.Strings.New_String (Name);
   begin
      Self.Internal := SDL_Load_Object (C_Str);

      C.Strings.Free (C_Str);

      if Self.Internal = null then
         raise Library_Error with SDL.Error.Get;
      end if;
   end Load;

   procedure Unload (Self : in out Handles) is
      procedure SDL_Unload_Object (H : in Internal_Handle_Access) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UnloadObject";
   begin
      SDL_Unload_Object (Self.Internal);

      Self.Internal := null;
   end Unload;

   function Load_Sub_Program (From_Library : in Handles) return Access_To_Sub_Program is
      --  TODO: How can I get rid of this Address use?
      function To_Sub_Program is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Access_To_Sub_Program);

      function SDL_Load_Function (H : in Internal_Handle_Access; N : in C.Strings.chars_ptr)
                                  return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_LoadFunction";

      C_Str    : C.Strings.chars_ptr     := C.Strings.New_String (Name);
      Func_Ptr : constant System.Address := SDL_Load_Function (From_Library.Internal, C_Str);

      use type System.Address;
   begin
      C.Strings.Free (C_Str);

      if Func_Ptr = System.Null_Address then
         raise Library_Error with SDL.Error.Get;
      end if;

      --        return To_Sub_Program (Func_Ptr);
      return To_Sub_Program (Func_Ptr);
   end Load_Sub_Program;

   overriding
   procedure Finalize (Self : in out Handles) is
   begin
      --  In case the user has already called Unload or Finalize on a derived type.
      if Self.Internal /= null then
         Unload (Self);
      end if;
   end Finalize;
end SDL.Libraries;
