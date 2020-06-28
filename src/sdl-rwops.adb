--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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

package body SDL.RWops is
   use type C.size_t;
   use type C.Strings.chars_ptr;

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
   begin
      if C_Path = C.Strings.Null_Ptr then
         raise RWops_Error with SDL.Error.Get;
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
      function SDL_Get_Pref_Path (Organisation : in C.Strings.chars_ptr;
                                  Application  : in C.Strings.chars_ptr) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPrefPath";

      C_Organisation : C.Strings.chars_ptr;
      C_Application  : C.Strings.chars_ptr;
      C_Path         : C.Strings.chars_ptr;
   begin
      C_Organisation := C.Strings.New_String (Organisation);
      C_Application  := C.Strings.New_String (Application);

      C_Path := SDL_Get_Pref_Path (Organisation => C_Organisation,
                                   Application  => C_Application);

      C.Strings.Free (C_Organisation);
      C.Strings.Free (C_Application);

      if C_Path = C.Strings.Null_Ptr then
         raise RWops_Error with SDL.Error.Get;
      end if;

      declare
         Ada_Path : constant UTF_Strings.UTF_String := C.Strings.Value (C_Path);
      begin
         SDL_Free (C_Path);

         return Ada_Path;
      end;
   end Preferences_Path;

   procedure Close (Ops : in RWops) is
      Result : C.int := -1;
   begin
      Result := Ops.Close (RWops_Pointer (Ops));

      if Result /= 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Close;

   function From_File (File_Name : in UTF_Strings.UTF_String;
                       Mode      : in File_Mode) return RWops
   is
      function SDL_RW_From_File (File : in C.Strings.chars_ptr;
                                 Mode : in C.Strings.chars_ptr) return RWops_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RWFromFile";

      Mode_String : String (1 .. 3)     := "   ";
      RWop        : RWops_Pointer;
      C_File_Name : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      C_File_Mode : C.Strings.chars_ptr := C.Strings.Null_Ptr;
   begin
      case Mode is
         when Read =>
            Mode_String := "r  ";

         when Create_To_Write =>
            Mode_String := "w  ";

         when Append =>
            Mode_String := "a  ";

         when Read_Write =>
            Mode_String := "r+ ";

         when Create_To_Read_Write =>
            Mode_String := "w+ ";

         when Append_And_Read =>
            Mode_String := "a+ ";

         when Read_Binary =>
            Mode_String := "rb ";

         when Create_To_Write_Binary =>
            Mode_String := "wb ";

         when Append_Binary =>
            Mode_String := "ab ";

         when Read_Write_Binary =>
            Mode_String := "r+b";

         when Create_To_Read_Write_Binary =>
            Mode_String := "w+b";

         when Append_And_Read_Binary =>
            Mode_String := "a+b";
      end case;

      C_File_Name := C.Strings.New_String (File_Name);
      C_File_Mode := C.Strings.New_String (Mode_String);

      RWop := SDL_RW_From_File (File => C_File_Name, Mode => C_File_Mode);

      C.Strings.Free (C_File_Name);
      C.Strings.Free (C_File_Mode);

      if RWop = null then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return RWops (RWop);
   end From_File;

   procedure From_File (File_Name : in UTF_Strings.UTF_String;
                        Mode      : in File_Mode;
                        Ops       : out RWops) is
   begin
      Ops := From_File (File_Name, Mode);
   end From_File;

   function Seek (Context : in RWops;
                  Offset  : in Offsets;
                  Whence  : in Whence_Type) return Offsets
   is
      Returned_Offset : SDL.RWops.Offsets := SDL.RWops.Error_Offset;
   begin
      Returned_Offset := Context.Seek (Context => RWops_Pointer (Context),
                                       Offset  => Offset,
                                       Whence  => Whence);

      if Returned_Offset = SDL.RWops.Error_Offset then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return Returned_Offset;
   end Seek;

   function Size (Context : in RWops) return Offsets is
      Returned_Offset : SDL.RWops.Offsets := SDL.RWops.Error_Offset;
   begin
      Returned_Offset := Context.Size (Context => RWops_Pointer (Context));

      if Returned_Offset < Null_Offset then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return Returned_Offset;
   end Size;

   function Tell (Context : in RWops) return Offsets is
      Returned_Offset : SDL.RWops.Offsets := SDL.RWops.Error_Offset;
   begin
      --  In C SDL_RWtell is a macro doing just this.
      Returned_Offset := Context.Seek (Context => RWops_Pointer (Context),
                                       Offset  => Null_Offset,
                                       Whence  => RW_Seek_Cur);

      if Returned_Offset = SDL.RWops.Error_Offset then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return Returned_Offset;
   end Tell;

   procedure Write_BE_16 (Destination : in RWops; Value : in Uint16)  is
      function SDL_Write_BE_16 (Destination : in RWops; Value : in Uint16) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteBE16";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_BE_16 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_BE_16;

   procedure Write_BE_32 (Destination : in RWops; Value : in Uint32)  is
      function SDL_Write_BE_32 (Destination : in RWops; Value : in Uint32) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteBE32";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_BE_32 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_BE_32;

   procedure Write_BE_64 (Destination : in RWops; Value : in Uint64)  is
      function SDL_Write_BE_64 (Destination : in RWops; Value : in Uint64) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteBE64";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_BE_64 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_BE_64;

   procedure Write_LE_16 (Destination : in RWops; Value : in Uint16)  is
      function SDL_Write_LE_16 (Destination : in RWops; Value : in Uint16) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteLE16";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_LE_16 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_LE_16;

   procedure Write_LE_32 (Destination : in RWops; Value : in Uint32)  is
      function SDL_Write_LE_32 (Destination : in RWops; Value : in Uint32) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteLE32";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_LE_32 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_LE_32;

   procedure Write_LE_64 (Destination : in RWops; Value : in Uint64)  is
      function SDL_Write_LE_64 (Destination : in RWops; Value : in Uint64) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteLE64";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_LE_64 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_LE_64;

   procedure Write_U_8 (Destination : in RWops; Value : in Uint8)  is
      function SDL_Write_U_8 (Destination : in RWops; Value : in Uint8) return C.size_t with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteU8";

      Result : C.size_t := 0;
   begin
      Result := SDL_Write_U_8 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write_U_8;

   function Is_Null (Source : in RWops) return Boolean is
   begin
      return (if Source = null then True else False);
   end Is_Null;
end SDL.RWops;
