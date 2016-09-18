with Interfaces.C.Strings;
with SDL.Error;

package body SDL.RWops is
   use type Interfaces.C.int;
   use type Interfaces.C.unsigned_long;

   procedure SDL_Free (Mem : Interfaces.C.Strings.chars_ptr) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_free";

   function Get_Base_Path return Strings.UTF_Encoding.UTF_String is
      use type Interfaces.C.Strings.chars_ptr;

      function SDL_GetBasePath return Interfaces.C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetBasePath";

      C_Path : constant Interfaces.C.Strings.chars_ptr :=
                 SDL_GetBasePath;

   begin

      if C_Path = Interfaces.C.Strings.Null_Ptr then
         raise RWops_Error with SDL.Error.Get;
      end if;

      declare
         Ada_Path : constant Strings.UTF_Encoding.UTF_String
           := Interfaces.C.Strings.Value (C_Path);
      begin
         SDL_Free (C_Path);

         return Ada_Path;
      end;

   end Get_Base_Path;

   function Get_Pref_Path
     (Organization : Strings.UTF_Encoding.UTF_String;
      Application  : Strings.UTF_Encoding.UTF_String) return Strings.UTF_Encoding.UTF_String
   is
      use type Interfaces.C.Strings.chars_ptr;

      function SDL_GetPrefPath
        (org : Interfaces.C.Strings.chars_ptr;
         app : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPrefPath";

      C_Organization : Interfaces.C.Strings.chars_ptr;
      C_Application  : Interfaces.C.Strings.chars_ptr;
      C_Path         : Interfaces.C.Strings.chars_ptr;
   begin
      C_Organization := Interfaces.C.Strings.New_String (Organization);
      C_Application := Interfaces.C.Strings.New_String (Application);

      C_Path :=
        SDL_GetPrefPath
          (org => C_Organization,
           app => C_Application);

      Interfaces.C.Strings.Free (C_Organization);
      Interfaces.C.Strings.Free (C_Application);

      if C_Path = Interfaces.C.Strings.Null_Ptr then
         raise RWops_Error with SDL.Error.Get;
      end if;

      declare
         Ada_Path : constant Strings.UTF_Encoding.UTF_String
           := Interfaces.C.Strings.Value (C_Path);
      begin
         SDL_Free (C_Path);

         return Ada_Path;
      end;

   end Get_Pref_Path;

   procedure RW_Close (Ops : RWops) is
      Result : Interfaces.C.int := -1;
   begin
      Result := Ops.Close (RWops_Pointer (Ops));

      if Result /= 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end RW_Close;

   function RW_From_File
     (File_Name : Strings.UTF_Encoding.UTF_String;
      Mode      : File_Mode) return RWops
   is

      function SDL_RWFromFile
        (File : Interfaces.C.Strings.chars_ptr;
         Mode : Interfaces.C.Strings.chars_ptr) return RWops_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RWFromFile";

      Mode_String : String (1 .. 3) := "   ";
      RWop        : RWops_Pointer;

      C_File_Name : Interfaces.C.Strings.chars_ptr :=
                      Interfaces.C.Strings.Null_Ptr;

      C_File_Mode : Interfaces.C.Strings.chars_ptr :=
                      Interfaces.C.Strings.Null_Ptr;

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

      C_File_Name := Interfaces.C.Strings.New_String (File_Name);
      C_File_Mode := Interfaces.C.Strings.New_String (Mode_String);
      RWop := SDL_RWFromFile (File => C_File_Name, Mode => C_File_Mode);
      Interfaces.C.Strings.Free (C_File_Name);
      Interfaces.C.Strings.Free (C_File_Mode);

      if RWop = null then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return RWops (RWop);
   end RW_From_File;

   procedure RW_From_File
     (File_Name : Strings.UTF_Encoding.UTF_String;
      Mode      : File_Mode;
      Ops       : out RWops) is
   begin
      Ops := RW_From_File (File_Name, Mode);
   end RW_From_File;

   function RW_Seek
     (Context : RWops;
      Offset  : Offsets;
      Whence  : Whence_Type) return Offsets
   is
      use type Interfaces.C.long;
      Returned_Offset : Interfaces.C.long := -1;
   begin

      Returned_Offset :=
        Context.Seek
          (context => RWops_Pointer (Context),
           offset  => Interfaces.C.long (Offset),
           whence  => Interfaces.C.int (Whence));

      if Returned_Offset = -1 then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return Offsets (Returned_Offset);

   end RW_Seek;

   function RW_Size (Context : RWops) return Offsets is
      use type Interfaces.C.long;
      Returned_Offset : Interfaces.C.long := -1;
   begin

      Returned_Offset :=
        Context.Size (context => RWops_Pointer (Context));

      if Returned_Offset < 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return Offsets (Returned_Offset);

   end RW_Size;

   function RW_Tell (Context : RWops) return Offsets is
      use type Interfaces.C.long;
      Returned_Offset : Interfaces.C.long := -1;
   begin

      Returned_Offset :=
        Context.Seek
          (context => RWops_Pointer (Context),
           offset  => 0,
           whence  => Interfaces.C.int (Rw_Seek_Cur));
      -- In C SDL_RWtell is a macro doing just this.

      if Returned_Offset = -1 then
         raise RWops_Error with SDL.Error.Get;
      end if;

      return Offsets (Returned_Offset);

   end RW_Tell;

   procedure WriteBE16 (Destination : RWops; Value : Uint16)  is
      function WriteBE16 (Dst : RWops; Value : Uint16) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteBE16";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteBE16 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteBE16;

   procedure WriteBE32 (Destination : RWops; Value : Uint32)  is
      function WriteBE32 (Dst : RWops; Value : Uint32) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteBE32";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteBE32 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteBE32;

   procedure WriteBE64 (Destination : RWops; Value : Uint64)  is
      function WriteBE64 (Dst : RWops; Value : Uint64) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteBE64";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteBE64 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteBE64;

   procedure WriteLE16 (Destination : RWops; Value : Uint16)  is
      function WriteLE16 (Dst : RWops; Value : Uint16) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteLE16";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteLE16 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteLE16;

   procedure WriteLE32 (Destination : RWops; Value : Uint32)  is
      function WriteLE32 (Dst : RWops; Value : Uint32) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteLE32";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteLE32 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteLE32;

   procedure WriteLE64 (Destination : RWops; Value : Uint64)  is
      function WriteLE64 (Dst : RWops; Value : Uint64) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteLE64";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteLE64 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteLE64;

   procedure WriteU8 (Destination : RWops; Value : Uint8)  is
      function WriteU8 (Dst : RWops; Value : Uint8) return Interfaces.C.unsigned_long with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_WriteU8";

      Result : Interfaces.C.unsigned_long := 0;
   begin
      Result := WriteU8 (Destination, Value);

      if Result = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end WriteU8;

end SDL.RWops;
