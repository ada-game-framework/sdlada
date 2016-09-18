with Ada.Strings.UTF_Encoding;
use Ada;

private with System;
with Interfaces.C;

package SDL.RWops is

   RWops_Error : exception;

   subtype Uint8 is Interfaces.Unsigned_8;
   subtype Uint16 is Interfaces.Unsigned_16;
   subtype Uint32 is Interfaces.Unsigned_32;
   subtype Uint64 is Interfaces.Unsigned_64;

   type RWops is limited private;

   type File_Mode is (Read,
                      Create_To_Write,
                      Append,
                      Read_Write,
                      Create_To_Read_Write,
                      Append_And_Read,
                      Read_Binary,
                      Create_To_Write_Binary,
                      Append_Binary,
                      Read_Write_Binary,
                      Create_To_Read_Write_Binary,
                      Append_And_Read_Binary);

   type Whence_Type is private;

   Rw_Seek_Set : constant Whence_Type;
   -- Seek from the beginning of data.

   Rw_Seek_Cur : constant Whence_Type;
   -- Seek relative to current read point.

   Rw_Seek_End : constant Whence_Type;
   -- Seek relative to the end of data.

   type Offsets is new Interfaces.C.long;

   function Get_Base_Path return Strings.UTF_Encoding.UTF_String;

   function Get_Pref_Path
     (Organization : Strings.UTF_Encoding.UTF_String;
      Application  : Strings.UTF_Encoding.UTF_String) return Strings.UTF_Encoding.UTF_String;

   function RW_Seek
     (Context : RWops;
      Offset  : Offsets;
      Whence  : Whence_Type) return Offsets;

   function RW_Size (Context : RWops) return Offsets;
   function RW_Tell (Context : RWops) return Offsets;

   function RW_From_File
     (File_Name : Strings.UTF_Encoding.UTF_String;
      Mode      : File_Mode) return RWops;

   procedure RW_From_File
     (File_Name : Strings.UTF_Encoding.UTF_String;
      Mode      : File_Mode;
      Ops       : out RWops);

   procedure RW_Close (Ops : RWops);

   function ReadU8 (src : RWops) return Uint8 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadU8";

   function ReadLE16 (Src : RWops) return Uint16 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadLE16";

   function ReadBE16 (Src : RWops) return Uint16 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadBE16";

   function ReadLE32 (Src : RWops) return Uint32 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadLE32";

   function ReadBE32 (Src : RWops) return Uint32 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadBE32";

   function ReadLE64 (Src : RWops) return Uint64 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadLE64";

   function ReadBE64 (Src : RWops) return Uint64 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadBE64";

   procedure WriteU8 (Destination : RWops; Value : Uint8);

   procedure WriteLE16 (Destination : RWops; Value : Uint16);

   procedure WriteBE16 (Destination : RWops; Value : Uint16);

   procedure WriteLE32 (Destination : RWops; Value : Uint32);

   procedure WriteBE32 (Destination : RWops; Value : Uint32);

   procedure WriteLE64 (Destination : RWops; Value : Uint64);

   procedure WriteBE64 (Destination : RWops; Value : Uint64);

private

   type Whence_Type is new Interfaces.C.int;
   Rw_Seek_Set : constant Whence_Type := 0;
   Rw_Seek_Cur : constant Whence_Type := 1;
   Rw_Seek_End : constant Whence_Type := 2;

   type Dummy_Type is
      record
         A1 : System.Address;
         A2 : System.Address;
         A3 : System.Address;
      end record;
   -- The SDL_RWops struct contains a uninon which is only used internally
   -- by SDL. The biggest variant of this union cosists of three pointers.
   -- An object of this dummy-type is just used as a placeholder and is never
   -- accessed by this binding.

   type RWops_Pointer;

   type SDL_RWops is record
      Size   : access function (context : RWops_Pointer) return Interfaces.C.long;
      Seek   : access function
        (context : RWops_Pointer;
         offset  : Interfaces.C.long;
         whence  : Interfaces.C.int) return Interfaces.C.long;
      Read   : access function
        (context : RWops_Pointer;
         ptr     : System.Address;
         size    : Interfaces.C.unsigned_long;
         maxnum  : Interfaces.C.unsigned_long) return Interfaces.C.unsigned_long;
      Write  : access function
        (context : RWops_Pointer;
         ptr     : System.Address;
         size    : Interfaces.C.unsigned_long;
         num     : Interfaces.C.unsigned_long) return Interfaces.C.unsigned_long;
      Close  : access function (Arg1 : RWops_Pointer) return Interfaces.C.int;
      C_Type : aliased Integer;
      Hidden : aliased Dummy_Type;
   end record with
     Convention => C_Pass_By_Copy;

   type RWops_Pointer is access all SDL_RWops with
     Convention => C;

   type RWops is new RWops_Pointer;

end SDL.RWops;
