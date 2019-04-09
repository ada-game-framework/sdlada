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
--  SDL.RWops
--
--  Read/Write operations, i.e. file related machinery.
--------------------------------------------------------------------------------------------------------------------
with Ada.Strings.UTF_Encoding;
with Interfaces.C;
private with System;

package SDL.RWops is
   pragma Preelaborate;

   package C renames Interfaces.C;
   package UTF_Strings renames Ada.Strings.UTF_Encoding;

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

   RW_Seek_Set : constant Whence_Type;  --  Seek from the beginning of data.
   RW_Seek_Cur : constant Whence_Type;  --  Seek relative to current read point.
   RW_Seek_End : constant Whence_Type;  --  Seek relative to the end of data.

   type Offsets is new Interfaces.Integer_64;

   Null_Offset  : constant Offsets :=  0;
   Error_Offset : constant Offsets := -1;

   subtype Sizes is Offsets;

   Error_Or_EOF : constant Sizes := 0;

   function Base_Path return UTF_Strings.UTF_String;

   function Preferences_Path (Organisation : in UTF_Strings.UTF_String;
                              Application  : in UTF_Strings.UTF_String) return UTF_Strings.UTF_String;

   function Seek (Context : in RWops;
                  Offset  : in Offsets;
                  Whence  : in Whence_Type) return Offsets;

   function Size (Context : in RWops) return Offsets;
   function Tell (Context : in RWops) return Offsets;

   function From_File (File_Name : in UTF_Strings.UTF_String;
                       Mode      : in File_Mode) return RWops;

   procedure From_File (File_Name : in UTF_Strings.UTF_String;
                        Mode      : in File_Mode;
                        Ops       : out RWops);

   procedure Close (Ops : in RWops);

   function Read_U_8 (src : in RWops) return Uint8 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadU8";

   function Read_LE_16 (Src : in RWops) return Uint16 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadLE16";

   function Read_BE_16 (Src : in RWops) return Uint16 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadBE16";

   function Read_LE_32 (Src : in RWops) return Uint32 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadLE32";

   function Read_BE_32 (Src : in RWops) return Uint32 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadBE32";

   function Read_LE_64 (Src : in RWops) return Uint64 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadLE64";

   function Read_BE_64 (Src : in RWops) return Uint64 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ReadBE64";

   procedure Write_U_8 (Destination : in RWops; Value : in Uint8);
   procedure Write_LE_16 (Destination : in RWops; Value : in Uint16);
   procedure Write_BE_16 (Destination : in RWops; Value : in Uint16);
   procedure Write_LE_32 (Destination : in RWops; Value : in Uint32);
   procedure Write_BE_32 (Destination : in RWops; Value : in Uint32);
   procedure Write_LE_64 (Destination : in RWops; Value : in Uint64);
   procedure Write_BE_64 (Destination : in RWops; Value : in Uint64);

   function Is_Null (Source : in RWops) return Boolean with
     Inline_Always => True;
private
   type Whence_Type is new C.int;

   RW_Seek_Set : constant Whence_Type := 0;
   RW_Seek_Cur : constant Whence_Type := 1;
   RW_Seek_End : constant Whence_Type := 2;

   --  The SDL_RWops struct contains a union which is only used internally by SDL.
   --
   --  The manual states that when a new RWops implementation is written, the Unknown struct within the union can be
   --  used to store data, this consists of two pointers. This is all that is allowed.
   --
   --  TODO: Make it generic passing in two access types of convention C.
   type User_Datums is
      record
         Data_1 : System.Address;
         Data_2 : System.Address;
      end record with
     Convention => C;

   type Stream_Types is new C.unsigned;

   Unknown_Stream : constant Stream_Types := 0;

   type RWops_Pointer;

   type SDL_RWops is
      record
         Size        : access function (Context : in RWops_Pointer) return Offsets;
         Seek        : access function (Context : in RWops_Pointer;
                                        Offset  : in Offsets;
                                        Whence  : in Whence_Type) return Offsets;
         Read        : access function (Context : in RWops_Pointer;
                                        Ptr     : in System.Address;
                                        Size    : in Sizes;
                                        Max_Num : in C.unsigned_long) return C.unsigned_long;
         Write       : access function (Context : in RWops_Pointer;
                                        Ptr     : in System.Address;
                                        Size    : in Sizes;
                                        Num     : in C.unsigned_long) return C.unsigned_long;
         Close       : access function (Context : in RWops_Pointer) return C.int;
         Stream_Type : Stream_Types;  --  When creating a RWops, this should always be set to Unknown_Stream.
         User_Data   : User_Datums;
      end record with
     Convention => C_Pass_By_Copy;

   type RWops_Pointer is access all SDL_RWops with
     Convention => C;

   type RWops is new RWops_Pointer;
end SDL.RWops;
