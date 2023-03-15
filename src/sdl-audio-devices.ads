--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2021, Eduard Llamosí
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
--  SDL.Audio.Devices
--
--  Operating system audio device access and control.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with SDL.Audio.Sample_Formats;
with System;

generic
   type Frame_Type is private;
   type Buffer_Index is (<>);
   type Buffer_Type is array (Buffer_Index range <>) of Frame_Type;
package SDL.Audio.Devices is

   Audio_Device_Error : exception;

   type Audio_Status is (Stopped, Playing, Paused) with Convention => C;

   type Changes is mod 2 ** 32 with
     Convention => C,
     Size       => C.int'Size;

   None      : constant Changes := 16#0000_0000#;
   Frequency : constant Changes := 16#0000_0001#;
   Format    : constant Changes := 16#0000_0002#;
   Channels  : constant Changes := 16#0000_0004#;
   Samples   : constant Changes := 16#0000_0008#;
   Any       : constant Changes := Frequency or Format or Channels or Samples;

   --  Allow users to derive new types from this.
   type User_Data is tagged private;

   type User_Data_Access is access all User_Data'Class;
   pragma No_Strict_Aliasing (User_Data_Access);

   --
   --  The calculated values in this structure are calculated by SDL_OpenAudio().
   --
   --  For multi-channel audio, the default SDL channel mapping is:
   --  2:  FL FR                       (stereo)
   --  3:  FL FR LFE                   (2.1 surround)
   --  4:  FL FR BL BR                 (quad)
   --  5:  FL FR FC BL BR              (quad + center)
   --  6:  FL FR FC LFE SL SR          (5.1 surround - last two can also be BL BR)
   --  7:  FL FR FC LFE BC SL SR       (6.1 surround)
   --  8:  FL FR FC LFE BL BR SL SR    (7.1 surround)
   --
   subtype Channel_Counts is Interfaces.Unsigned_8 with
     Static_Predicate => Channel_Counts in 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8;

   type Device is new Ada.Finalization.Limited_Controlled with private;

   type Audio_Callback is access procedure
     (User : in User_Data_Access;
      Data : out Buffer_Type);

   type Spec_Mode is (Desired, Obtained);

   type Spec (Mode : Spec_Mode) is record
      Frequency : C.int;
      Format    : SDL.Audio.Sample_Formats.Sample_Format;
      Channels  : Channel_Counts;
      Samples   : Interfaces.Unsigned_16;
      case Mode is
         when Desired =>
            null;
         when Obtained =>
            Silence : Interfaces.Unsigned_8;
            Size    : Interfaces.Unsigned_32;
      end case;
   end record;

   subtype Desired_Spec is Spec (Desired);
   subtype Obtained_Spec is Spec (Obtained);

   type ID is mod 2 ** 32 with
     Convention => C;

   function Total_Devices (Is_Capture : in Boolean := False) return Positive;

   function Get_Name
     (Index      : in Positive;
      Is_Capture : in Boolean := False)
      return String;

   function Open
     (Name            : in String := "";
      Is_Capture      : in Boolean := False;
      Desired         : in Desired_Spec;
      Obtained        : out Obtained_Spec;
      Callback        : in Audio_Callback := null;
      User_Data       : in User_Data_Access := null;
      Allowed_Changes : in Changes := None)
      return Device;

   procedure Open
     (Self            : out Device;
      Name            : in String := "";
      Is_Capture      : in Boolean := False;
      Desired         : in Desired_Spec;
      Obtained        : out Obtained_Spec;
      Callback        : in Audio_Callback := null;
      User_Data       : in User_Data_Access := null;
      Allowed_Changes : in Changes := None);

   procedure Queue
     (Self : in Device;
      Data : aliased in Buffer_Type);

   function Get_Status (Self : in Device) return Audio_Status;

   function Get_ID (Self : in Device) return ID;

   procedure Pause (Self : in Device; Pause : in Boolean);

   function Get_Queued_Size (Self : in Device) return Interfaces.Unsigned_32;

   procedure Clear_Queued (Self : in Device);

   procedure Close (Self : in out Device);

private

   Default_Device : constant C.int := 1;

   type User_Data is new Ada.Finalization.Controlled with null record;

   type External_Data is record
      Callback  : Audio_Callback;
      User_Data : User_Data_Access;
   end record;

   type External_Data_Ptr is access all External_Data;

   type Internal_Callback_Type is access procedure
     (User        : in External_Data_Ptr;
      Data        : in System.Address;
      Byte_Length : in Positive)
     with Convention => C;

   procedure Internal_Callback
     (External    : in External_Data_Ptr;
      Data        : in System.Address;
      Byte_Length : in Positive)
     with Convention => C;

   type Internal_Spec is record
      Frequency : C.int;
      Format    : SDL.Audio.Sample_Formats.Sample_Format;
      Channels  : Channel_Counts;
      Silence   : Interfaces.Unsigned_8;
      Samples   : Interfaces.Unsigned_16;
      Padding   : Interfaces.Unsigned_16;
      Size      : Interfaces.Unsigned_32;
      Callback  : Internal_Callback_Type;
      User_Data : External_Data_Ptr;
   end record with
     Convention => C;

   type Internal_Spec_Ptr is access all Internal_Spec with
     Convention => C;

   procedure Internal_Open
     (Self      : out Device;
      Num       : in C.int;
      Callback  : in Audio_Callback;
      User_Data : in User_Data_Access);

   function To_Internal_Spec
     (From     : in Desired_Spec;
      Callback : in Audio_Callback;
      External : in External_Data_Ptr)
      return Internal_Spec;

   function To_External_Spec (From : Internal_Spec) return Obtained_Spec;

   type Device is new Ada.Finalization.Limited_Controlled with
      record
         Internal : ID := 0;
         Is_Open  : Boolean := False;
         External : aliased External_Data;
      end record;

   overriding
   procedure Finalize (Self : in out Device);

end SDL.Audio.Devices;
