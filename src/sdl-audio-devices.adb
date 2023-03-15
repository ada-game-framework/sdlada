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
with Interfaces.C.Strings;
with SDL.Error;
with Ada.Unchecked_Conversion;

package body SDL.Audio.Devices is
   package C renames Interfaces.C;

   function Total_Devices
     (Is_Capture : in Boolean := False)
      return Positive
   is
      function SDL_Get_Num_Audio_Devices
        (Is_Capture : in SDL_Bool)
         return C.int
        with
          Import        => True,
          Convention    => C,
          External_Name => "SDL_GetNumAudioDevices";

      Num : constant C.int := SDL_Get_Num_Audio_Devices (To_Bool (Is_Capture));
   begin
      if Num < 0 then
         raise Audio_Device_Error with SDL.Error.Get;
      end if;

      return Positive (Num);
   end Total_Devices;

   function Get_Name
     (Index      : in Positive;
      Is_Capture : in Boolean := False)
      return String
   is
      function SDL_Get_Audio_Device_Name
        (Index : in C.int; Is_Capture : in SDL_Bool)
         return C.Strings.chars_ptr
        with
          Import        => True,
          Convention    => C,
          External_Name => "SDL_GetAudioDeviceName";

      --  Index is zero based, so need to subtract 1 to correct it.
      C_Str : constant C.Strings.chars_ptr := SDL_Get_Audio_Device_Name
        (C.int (Index) - 1, To_Bool (Is_Capture));
   begin
      return C.Strings.Value (C_Str);
   end Get_Name;

   function Open
     (Name            : in String := "";
      Is_Capture      : in Boolean := False;
      Desired         : in Desired_Spec;
      Obtained        : out Obtained_Spec;
      Callback        : in Audio_Callback := null;
      User_Data       : in User_Data_Access := null;
      Allowed_Changes : in Changes := None)
      return Device
   is
   begin
      return Result : Device do
         Open
           (Result, Name, Is_Capture, Desired,
            Obtained, Callback, User_Data, Allowed_Changes);
      end return;
   end Open;

   procedure Open
     (Self            : out Device;
      Name            : in String := "";
      Is_Capture      : in Boolean := False;
      Desired         : in Desired_Spec;
      Obtained        : out Obtained_Spec;
      Callback        : in Audio_Callback := null;
      User_Data       : in User_Data_Access := null;
      Allowed_Changes : in Changes := None)
   is
      function SDL_Open_Audio_Device
        (C_Name     : in C.Strings.chars_ptr;
         Is_Capture : in SDL_Bool;
         D          : in Internal_Spec_Ptr;
         O          : in Internal_Spec_Ptr;
         AC         : in Changes)
         return C.int
        with
          Import        => True,
          Convention    => C,
          External_Name => "SDL_OpenAudioDevice";

      Desired_Internal, Obtained_Internal : aliased Internal_Spec;

      C_Str  : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      Num : C.int;
   begin
      Desired_Internal :=
        To_Internal_Spec
          (From     => Desired,
           Callback => Callback,
           External => Self.External'Unchecked_Access);

      if Name /= "" then
         C_Str := C.Strings.New_String (Name);

         Num := SDL_Open_Audio_Device
           (C_Name     => C_Str,
            Is_Capture => To_Bool (Is_Capture),
            D          => Desired_Internal'Unrestricted_Access,
            O          => Obtained_Internal'Unchecked_Access,
            AC         => Allowed_Changes);

         C.Strings.Free (C_Str);
      else
         Num := SDL_Open_Audio_Device
           (C_Name     => C.Strings.Null_Ptr,
            Is_Capture => To_Bool (Is_Capture),
            D          => Desired_Internal'Unrestricted_Access,
            O          => Obtained_Internal'Unchecked_Access,
            AC         => Allowed_Changes);
      end if;

      Obtained := To_External_Spec (Obtained_Internal);

      if Num < 0 then -- Will alwats be >= 2 if successful
         raise Audio_Device_Error with SDL.Error.Get;
      end if;

      Internal_Open (Self, Num, Callback, User_Data);
   end Open;

   procedure Queue
     (Self : in Device;
      Data : aliased in Buffer_Type)
   is
      use Interfaces;

      function SDL_Queue_Audio
        (Dev  : in ID;
         Data : in System.Address;
         Len  : in Interfaces.Unsigned_32)
         return C.int
        with
          Import        => True,
          Convention    => C,
          External_Name => "SDL_QueueAudio";

      Num : C.int;
   begin
      Num := SDL_Queue_Audio
        (Dev  => Self.Internal,
         Data => Data (Data'First)'Address,
         Len  => Data'Size / System.Storage_Unit);

      if Num < 0 then
         raise Audio_Device_Error with SDL.Error.Get;
      end if;
   end Queue;

   procedure Internal_Callback
     (External    : in External_Data_Ptr;
      Data        : in System.Address;
      Byte_Length : in Positive)
   is
      Frame_Size  : constant Positive := Frame_Type'Size / System.Storage_Unit;
      Frame_Count : constant Positive := Byte_Length / Frame_Size;

      First_Index : constant Buffer_Index := Buffer_Index'First;
      Last_Index  : constant Buffer_Index := Buffer_Index'Val (Frame_Count);

      subtype Constrained_Buffer is Buffer_Type (First_Index .. Last_Index);
      type Constrained_Buffer_Ptr is access Constrained_Buffer;

      function To_Ada_Array is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Constrained_Buffer_Ptr);

      Ada_Array : constant Constrained_Buffer_Ptr := To_Ada_Array (Data);
   begin
      External.Callback (External.User_Data, Ada_Array.all);
   end Internal_Callback;

   function Get_Status (Self : in Device) return Audio_Status is
      function SDL_Get_Audio_Device_Status (Dev : in ID) return Audio_Status
        with
          Import        => True,
          Convention    => C,
          External_Name => "SDL_GetAudioDeviceStatus";
   begin
      return SDL_Get_Audio_Device_Status (Self.Internal);
   end Get_Status;

   function Get_ID (Self : in Device) return ID is
   begin
      return Self.Internal;
   end Get_ID;

   procedure Pause (Self : in Device; Pause : in Boolean) is
      procedure SDL_Pause_Audio_Device (Dev : in ID; P : in SDL_Bool)
      with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_PauseAudioDevice";
   begin
      SDL_Pause_Audio_Device (Self.Internal, To_Bool (Pause));
   end Pause;

   function Get_Queued_Size (Self : in Device) return Interfaces.Unsigned_32 is
      function SDL_Get_Queued_Audio_Size (Dev : in ID)
         return Interfaces.Unsigned_32
      with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetQueuedAudioSize";
   begin
      return SDL_Get_Queued_Audio_Size (Self.Internal);
   end Get_Queued_Size;

   procedure Clear_Queued (Self : in Device) is
      procedure SDL_Clear_Queued_Audio (Dev : in ID)
      with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_ClearQueuedAudio";
   begin
      SDL_Clear_Queued_Audio (Self.Internal);
   end Clear_Queued;

   procedure Close (Self : in out Device) is
      procedure SDL_Close_Audio_Device (Dev : in ID)
      with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CloseAudioDevice";
   begin
      SDL_Close_Audio_Device (Self.Internal);
      Self.Is_Open := False;
   end Close;

   procedure Internal_Open
     (Self      : out Device;
      Num       : in C.int;
      Callback  : in Audio_Callback;
      User_Data : in User_Data_Access)
   is
   begin
      Self.Internal := ID (Num);
      Self.Is_Open := True;
      Self.External.Callback := Callback;
      Self.External.User_Data := User_Data;
   end Internal_Open;

   function To_Internal_Spec
     (From     : in Desired_Spec;
      Callback : in Audio_Callback;
      External : in External_Data_Ptr)
      return Internal_Spec is
   begin
      return Result : Internal_Spec do
         Result.Frequency := From.Frequency;
         Result.Format    := From.Format;
         Result.Channels  := From.Channels;
         Result.Samples   := From.Samples;
         Result.Callback  :=
           (if Callback /= null then Internal_Callback'Access else null);
         Result.User_Data := External;
      end return;
   end To_Internal_Spec;

   function To_External_Spec (From : Internal_Spec) return Obtained_Spec is
   begin
      return Result : Obtained_Spec do
         Result :=
           (Mode      => Obtained,
            Frequency => From.Frequency,
            Format    => From.Format,
            Channels  => From.Channels,
            Samples   => From.Samples,
            Silence   => From.Silence,
            Size      => From.Size);
      end return;
   end To_External_Spec;

   overriding
   procedure Finalize (Self : in out Device) is
   begin
      if Self.Is_Open then
         Self.Close;
      end if;
   end Finalize;

end SDL.Audio.Devices;
