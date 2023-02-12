with SDL;
with SDL.Log;
with SDL.Audio;
with Audio_Support; use Audio_Support;

procedure Audio is
   Total_Drivers : Positive;
   Total_Devices : Positive;
   Success : Boolean;

   Playback_Length : constant Duration := 2.0;

   Desired  : Audio_Devices.Desired_Spec;
   Obtained : Audio_Devices.Obtained_Spec;

   State : aliased Audio_Support.Support_User_Data;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   Total_Drivers := SDL.Audio.Total_Drivers;
   SDL.Log.Put_Debug ("Total Audio Drivers : " & Total_Drivers'Img);
   for i in 1 .. Total_Drivers loop
      SDL.Log.Put_Debug ("Driver" & i'Img & " : " & SDL.Audio.Driver_Name (i));
   end loop;

   Success := SDL.Initialise;
   SDL.Log.Put_Debug ("SDL Init : " & Success'Img);

   Total_Devices := Audio_Devices.Total_Devices (False);
   SDL.Log.Put_Debug ("Total Audio Devices : " & Total_Devices'Img);
   for i in 1 .. Total_Devices loop
      SDL.Log.Put_Debug ("Device" & i'Img & " : " & Audio_Devices.Get_Name (i));
   end loop;

   Desired.Frequency := 48_000;
   Desired.Format    := Audio_Support.Sample_Format;
   Desired.Channels  := 2;
   Desired.Samples   := 4_096;

   SDL.Log.Put_Debug ("Desired - Frequency :" & Desired.Frequency'Img);
   SDL.Log.Put_Debug ("Desired - Format/Bit_Size :" & Desired.Format.Bit_Size'Img);
   SDL.Log.Put_Debug ("Desired - Format/Float :" & Desired.Format.Float'Img);
   SDL.Log.Put_Debug ("Desired - Format/Big_Endian :" & Desired.Format.Endianness'Img);
   SDL.Log.Put_Debug ("Desired - Format/Signed :" & Desired.Format.Signed'Img);
   SDL.Log.Put_Debug ("Desired - Channels :" & Desired.Channels'Img);
   SDL.Log.Put_Debug ("Desired - Samples :" & Desired.Samples'Img);

   SDL.Log.Put_Debug ("Opening Default Device");

   --  Use a callback to provide samples
   declare
      Device : constant Audio_Devices.Device :=
        Audio_Devices.Open
          (Desired   => Desired,
           Obtained  => Obtained,
           Callback  => Audio_Support.Callback'Access,
           User_Data => State'Unchecked_Access);
   begin
      SDL.Log.Put_Debug ("Opened Device:" & Audio_Devices.Get_ID (Device)'Img);
      SDL.Log.Put_Debug ("Device Status: " & Audio_Devices.Get_Status (Device)'Img);

      SDL.Log.Put_Debug ("Obtained - Frequency :" & Obtained.Frequency'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Bit_Size :" & Obtained.Format.Bit_Size'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Float : " & Obtained.Format.Float'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Endianness : " & Obtained.Format.Endianness'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Signed : " & Obtained.Format.Signed'Img);
      SDL.Log.Put_Debug ("Obtained - Channels :" & Obtained.Channels'Img);
      SDL.Log.Put_Debug ("Obtained - Samples :" & Obtained.Samples'Img);
      SDL.Log.Put_Debug ("Obtained - Silence :" & Obtained.Silence'Img);
      SDL.Log.Put_Debug ("Obtained - Size :" & Obtained.Size'Img);

      SDL.Log.Put_Debug ("Unpausing Device: " & Audio_Devices.Get_Status (Device)'Img);
      Audio_Devices.Pause (Device, False);
      SDL.Log.Put_Debug ("Device Status: " & Audio_Devices.Get_Status (Device)'Img);

      delay Playback_Length;

      SDL.Log.Put_Debug ("Implicitly Closing Device...");
   end;

   SDL.Log.Put_Debug ("Device Closed, silence expected.");

   delay 1.0;

   --  Use a queue to provide samples
   declare
      Device : constant Audio_Devices.Device :=
        Audio_Devices.Open
          (Desired  => Desired,
           Obtained => Obtained);
      Buffer : aliased Audio_Support.Buffer_Type := (1 .. 4_096 => (0, 0));
      Segment_Count : constant := 40;
   begin
      SDL.Log.Put_Debug ("Opened Device: " & Audio_Devices.Get_ID (Device)'Img);
      SDL.Log.Put_Debug ("Device Status: " & Audio_Devices.Get_Status (Device)'Img);

      SDL.Log.Put_Debug ("Obtained - Frequency :" & Obtained.Frequency'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Bit_Size :" & Obtained.Format.Bit_Size'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Float : " & Obtained.Format.Float'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Endianness : " & Obtained.Format.Endianness'Img);
      SDL.Log.Put_Debug ("Obtained - Format/Signed :" & Obtained.Format.Signed'Img);
      SDL.Log.Put_Debug ("Obtained - Channels :" & Obtained.Channels'Img);
      SDL.Log.Put_Debug ("Obtained - Samples :" & Obtained.Samples'Img);
      SDL.Log.Put_Debug ("Obtained - Silence :" & Obtained.Silence'Img);
      SDL.Log.Put_Debug ("Obtained - Size :" & Obtained.Size'Img);

      SDL.Log.Put_Debug ("Unpausing Device: " & Audio_Devices.Get_Status (Device)'Img);
      Audio_Devices.Pause (Device, False);
      SDL.Log.Put_Debug ("Device Status: " & Audio_Devices.Get_Status (Device)'Img);

      for i in 1 .. Segment_Count loop
         Audio_Support.Callback (State'Unchecked_Access, Buffer);
         Audio_Devices.Queue (Device, Buffer);
         delay Playback_Length / Duration (Segment_Count);
      end loop;

      SDL.Log.Put_Debug ("Implicitly Closing Device...");
   end;

   SDL.Log.Put_Debug ("Device Closed, silence expected.");

   delay 1.0;

   SDL.Log.Put_Debug ("Program Finished.");

   SDL.Finalise;
end Audio;
