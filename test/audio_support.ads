with SDL.Audio.Devices;
with SDL.Audio.Sample_Formats;
with Interfaces;

package Audio_Support is

   subtype Sample is Interfaces.Integer_16;

   type Frames is record
      L, R : Sample;
   end record with
     Convention => C;

   subtype Buffer_Index is Positive range Positive'Range;

   type Buffer_Type is array (Buffer_Index range <>) of Frames;

   package Audio_Devices is new SDL.Audio.Devices
     (Frame_Type   => Frames,
      Buffer_Index => Buffer_Index,
      Buffer_Type  => Buffer_Type);

   Sample_Format : constant SDL.Audio.Sample_Formats.Sample_Format :=
     SDL.Audio.Sample_Formats.Sample_Format_S16SYS;

   type Support_User_Data is new Audio_Devices.User_Data with private;

   procedure Callback
     (User   : in Audio_Devices.User_Data_Access;
      Buffer : out Buffer_Type);

private

   type Pulse_State is (Low, High);

   Pulse_Frames : constant array (Pulse_State) of Frames :=
     (Low  => (Sample'First, Sample'First),
      High => (Sample'Last,  Sample'Last));

   type Support_User_Data is new Audio_Devices.User_Data with record
      Frame_Count : Natural := 0;
      State       : Pulse_State := Low;
   end record;

   type Support_User_Data_Access is access all Support_User_Data;

end Audio_Support;
