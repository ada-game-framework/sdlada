--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Mixer.Channels
--------------------------------------------------------------------------------------------------------------------

package SDL.Mixer.Channels is

   type Channel_Index is new Integer;
   All_Channels : constant Channel_Index := -1;

   type Loop_Count is new Integer;
   Loop_Forever : constant Loop_Count := -1;

   type Time_Ms     is new Integer;
   type Ticks_Count is new Integer;

   procedure Allocate (Channel_Count : in Positive);
   --  Dynamically change the number of channels managed by the mixer.
   --  If decreasing the number of channels, the upper channels are
   --  stopped.

   procedure Volume (Channel    : in Channel_Index;
                     New_Volume : in Volume_Type);
   --  Set the volume in the range of 0-128 of a specific channel or chunk.
   --  If the specified channel is -1, set volume for all channels.
   --  Returns the original volume.
   --  If the specified volume is -1, just return the current volume.

   procedure Play (Channel : in Channel_Index;
                   Chunk   : in Chunk_Type;
                   Loops   : in Loop_Count);
   procedure Play_Timed (Channel : in Channel_Index;
                         Chunk   : in Chunk_Type;
                         Loops   : in Loop_Count;
                         Ticks   : in Ticks_Count);
   --  Play an audio chunk on a specific channel.
   --  If Channel is All_Channels (-1), play on the first free channel.
   --  If Loops is greater than zero, loop the sound that many times.
   --  If Loops is Loop_Forever (-1), loop inifinitely (~65000 times).
   --  Returns which channel was used to play the sound.

   procedure Fade_In (Channel : in Channel_Index;
                      Chunk   : in Chunk_Type;
                      Loops   : in Loop_Count;
                      Ms      : in Time_Ms);
   procedure Fade_In_Timed (Channel : in Channel_Index;
                            Chunk   : in Chunk_Type;
                            Loops   : in Loop_Count;
                            Ms      : in Time_Ms;
                            Ticks   : in Ticks_Count);
   --  Fade in music or a channel over "ms" milliseconds, same semantics as the
   --  Play functions

   procedure Pause (Channel : in Channel_Index);
   procedure Resume (Channel : in Channel_Index);
   --  Pause/Resume a particular channel

   procedure Halt (Channel : in Channel_Index);
   --  Halt playing of a particular channel

   procedure Expire (Channel : in Channel_Index;
                     Ticks   : in Ticks_Count);
   --  Change the expiration delay for a particular channel.
   --  The sample will stop playing after the 'ticks' milliseconds have elapsed,
   --  or remove the expiration if 'ticks' is -1

   procedure Fade_Out (Channel : in Channel_Index;
                       Ms      : in Time_Ms);
   --  Halt a channel, fading it out progressively till it's silent
   --  The Ms parameter indicates the number of milliseconds the fading
   --  will take.

   function Is_Playing (Channel : in Channel_Index) return Boolean;
   function Playing_Count return Natural;
   function Is_Paused (Channel : in Channel_Index) return Boolean;
   function Paused_Count return Natural;
   --  Check the status of a specific channel.
   --  If Channel is All_Channels (-1), check all channels.

   function Fading (Channel : in Channel_Index) return Fading_Type;
   --  Query the fading status of a channel

   function Get_Chunk (Channel : in Channel_Index) return Chunk_Type;
   --  Get the Mix_Chunk currently associated with a mixer channel
   --  Returns Null if it's an invalid channel, or there's no chunk associated.

end SDL.Mixer.Channels;
