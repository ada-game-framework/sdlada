--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;

package body SDL.Mixer.Channels is
   procedure Allocate (Channel_Count : in Positive) is
      function Mix_Allocate_Channels (numchannels : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_AllocateChannels";

      Allocated : constant C.int := Mix_Allocate_Channels (C.int (Channel_Count));
      pragma Unreferenced (Allocated);
   begin
      null;
   end Allocate;


   procedure Volume (Channel : in Channel_Index; New_Volume : in Volume_Type) is
      function Mix_Volume (Channel : in C.int;
                           Volume  : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Volume";

      Current_Volumen : constant C.int := Mix_Volume (C.int (Channel),
                                                      C.int (New_Volume));
      pragma Unreferenced (Current_Volumen);
   begin
      null;
   end Volume;


   procedure Play (Channel : in Channel_Index; Chunk : in Chunk_Type; Loops : in Loop_Count) is
   begin
      Play_Timed (Channel, Chunk, Loops, Ticks => -1);
   end Play;


   procedure Play_Timed (Channel : in Channel_Index;
                         Chunk   : in Chunk_Type;
                         Loops   : in Loop_Count;
                         Ticks   : in Ticks_Count) is
      function Mix_Play_Channel_Timed (Channel : in C.int;
                                       Chunk   : in Chunk_Type;
                                       Loops   : in C.int;
                                       Ticks   : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayChannelTimed";

      Channel_Used : constant C.int := Mix_Play_Channel_Timed (C.int (Channel),
                                                               Chunk,
                                                               C.int (Loops),
                                                               C.int (Ticks));
      pragma Unreferenced (Channel_Used);
   begin
      null;
   end Play_Timed;


   procedure Fade_In (Channel : in Channel_Index;
                      Chunk   : in Chunk_Type;
                      Loops   : in Loop_Count;
                      Ms      : in Time_Ms) is
   begin
      Fade_In_Timed (Channel, Chunk, Loops, Ms, Ticks => -1);
   end Fade_In;


   procedure Fade_In_Timed (Channel : in Channel_Index;
                            Chunk   : in Chunk_Type;
                            Loops   : in Loop_Count;
                            Ms      : in Time_Ms;
                            Ticks   : in Ticks_Count) is
      function Mix_Fade_In_Channel_Timed (Channel : in C.int;
                                          Chunk   : in Chunk_Type;
                                          Loops   : in C.int;
                                          Ms      : in C.int;
                                          Ticks   : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeInChannelTimed";

      Channel_Used : constant C.int := Mix_Fade_In_Channel_Timed (C.int (Channel),
                                                                  Chunk,
                                                                  C.int (Loops),
                                                                  C.int (Ms),
                                                                  C.int (Ticks));
      pragma Unreferenced (Channel_Used);
   begin
      null;
   end Fade_In_Timed;

   procedure Pause (Channel : in Channel_Index) is
      procedure Mix_Pause (Channel : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Pause";
   begin
      Mix_Pause (C.int (Channel));
   end Pause;


   procedure Resume (Channel : in Channel_Index) is
      procedure Mix_Resume (Channel : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Resume";
   begin
      Mix_Resume (C.int (Channel));
   end Resume;


   procedure Halt (Channel : in Channel_Index) is
      function Mix_Halt_Channel (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HaltChannel";

      Dummy : constant C.int := Mix_Halt_Channel (C.int (Channel));
      pragma Unreferenced (Dummy);
   begin
      null;
   end Halt;


   procedure Expire (Channel : in Channel_Index; Ticks : in Ticks_Count) is
      function Mix_Expire_Channel (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_ExpireChannel";

      Number_Channels : constant C.int := Mix_Expire_Channel (C.int (Channel));
      pragma Unreferenced (Number_Channels);
   begin
      null;
   end Expire;


   procedure Fade_Out (Channel : in Channel_Index; Ms : in Time_Ms) is
      function Mix_Fade_Out_Channel (Channel : in C.int;
                                     Ms      : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeOutChannel";

      Number_Channels : constant C.int := Mix_Fade_Out_Channel (C.int (Channel), C.int (Ms));
      pragma Unreferenced (Number_Channels);
   begin
      null;
   end Fade_Out;


   function Is_Playing (Channel : in Channel_Index) return Boolean is
      function Mix_Playing (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Playing";
   begin
      return Mix_Playing (C.int (Channel)) /= Success;
   end Is_Playing;


   function Playing_Count return Natural is
      function Mix_Playing (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Playing";
   begin
      return Natural (Mix_Playing (-1));
   end Playing_Count;


   function Is_Paused (Channel : in Channel_Index) return Boolean is
      function Mix_Paused (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Paused";
   begin
      return Mix_Paused (C.int (Channel)) /= Success;
   end Is_Paused;


   function Paused_Count return Natural is
      function Mix_Paused (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Paused";
   begin
      return Natural (Mix_Paused (-1));
   end Paused_Count;


   function Fading (Channel : in Channel_Index) return Fading_Type is
      function Mix_Fading_Channel (Channel : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadingChannel";
   begin
      return Fading_Type'Val (Mix_Fading_Channel (C.int (Channel)));
   end Fading;


   function Get_Chunk (Channel : in Channel_Index) return Chunk_Type is
      function Mix_Get_Chunk (Channel : in C.int) return Chunk_Type with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetChunk";

      Result : constant Chunk_Type := Mix_Get_Chunk (C.int (Channel));
   begin
      if Result = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      return Result;
   end Get_Chunk;
end SDL.Mixer.Channels;
