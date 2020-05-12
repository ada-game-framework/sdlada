--
--
--

with Ada.Text_IO;
with Ada.Command_Line;

with SDL.Mixer.Channels;
with SDL.Mixer.Chunks;

procedure Play_Channels is
   use Ada.Text_IO;
   use SDL.Mixer;
   use SDL.Mixer.Channels;

   procedure Put_State (Channel : in Channel_Index);

   procedure Put_State (Channel : in Channel_Index) is
   begin
      Put_Line ("Playing: " & Is_Playing (Channel)'Image);
      Put_Line ("Paused : " & Is_Paused  (Channel)'Image);
      Put_Line ("Fading : " & Fading     (Channel)'Image);
      New_Line;
   end Put_State;

   Chunk   : Chunk_Type;
   Channel : constant Channel_Index := 1;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line ("Usage: play_channels musicfile");
      return;
   end if;

   if not SDL.Initialise then
      Put_Line ("SDL.Initialise error.");
   end if;

   Open (Frequency  => Default_Frequency,
         Format     => 16#8010#,
         Channels   => 2,
         Chunk_Size => 2048);
   Initialise (Init_Flac + Init_MOD + Init_MP3 + Init_OGG + Init_MID + Init_Opus);

   Put_Line ("Load");
   Chunks.Load_WAV (Ada.Command_Line.Argument (1), Chunk);
   Put_State (Channel);

   Put_Line ("Allocate channels");
   Allocate (4);

   Put_Line ("Play");
   Play (Channel, Chunk, Loops => Loop_Forever);
   Put_State (Channel);
   delay 2.000;

   Put_Line ("Pause");
   Pause (Channel);
   Put_State (Channel);
   delay 2.000;

   Put_Line ("Resume");
   Resume (Channel);
   Put_State (Channel);
   delay 2.000;

   Put_Line ("Fade out");
   Fade_Out (Channel, Ms => 1000);
   Put_State (Channel);
   delay 2.000;

   Put_Line ("Fade in");
   Fade_In (Channel, Chunk, Loops => Loop_Forever, Ms => 1000);
   Put_State (Channel);
   delay 5.000;

   Put_Line ("Cleanup");
   Pause (Channel);
   Chunks.Free (Chunk);

   Close;
   Quit;

end Play_Channels;
