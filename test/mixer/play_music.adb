--
--
--

with Ada.Text_IO;
with Ada.Command_Line;

with SDL.Mixer.Music;

procedure Play_Music is
   use Ada.Text_IO;
   use SDL.Mixer;
   use SDL.Mixer.Music;

   procedure Put_State;

   procedure Put_State is
   begin
      Put_Line ("Playing: " & Is_Playing'Image);
      Put_Line ("Paused : " & Is_Paused'Image);
      Put_Line ("Fading : " & Fading'Image);
      New_Line;
   end Put_State;

   Music : Music_Type;
begin
   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line ("Usage: play_music musicfile");
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
   Load_MUS (Ada.Command_Line.Argument (1), Music);
   Put_State;

   Put ("Music type: ");
   Put_Line (Get_Type (Music)'Image);
   New_Line;

   Put_Line ("Play");
   Play (Music, Loops => Loop_Forever);
   Put_State;
   delay 2.000;

   Put_Line ("Pause");
   Pause;
   Put_State;
   delay 2.000;

   Put_Line ("Resume");
   Resume;
   Put_State;
   delay 2.000;

   Put_Line ("Rewind");
   Rewind;
   Put_State;
   delay 15.000;

   Put_Line ("Fade out");
   Fade_Out (Ms => 1000);
   Put_State;
   delay 2.000;

   Put_Line ("Fade in");
   Fade_In (Music, Loops => Loop_Forever, Ms => 1000);
   Put_State;
   delay 5.000;

   Put_Line ("Cleanup");
   Pause;
   Free (Music);

   Close;
   Quit;

end Play_Music;
