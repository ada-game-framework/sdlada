--
--
--

with Ada.Text_IO;

with SDL.Versions;

with SDL.Mixer.Versions;
with SDL.Mixer.Chunks;
with SDL.Mixer.Music;

procedure Mix_Inspection is
   use Ada.Text_IO;
   use SDL.Mixer;
   Compiled_Version : constant SDL.Versions.Version := SDL.Mixer.Versions.Compiled;
   Linked_Version   : SDL.Versions.Version;
begin
   SDL.Mixer.Versions.Linked_With (Linked_Version);
   Put_Line ("Compiled with version: "
               & Compiled_Version.Major'Image & "."
               & Compiled_Version.Minor'Image & "."
               & Compiled_Version.Patch'Image);
   Put_Line ("Linked with version  : "
               & Linked_Version.Major'Image & "."
               & Linked_Version.Minor'Image & "."
               & Linked_Version.Patch'Image);
   New_Line;

   if not SDL.Initialise then
      Put_Line ("SDL.Initialise error.");
   end if;

   Open (Frequency  => Default_Frequency,
         Format     => 16#8010#,
         Channels   => 2,
         Chunk_Size => 2048);
   Initialise (Init_Flac + Init_MOD + Init_MP3 + Init_OGG + Init_MID + Init_Opus);

   declare
      Chunk_Count : constant Natural := Chunks.Number_Of_Decoders;
      Music_Count : constant Natural := Music.Number_Of_Decoders;
   begin
      Put_Line ("Chunk decoder count: " & Chunk_Count'Image);
      for Index in 1 .. Chunk_Count loop
         Put (Index'Image);
         Put (") ");
         Put (Chunks.Decoder_Name (Index));
         New_Line;
      end loop;
      New_Line;

      Put_Line ("Music decoder count: " & Music_Count'Image);
      for Index in 1 .. Music_Count loop
         Put (Index'Image);
         Put (") ");
         Put (Music.Decoder_Name (Index));
         New_Line;
      end loop;
   end;
   New_Line;
end Mix_Inspection;
