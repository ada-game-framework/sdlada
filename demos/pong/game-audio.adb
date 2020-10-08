--  Pong-Demo for SDLAda, audio stuff.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Ada.Command_Line,
     Ada.Directories,
     Ada.Text_IO;

with Interfaces.C;

with SDL.Audio,
     SDL.Error;

with System.Storage_Elements;

package body Game.Audio is

   subtype Byte_Array is System.Storage_Elements.Storage_Array;
   subtype Byte_Index is System.Storage_Elements.Storage_Offset;

   --  WAV_Info
   type WAV_Info is
      record
         Buffer : SDL.Audio.Audio_Buffer;
         Length : Interfaces.Unsigned_32;
      end record;

   No_Wave : constant WAV_Info :=
               WAV_Info'(Buffer => SDL.Audio.Null_Audio,
                         Length => 0);

   --  Play_Info
   type Play_Info is
      record
         Data       : SDL.Audio.Audio_Buffer;
         Length     : Byte_Index;
         Data_Index : Byte_Index;
      end record;

   Nothing : constant Play_Info :=
               Play_Info'(Data       => SDL.Audio.Null_Audio,
                          Length     => 0,
                          Data_Index => 0);

   --  Loaded WAVs.
   WAV_Ping : WAV_Info;
   WAV_Pong : WAV_Info;

   --  Which is currently playing.
   Currently_Playing : Play_Info;

   ---------------------------------------------------------------------
   --  Load_Data
   ---------------------------------------------------------------------
   procedure Load_Data;

   procedure Load_Data is
      Data_Dir : constant String :=
        Ada.Directories.Compose
          (Containing_Directory =>
             Ada.Directories.Containing_Directory
               (Name => Ada.Command_Line.Command_Name),
           Name => "data");
      WAV_Spec : SDL.Audio.Audio_Spec;
      Success  : Boolean;
   begin
      SDL.Audio.Load_WAV (File_Name =>
                            Ada.Directories.Compose
                              (Containing_Directory => Data_Dir,
                               Name                 => "ping",
                               Extension            => "wav"),
                          Spec      => WAV_Spec,
                          Audio_Buf => WAV_Ping.Buffer,
                          Audio_Len => WAV_Ping.Length,
                          Success   => Success);
      pragma Unreferenced (WAV_Spec);

      if not Success then
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => SDL.Error.Get);

         WAV_Ping := No_Wave;
      end if;

      SDL.Audio.Load_WAV (File_Name =>
                             Ada.Directories.Compose
                              (Containing_Directory => Data_Dir,
                               Name                 => "pong",
                               Extension            => "wav"),
                          Spec      => WAV_Spec,
                          Audio_Buf => WAV_Pong.Buffer,
                          Audio_Len => WAV_Pong.Length,
                          Success   => Success);

      if not Success then
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => SDL.Error.Get);

         WAV_Pong := No_Wave;
      end if;
   end Load_Data;

   ---------------------------------------------------------------------
   --  Callback
   ---------------------------------------------------------------------
   procedure Callback (User_Data : in SDL.Audio.User_Data_Ptr;
                       Stream    : in SDL.Audio.Audio_Buffer;
                       Length    : in Interfaces.C.int);
   pragma Convention (Convention => C,
                      Entity     => Callback);

   procedure Callback (User_Data : in SDL.Audio.User_Data_Ptr;
                       Stream    : in SDL.Audio.Audio_Buffer;
                       Length    : in Interfaces.C.int)
   is
      use type SDL.Audio.Audio_Buffer;
      use type Byte_Index;

      Cur_Play : Play_Info;
      for Cur_Play'Address use System.Address (User_Data);
      In_Buf   : Byte_Array (0 .. Byte_Index (Cur_Play.Length - 1));
      for In_Buf'Address use System.Address (Cur_Play.Data);
      Out_Buf  : Byte_Array (0 .. Byte_Index (Length) - 1);
      for Out_Buf'Address use System.Address (Stream);

      Last_Byte : Byte_Index;
   begin
      if
        Cur_Play.Data /= SDL.Audio.Null_Audio
      then
         --  Now fill buffer with audio data and update tbe audio index.
         Last_Byte := Byte_Index'Min (Byte_Index (Cur_Play.Length - Cur_Play.Data_Index),
                                      Out_Buf'Length);

         Out_Buf (Out_Buf'First .. Last_Byte - 1) :=
           In_Buf (Cur_Play.Data_Index .. Cur_Play.Data_Index + Last_Byte - 1);
         Out_Buf (Last_Byte     .. Out_Buf'Last) := (others => 0);

         Cur_Play.Data_Index := Cur_Play.Data_Index + Last_Byte;

         if
           Cur_Play.Data_Index >= Cur_Play.Length
         then
            Cur_Play := Nothing;
         end if;
      else
         --  Fill target buffer with silence.
         Out_Buf := Byte_Array'(Out_Buf'Range => 0);
      end if;
   end Callback;

   ---------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------
   procedure Initialize is
      Required : SDL.Audio.Audio_Spec;
      Success  : Boolean;
   begin
      Currently_Playing := Nothing;

      Load_Data;
      Required :=
        SDL.Audio.Audio_Spec'(Frequency => 48_000,
                              Format    => SDL.Audio.Signed_16_LE,
                              Channels  => 1,
                              Silence   => 0,
                              Samples   => 512,
                              Padding   => 0,
                              Size      => 0,
                              Callback  => Callback'Access,
                              User_Data => SDL.Audio.User_Data_Ptr (Currently_Playing'Address));

      SDL.Audio.Open (Required => Required,
                      Success  => Success);
      pragma Unreferenced (Required);

      if not Success then
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => "Failed to initialize audio");
      else
         SDL.Audio.Pause (Pause_On => SDL.Audio.False);
      end if;
   end Initialize;

   ---------------------------------------------------------------------
   --  Finalize
   ---------------------------------------------------------------------
   procedure Finalize is
   begin
      SDL.Audio.Pause (Pause_On => SDL.Audio.True);
      SDL.Audio.Close;

      SDL.Audio.Free_WAV (Audio_Buf => WAV_Ping.Buffer);
      SDL.Audio.Free_WAV (Audio_Buf => WAV_Pong.Buffer);
   end Finalize;

   ---------------------------------------------------------------------
   --  Play_Ping
   ---------------------------------------------------------------------
   procedure Play_Ping is
      use type SDL.Audio.Audio_Buffer;
   begin
      SDL.Audio.Lock;

      --  Only write new buffer if previous one has played already.
      if
        Currently_Playing.Data = SDL.Audio.Null_Audio
      then
         Currently_Playing :=
           Play_Info'(Data       => WAV_Ping.Buffer,
                      Length     => Byte_Index (WAV_Ping.Length),
                      Data_Index => 0);
      end if;

      SDL.Audio.Unlock;
   end Play_Ping;

   ---------------------------------------------------------------------
   --  Play_Pong
   ---------------------------------------------------------------------
   procedure Play_Pong is
      use type SDL.Audio.Audio_Buffer;
   begin
      SDL.Audio.Lock;

      --  Only write new buffer if previous one has played already.
      if
        Currently_Playing.Data = SDL.Audio.Null_Audio
      then
         Currently_Playing :=
           Play_Info'(Data       => WAV_Pong.Buffer,
                      Length     => Byte_Index (WAV_Pong.Length),
                      Data_Index => 0);
      end if;

      SDL.Audio.Unlock;
   end Play_Pong;

end Game.Audio;
