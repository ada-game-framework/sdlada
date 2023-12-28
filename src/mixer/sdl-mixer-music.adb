--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------

with SDL.Error;

with Interfaces.C.Strings;

package body SDL.Mixer.Music is

   ------------------------
   -- Number_Of_Decoders --
   ------------------------

   function Number_Of_Decoders return Natural
   is
      function Mix_Get_Num_Music_Decoders return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetNumMusicDecoders";
   begin
      return Natural (Mix_Get_Num_Music_Decoders);
   end Number_Of_Decoders;

   -----------------
   -- Decode_Name --
   -----------------

   function Decoder_Name (Index : in Positive) return String
   is
      use Interfaces.C.Strings;
      function Mix_Get_Music_Decoder_Name (Index : in C.int)
                                          return chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetMusicDecoder";
      Index_C : constant C.int := C.int (Index - 1);
   begin
      return Value (Mix_Get_Music_Decoder_Name (Index_C));
   end Decoder_Name;

   --------------
   -- Load_MUS --
   --------------

   procedure Load_MUS (Filename : in     String;
                       Music    :    out Music_Type)
   is
      use Interfaces.C.Strings;
      function Mix_Load_MUS (Filename : in chars_ptr) return Music_Type
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadMUS";
      Filename_C : constant chars_ptr  := New_String (Filename);
      Music_C    : constant Music_Type := Mix_Load_MUS (Filename_C);
   begin
      if Music_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      Music := Music_C;
   end Load_MUS;

   -----------------
   -- Load_MUS_RW --
   -----------------

   procedure Load_MUS_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Music       :    out Music_Type)
   is
      function Mix_Load_MUS_RW (Src      : in out SDL.RWops.RWops;
                                Free_Src : in     C.int) return Music_Type
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadMUS_RW";
      Music_C : constant Music_Type := Mix_Load_MUS_RW (Source, Boolean'Pos (Free_Source));
   begin
      if Music_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      Music := Music_C;
   end Load_MUS_RW;

   ----------------------
   -- Load_MUS_Type_RW --
   ----------------------

   procedure Load_MUS_Type_RW (Source      : in out SDL.RWops.RWops;
                               Typ         : in     Music_Type_Type;
                               Free_Source : in     Boolean;
                               Music       :    out Music_Type)
   is
      function Mix_Load_MUS_Type_RW (Src      : in out SDL.RWops.RWops;
                                     Typ      : in     Music_Type_Type;
                                     Free_Src : in     C.int) return Music_Type
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadMUSType_RW";
      Music_C : constant Music_Type := Mix_Load_MUS_Type_RW (Source, Typ,
                                                             Boolean'Pos (Free_Source));
   begin
      if Music_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      Music := Music_C;
   end Load_MUS_Type_RW;

   ----------
   -- Free --
   ----------

   procedure Free (Music : in out Music_Type)
   is
      procedure Mix_Free_Music (Music : in Music_Type)
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeMusic";
   begin
      Mix_Free_Music (Music);
   end Free;

   ----------
   -- Play --
   ----------

   procedure Play (Music : in Music_Type; Loops : in Loop_Count)
   is
      function Mix_Play_Music (Music : in Music_Type;
                               Loops : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayMusic";
      Result : constant C.int := Mix_Play_Music (Music, C.int (Loops));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Play;

   -------------
   -- Fade_In --
   -------------

   procedure Fade_In (Music : in Music_Type; Loops : in Loop_Count; Ms : in Integer)
   is
      function Mix_Fade_In_Music (Music : in Music_Type;
                                  Loops : in C.int;
                                  Ms    : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeInMusic";
      Result : constant C.int := Mix_Fade_In_Music (Music, C.int (Loops), C.int (Ms));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Fade_In;

   -----------------
   -- Fade_In_Pos --
   -----------------

   procedure Fade_In_Pos (Music    : in Music_Type;
                          Loops    : in Loop_Count;
                          Ms       : in Integer;
                          Position : in Long_Float)
   is
      function Mix_Fade_In_Music_Pos (Music : in Music_Type;
                                      Loops : in C.int;
                                      Ms    : in C.int;
                                      Pos   : in Long_Float) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeInMusicPos";
      Result : constant C.int := Mix_Fade_In_Music_Pos (Music,
                                                        C.int (Loops),
                                                        C.int (Ms),
                                                        Position);
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Fade_In_Pos;

   ------------
   -- Volume --
   ------------

   procedure Volume (New_Volume : in     Volume_Type;
                     Old_Volume :    out Volume_Type)
   is
      function Mix_Volume_Music (Volume : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_VolumeMusic";
      Volume_C     : constant C.int := C.int (New_Volume);
      Old_Volume_C : constant C.int := Mix_Volume_Music (Volume_C);
   begin
      Old_Volume := Volume_Type (Old_Volume_C);
   end Volume;

   ------------
   -- Volume --
   ------------

   procedure Volume (New_Volume : in Volume_Type)
   is
      Dummy : Volume_Type;
      pragma Unreferenced (Dummy);
   begin
      Volume (New_Volume, Dummy);
   end Volume;

   ----------------
   -- Get_Volume --
   ----------------

   function Get_Volume return Volume_Type
   is
      use Interfaces;
      function Mix_Volume_Music (Volume : in C.int) return Unsigned_8
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_VolumeMusic";
      Old_Volume_C : constant Unsigned_8 := Mix_Volume_Music (-1);
   begin
      return Volume_Type (Old_Volume_C);
   end Get_Volume;

   -----------
   -- Pause --
   -----------

   procedure Pause
   is
      procedure Mix_Pause_Music
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PauseMusic";
   begin
      Mix_Pause_Music;
   end Pause;

   ------------
   -- Resume --
   ------------

   procedure Resume
   is
      procedure Mix_Resume_Music
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_ResumeMusic";
   begin
      Mix_Resume_Music;
   end Resume;

   ------------
   -- Rewind --
   ------------

   procedure Rewind
   is
      procedure Mix_Rewind_Music
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_RewindMusic";
   begin
      Mix_Rewind_Music;
   end Rewind;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Position : in Long_Float) is
      function Mix_Set_Music_Position (Position : in Long_Float) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetMusicPosition";
      Result : constant C.int := Mix_Set_Music_Position (Position);
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Position;

   -------------
   -- Set_CMD --
   -------------

   procedure Set_CMD (Command : in String)
   is
      use Interfaces.C.Strings;
      function Mix_Set_Music_CMD (cmd : in chars_ptr) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetMusicCMD";
      Command_C : constant chars_ptr := New_String (Command);
      Result    : constant C.int     := Mix_Set_Music_CMD (Command_C);
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_CMD;

   ----------
   -- Halt --
   ----------

   procedure Halt is
      function Mix_Halt_Music return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HaltMusic";
      Dummy : constant C.int := Mix_Halt_Music;
      pragma Unreferenced (Dummy);
   begin
      null;
   end Halt;

   --------------
   -- Fade_Out --
   --------------

   procedure Fade_Out (Ms : in Integer) is
      function Mix_Fade_Out_Music (Ms : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeOutMusic";
      Result : constant C.int := Mix_Fade_Out_Music (Ms => C.int (Ms));
   begin
      if Result /= 1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Fade_Out;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Music : in Music_Type) return Music_Type_Type
   is
      function Mix_Get_Music_Type (Music : in Music_Type) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetMusicType";
      Result : constant C.int := Mix_Get_Music_Type (Music);
   begin
      return Music_Type_Type'Val (Result);
   end Get_Type;

   ----------------
   -- Is_Playing --
   ----------------

   function Is_Playing return Boolean
   is
      function Mix_Playing_Music return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayingMusic";
      Result : constant C.int := Mix_Playing_Music;
   begin
      return Result /= 0;
   end Is_Playing;

   ---------------
   -- Is_Paused --
   ---------------

   function Is_Paused return Boolean
   is
      function Mix_Paused_Music return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PausedMusic";
      Result : constant C.int := Mix_Paused_Music;
   begin
      return Result /= 0;
   end Is_Paused;

   ------------
   -- Fading --
   ------------

   function Fading return Fading_Type
   is
      function Mix_Fading_Music return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadingMusic";
      Result : constant C.int := Mix_Fading_Music;
   begin
      return Fading_Type'Val (Result);
   end Fading;


end SDL.Mixer.Music;
