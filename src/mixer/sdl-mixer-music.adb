--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;
with Interfaces.C.Strings;

package body SDL.Mixer.Music is
   function Number_Of_Decoders return Natural is
      function Mix_Get_Num_Music_Decoders return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetNumMusicDecoders";
   begin
      return Natural (Mix_Get_Num_Music_Decoders);
   end Number_Of_Decoders;


   function Decoder_Name (Index : in Positive) return String is
      function Mix_Get_Music_Decoder_Name (Index : in C.int) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetMusicDecoder";
   begin
      return C.Strings.Value (Mix_Get_Music_Decoder_Name (C.int (Index - 1)));
   end Decoder_Name;


   procedure Load (Filename : in String; Music : out Music_Type) is
      function Mix_Load_MUS (Filename : in C.char_array) return Music_Type with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadMUS";

      Music_C : constant Music_Type := Mix_Load_MUS (C.To_C (Filename));
   begin
      if Music_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Music := Music_C;
   end Load;


   procedure Load (Source      : in out SDL.RWops.RWops;
                   Free_Source : in     Boolean;
                   Music       :    out Music_Type) is
      function Mix_Load_MUS_RW (Src      : in out SDL.RWops.RWops;
                                Free_Src : in     C.int) return Music_Type with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadMUS_RW";

      Music_C : constant Music_Type := Mix_Load_MUS_RW (Source, Boolean'Pos (Free_Source));
   begin
      if Music_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Music := Music_C;
   end Load;


   procedure Load (Source      : in out SDL.RWops.RWops;
                   Typ         : in     Music_Type_Type;
                   Free_Source : in     Boolean;
                   Music       :    out Music_Type) is
      function Mix_Load_MUS_Type_RW (Src      : in out SDL.RWops.RWops;
                                     Typ      : in     Music_Type_Type;
                                     Free_Src : in     C.int) return Music_Type with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadMUSType_RW";

      Music_C : constant Music_Type := Mix_Load_MUS_Type_RW (Source, Typ, Boolean'Pos (Free_Source));
   begin
      if Music_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Music := Music_C;
   end Load;


   procedure Free (Music : in out Music_Type) is
      procedure Mix_Free_Music (Music : in Music_Type) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeMusic";
   begin
      Mix_Free_Music (Music);
   end Free;


   procedure Play (Music : in Music_Type; Loops : in Loop_Count) is
      function Mix_Play_Music (Music : in Music_Type;
                               Loops : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayMusic";

      Result : constant C.int := Mix_Play_Music (Music, C.int (Loops));
   begin
      if Result /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Play;


   procedure Fade_In (Music : in Music_Type; Loops : in Loop_Count; Ms : in Integer) is
      function Mix_Fade_In_Music (Music : in Music_Type;
                                  Loops : in C.int;
                                  Ms    : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeInMusic";

      Result : constant C.int := Mix_Fade_In_Music (Music, C.int (Loops), C.int (Ms));
   begin
      if Result /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Fade_In;


   procedure Fade_In (Music    : in Music_Type;
                      Loops    : in Loop_Count;
                      Ms       : in Integer;
                      Position : in Long_Float) is
      function Mix_Fade_In_Music_Pos (Music : in Music_Type;
                                      Loops : in C.int;
                                      Ms    : in C.int;
                                      Pos   : in Long_Float) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeInMusicPos";
   begin
      if Mix_Fade_In_Music_Pos (Music, C.int (Loops), C.int (Ms), Position) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Fade_In;


   procedure Volume (New_Volume : in Volume_Type; Old_Volume : out Volume_Type) is
      function Mix_Volume_Music (Volume : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_VolumeMusic";
   begin
      Old_Volume := Volume_Type (Mix_Volume_Music (C.int (New_Volume)));
   end Volume;


   procedure Volume (New_Volume : in Volume_Type) is
      Dummy : Volume_Type;
   begin
      Volume (New_Volume, Dummy);
   end Volume;


   function Get_Volume return Volume_Type is
      function Mix_Volume_Music (Volume : in C.int) return Interfaces.Unsigned_8 with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_VolumeMusic";

      Do_Not_Set : constant C.int := -1;  --  Get's the volume, doesn't set it.
   begin
      return Volume_Type (Mix_Volume_Music (Do_Not_Set));
   end Get_Volume;


   procedure Set_Position (Position : in Long_Float) is
      function Mix_Set_Music_Position (Position : in Long_Float) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetMusicPosition";
   begin
      if Mix_Set_Music_Position (Position) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Position;


   procedure Set_Command (Command : in String) is
      function Mix_Set_Music_CMD (cmd : in C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetMusicCMD";
   begin
      if Mix_Set_Music_CMD (C.To_C (Command)) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Command;


   procedure Halt is
      function Mix_Halt_Music return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HaltMusic";

      Dummy : constant C.int := Mix_Halt_Music;
      pragma Unreferenced (Dummy);
   begin
      null;
   end Halt;


   procedure Fade_Out (Ms : in Integer) is
      function Mix_Fade_Out_Music (Ms : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeOutMusic";
   begin
      if Mix_Fade_Out_Music (Ms => C.int (Ms)) /= 1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Fade_Out;


   function Get_Type (Music : in Music_Type) return Music_Type_Type is
      function Mix_Get_Music_Type (Music : in Music_Type) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetMusicType";
   begin
      return Music_Type_Type'Val (Mix_Get_Music_Type (Music));
   end Get_Type;


   function Is_Playing return Boolean is
      function Mix_Playing_Music return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PlayingMusic";
   begin
      return Mix_Playing_Music /= Success;
   end Is_Playing;


   function Is_Paused return Boolean is
      function Mix_Paused_Music return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_PausedMusic";
   begin
      return Mix_Paused_Music /= Success;
   end Is_Paused;


   function Fading return Fading_Type is
      function Mix_Fading_Music return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadingMusic";
   begin
      return Fading_Type'Val (Mix_Fading_Music);
   end Fading;
end SDL.Mixer.Music;
