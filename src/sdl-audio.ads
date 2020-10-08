--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2020, Luke A. Guest
--  Contributed by Vinzent "Jellix" Saranen
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Audio
--
--  Simple audio functionality.
--------------------------------------------------------------------------------------------------------------------

with System;

package SDL.Audio is

   type Audio_Buffer  is new System.Address; --  C allocated buffer, see Load_WAV
   type User_Data_Ptr is new System.Address; --  Variable type.

   Null_Audio : constant Audio_Buffer;
   Null_User  : constant User_Data_Ptr;

   MAX_MIX_VOLUME : constant := 128;
   subtype Volume is Interfaces.C.int range 0 .. MAX_MIX_VOLUME;

   --  Callback prototype:
   --  User_Data is the pointer stored in User_Data field of the
   --            Audio_Spec.
   --  Stream    is a pointer to the audio buffer you want to fill with
   --            information and
   --  Length    is the length of the audio buffer in bytes.
   type Audio_Callback is access
     procedure (User_Data : in User_Data_Ptr;
                Stream    : in Audio_Buffer;
                Length    : in Interfaces.C.int);
   pragma Convention (Convention => C,
                      Entity     => Audio_Callback);

   type Format_Id is new Interfaces.Unsigned_16;

   Unsigned_8     : constant Format_Id := 16#0008#;
   Unsigned_16_LE : constant Format_Id := 16#0010#;
   Unsigned_16_BE : constant Format_Id := 16#1010#;
   Signed_8       : constant Format_Id := 16#8008#;
   Signed_16_LE   : constant Format_Id := 16#8010#;
   Signed_16_BE   : constant Format_Id := 16#9010#;

   type Status is (Stopped, Playing, Paused);
   pragma Convention (Convention => C,
                      Entity     => Status);

   type Audio_Spec is
      record
         Frequency : Interfaces.C.int;
         Format    : Format_Id;
         Channels  : Interfaces.Unsigned_8;
         Silence   : Interfaces.Unsigned_8;
         Samples   : Interfaces.Unsigned_16;
         Padding   : Interfaces.Unsigned_16;
         Size      : Interfaces.Unsigned_32;
         Callback  : Audio_Callback;
         User_Data : User_Data_Ptr;
      end record;
   pragma Convention (Convention => C,
                      Entity     => Audio_Spec);

   --  Filter function prototype
   type Conversion;                         -- SDL_AudioCVT
   type Filter_Callback is access
     procedure (Conv_Table : in Conversion;
                Format     : in Format_Id);
   pragma Convention (Convention => C,
                      Entity     => Filter_Callback);

   type Filter_Callbacks is array (0 .. 9) of Filter_Callback;
   pragma Convention (Convention => C,
                      Entity     => Filter_Callbacks);

   type Bool is new Boolean with
     Size => Interfaces.C.int'Size;

   type Conversion is -- SDL_AudioCVT
      record
         Needed       : Bool;                --  True if conversion possible
         Src_Format   : Format_Id;           --  source audio format
         Dst_Format   : Format_Id;           --  target audio format
         Rate_Incr    : Interfaces.C.double; --  rate conversion increment
         Buf          : Audio_Buffer;        --  buffer to hold entire audio data
         Len          : Interfaces.C.int;    --  length of original audio buffer
         Len_Cvt      : Interfaces.C.int;    --  length of converted audio buffer
         Len_Mult     : Interfaces.C.int;    --  buffer must be len*len_mult big
         Len_Ratio    : Interfaces.C.double; --  Given len, final size is len*len_ratio
         Filters      : Filter_Callbacks;
         Filter_Index : Interfaces.C.int;    --  Current audio conversion function
      end record;
   pragma Convention (Convention => C,
                      Entity     => Conversion);

   ---------------------------------------------------------------------
   --  Open
   --
   --  Opens the audio device with the desired parameters.
   --
   --  This function opens the audio device with the Desired parameters,
   --  and returns True if successful, placing the actual hardware
   --  parameters in the structure pointed to by Obtained.
   --
   --  This function returns False if it failed to open the audio
   --  device, or couldn't set up the audio thread.
   --
   --  To open the audio device a desired Audio_Spec must be created.
   --  You must then fill this structure with your desired audio
   --  specifications:
   --
   --  Desired.Frequency:
   --    The desired audio frequency in samples-per-second.
   --
   --  Desired.Format:
   --    The desired audio format.
   --
   --  Desired.Samples:
   --    The desired size of the audio buffer in samples. This number
   --    should be a power of two, and may be adjusted by the audio
   --    driver to a value more suitable for the hardware. Good values
   --    seem to range between 512 and 8192 inclusive, depending on the
   --    application and CPU speed. Smaller values yield faster response
   --    time, but can lead to underflow if the application is doing
   --    heavy processing and cannot fill the audio buffer in time. A
   --    stereo sample consists of both right and left channels in LR
   --    ordering. Note that the number of samples is directly related
   --    to time.
   --
   --  Desired.Callback:
   --    This should be set to a function that will be called when the
   --    audio device is ready for more data. It is passed a pointer to
   --    the audio buffer, and the length in bytes of the audio buffer.
   --    This function usually runs in a separate thread, and so you
   --    should protect data structures that it accesses by calling
   --    Lock and Unlock in your code (NOT in your callback!).
   --
   --  Desired.User_Data
   --    This pointer is passed as the first parameter to the callback
   --    function.
   --
   --  Open reads these fields from the Desired Audio_Spec structure
   --  passed to the function and attempts to find an audio
   --  configuration matching your Desired.
   --
   --  The obtained Audio_Spec becomes the working specification and the
   --  Desired specification can be deleted. The data in the working
   --  specification is used when building AudioCVT's for converting
   --  loaded data to the hardware format.
   --
   --  Open calculates the Size and Silence fields for both the Desired
   --  and Obtained specifications. The Size field stores the total size
   --  of the audio buffer in bytes, while the Silence stores the value
   --  used to represent silence in the audio buffer.
   --
   --  The audio device starts out playing Silence when it's opened, and
   --  should be enabled for playing by calling Pause (False) when you
   --  are ready for your audio callback function to be called. Since
   --  the audio driver may modify the requested size of the audio
   --  buffer, you should allocate any local mixing buffers after you
   --  open the audio device.
   ---------------------------------------------------------------------
   procedure Open (Desired  : in out Audio_Spec;
                   Obtained : in out Audio_Spec;
                   Success  :    out Boolean);

   ---------------------------------------------------------------------
   --  Open
   --
   --  Opens the audio device with the desired parameters.
   --
   --  This function opens the audio device with the Required
   --  parameters, and returns True if successful. The audio data passed
   --  to the callback function will be guaranteed to be in the
   --  requested format, and will be automatically converted to the
   --  hardware audio format if necessary.
   --
   --  This function returns False if it failed to open the audio
   --  device, or couldn't set up the audio thread.
   --
   --  To open the audio device a Required Audio_Spec must be created.
   --  You must then fill this structure with your required audio
   --  specifications:
   --
   --  Required.Frequency:
   --    The required audio frequency in samples-per-second.
   --
   --  Required.Format:
   --    The required audio format.
   --
   --  Required.Samples:
   --    The required size of the audio buffer in samples. This number
   --    should be a power of two, and may be adjusted by the audio
   --    driver to a value more suitable for the hardware. Good values
   --    seem to range between 512 and 8192 inclusive, depending on the
   --    application and CPU speed. Smaller values yield faster response
   --    time, but can lead to underflow if the application is doing
   --    heavy processing and cannot fill the audio buffer in time. A
   --    stereo sample consists of both right and left channels in LR
   --    ordering. Note that the number of samples is directly related
   --    to time.
   --
   --  Required.Callback:
   --    This should be set to a function that will be called when the
   --    audio device is ready for more data. It is passed a pointer to
   --    the audio buffer, and the length in bytes of the audio buffer.
   --    This function usually runs in a separate thread, and so you
   --    should protect data structures that it accesses by calling
   --    Lock and Unlock in your code (NOT in your callback!).
   --
   --  Required.User_Data
   --    This pointer is passed as the first parameter to the callback
   --    function.
   --
   --  Open reads these fields from the Required Audio_Spec structure
   --  passed to the function and attempts to find an audio
   --  configuration matching your Required. SDL will convert from your
   --  Required audio settings to the hardware settings as it plays.
   --
   --  The Required Audio_Spec is your working specification. The data
   --  in the working specification is used when building AudioCVT's for
   --  converting loaded data to the hardware format.
   --
   --  Open calculates the Size and Silence fields for the Required
   --  specifications. The Size field stores the total size of the audio
   --  buffer in bytes, while the Silence stores the value used to
   --  represent silence in the audio buffer.
   --
   --  The audio device starts out playing Silence when it's opened, and
   --  should be enabled for playing by calling Pause (False) when you
   --  are ready for your audio callback function to be called. Since
   --  the audio driver may modify the requested size of the audio
   --  buffer, you should allocate any local mixing buffers after you
   --  open the audio device.
   ---------------------------------------------------------------------
   procedure Open (Required : in out Audio_Spec;
                   Success  :    out Boolean);

   ---------------------------------------------------------------------
   --  Pause
   --
   --  Pauses and unpauses the audio callback processing.
   --
   --  This function pauses and unpauses the audio callback processing.
   --  It should be called with Pause_On => False after opening the
   --  audio device to start playing sound. This is so you can safely
   --  initialize data for your callback function after opening the
   --  audio device.
   --
   --  Silence will be written to the audio device during the pause.
   ---------------------------------------------------------------------
   procedure Pause (Pause_On : in Bool);

   ---------------------------------------------------------------------
   --  Get_Status
   --
   --  Gets the current audio state.
   ---------------------------------------------------------------------
   function Get_Status return Status;

   ---------------------------------------------------------------------
   --  Load_WAV
   --
   --  Load a WAVE file.
   --
   --  This function loads a WAVE file into memory.
   --  If this function succeeds, it returns the given Audio_Spec,
   --  filled with the audio data format of the wave data, and sets
   --  Audio_Buf to a malloc'd buffer containing the audio data, and
   --  sets Audio_Len to the length of that audio buffer, in bytes. You
   --  need to free the audio buffer with Free_WAV when you are done
   --  with it.
   --
   --  This function returns NULL and sets the SDL error message if the
   --  wave file cannot be opened, uses an unknown data format, or is
   --  corrupt. Currently raw, MS-ADPCM and IMA-ADPCM WAVE files are
   --  supported.
   ---------------------------------------------------------------------
   procedure Load_WAV (File_Name : in     String;
                       Spec      :    out Audio_Spec;
                       Audio_Buf :    out Audio_Buffer;
                       Audio_Len :    out Interfaces.Unsigned_32;
                       Success   :    out Boolean);

   ---------------------------------------------------------------------
   --  Free_WAV
   --
   --  Frees previously opened WAV data.
   --
   --  After a WAVE file has been opened with Load_WAV its data can
   --  eventually be freed with Free_WAV. Audio_Buf is a pointer to the
   --  buffer created by Load_WAV.
   ---------------------------------------------------------------------
   procedure Free_WAV (Audio_Buf : in out Audio_Buffer);

   ---------------------------------------------------------------------
   --  Build_CVT
   --
   --  Initializes a Conversion structure for conversion
   --
   --  Before a Conversion structure can be used to convert audio data
   --  it must be initialized with source and destination information.
   --
   --  Src_Format and Dst_Format are the source and destination format
   --  of the conversion (for information on audio formats see
   --  Audio_Spec). Src_Channels and Dst_Channels are the number of
   --  channels in the source and destination formats. Finally, Src_Rate
   --  and Dst_Rate are the frequency or samples-per-second of the
   --  source and destination formats. Once again, see Audio_Spec.
   --
   --  Returns True if the filter could be built or False if not.
   ---------------------------------------------------------------------
   procedure Build_CVT (CVT          : in out Conversion;
                        Src_Format   : in     Format_Id;
                        Src_Channels : in     Interfaces.Unsigned_8;
                        Src_Rate     : in     Interfaces.C.int;
                        Dst_Format   : in     Format_Id;
                        Dst_Channels : in     Interfaces.Unsigned_8;
                        Dst_Rate     : in     Interfaces.C.int;
                        Success      :    out Boolean);

   ---------------------------------------------------------------------
   --  Convert
   --
   --  Convert audio data to a desired audio format.
   --
   --  Convert takes one parameter, cvt, which was previously
   --  initialized. Initializing a Conversion record is a two step
   --  process. First of all, the structure must be passed to Build_CVT
   --  along with source and destination format parameters. Secondly,
   --  the CVT.Buf and CVT.Len fields must be setup. CVT.Buf should
   --  point to the audio data and CVT.Len should be set to the length
   --  of the audio data in bytes. Remember, the length of the buffer
   --  pointed to by CVT.Buf show be CVT.Len * CVT.Len_Mult bytes in
   --  length.
   --
   --  Once the Conversion structure is initialized then we can pass it
   --  to Convert, which will convert the audio data pointer to by
   --  CVT.Buf. If Convert returned 0 then the conversion was completed
   --  successfully, otherwise -1 is returned.
   --
   --  If the conversion completed successfully then the converted audio
   --  data can be read from CVT.Buf. The amount of valid, converted,
   --  audio data in the buffer is equal to CVT.Len * CVT.Len_Ratio.
   ---------------------------------------------------------------------
   procedure Convert (CVT     : in out Conversion;
                      Success :    out Boolean);

   ---------------------------------------------------------------------
   --  Mix
   --
   --  Mix audio data
   --
   --  This function takes two audio buffers of Length bytes each of the
   --  playing audio format and mixes them, performing addition, volume
   --  adjustment, and overflow clipping. The Vol(ume) ranges from 0 to
   --  MIX_MAX_VOLUME and should be set to the maximum value for full
   --  audio volume. Note this does not change hardware volume. This is
   --  provided for convenience -- you can mix your own audio data.
   --
   --  Note: Do not use this function for mixing together more than two
   --        streams of sample data. The output from repeated
   --        application of this function may be distorted by clipping,
   --        because there is no accumulator with greater range than the
   --        input (not to mention this being an inefficient way of
   --        doing it). Use mixing functions from SDL_mixer, OpenAL, or
   --        write your own mixer instead.
   ---------------------------------------------------------------------
   procedure Mix (Destination : in Audio_Buffer;
                  Source      : in Audio_Buffer;
                  Length      : in Interfaces.Unsigned_32;
                  Vol         : in Volume);

   ---------------------------------------------------------------------
   --  Lock
   --
   --  Lock out the callback function.
   --
   --  The lock manipulated by these functions protects the callback
   --  function. During a Lock-Audio period, you can be guaranteed that
   --  the callback function is not running. Do not call these from the
   --  callback function or you will cause deadlock.
   ---------------------------------------------------------------------
   procedure Lock;

   ---------------------------------------------------------------------
   --  Unlock
   --
   --  Unlocks the callback function locked by a previous Lock call.
   ---------------------------------------------------------------------
   procedure Unlock;

   ---------------------------------------------------------------------
   --  Close
   --
   --  Shuts down audio processing and closes the audio device.
   ---------------------------------------------------------------------
   procedure Close;

private

   Null_Audio : constant Audio_Buffer  := Audio_Buffer  (System.Null_Address);
   Null_User  : constant User_Data_Ptr := User_Data_Ptr (System.Null_Address);

   pragma Import (Convention    => C,
                  Entity        => Close,
                  External_Name => "SDL_CloseAudio");

   pragma Import (Convention    => C,
                  Entity        => Get_Status,
                  External_Name => "SDL_GetAudioStatus");

   pragma Import (Convention    => C,
                  Entity        => Lock,
                  External_Name => "SDL_LockAudio");

   pragma Import (Convention    => C,
                  Entity        => Mix,
                  External_Name => "SDL_MixAudio");

   pragma Import (Convention    => C,
                  Entity        => Pause,
                  External_Name => "SDL_PauseAudio");

   pragma Import (Convention    => C,
                  Entity        => Unlock,
                  External_Name => "SDL_UnlockAudio");

end SDL.Audio;
