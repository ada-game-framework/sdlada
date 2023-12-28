--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Mixer.Effects
--------------------------------------------------------------------------------------------------------------------

with SDL.Mixer.Channels;

package SDL.Mixer.Effects is

   use SDL.Mixer.Channels;

   Mix_Channel_Post : constant Channel_Index;

   type Effect_Function_Access is
     access procedure (Channel : in C.int;
                       Stream  : in C.int;
                       Len     : in C.int;
                       Udata   : in C.int)
     with Convention => C;
   --  This is the format of a special effect callback:
   --
   --  Channel is the channel number that your effect is affecting. Stream is
   --  the buffer of data to work upon. Len is the size of Stream, and
   --  Udata is a user-defined bit of data, which you pass as the last arg of
   --  SDL.Mixer.Effects.Register, and is passed back unmolested to your
   --  callback. Your effect changes the contents of Stream based on whatever
   --  parameters are significant, or just leaves it be, if you prefer. You can
   --  do whatever you like to the buffer, though, and it will continue in its
   --  changed state down the mixing pipeline, through any other effect
   --  functions, then finally to be mixed with the rest of the channels and
   --  music for the final output stream.
   --
   --  Do never call SDL.Audio.Lock from your callback function!

   type Effect_Done_Access is
     access procedure (Channel : in C.int;
                       Udata   : in C.int)
     with Convention => C;

   type Mix_Function_Access is
     access procedure (Stream  : in C.int;
                       Len     : in C.int;
                       Udata   : in C.int)
     with Convention => C;

   type Volumen_Type  is new Interfaces.Unsigned_8;
   type Distance_Type is new Interfaces.Unsigned_8;
   type Angle_Type    is new Interfaces.Integer_16;

   procedure Register (Channel  : in Channel_Index;
                       Effect   : in Effect_Function_Access;
                       Done     : in Effect_Done_Access;
                       Argument : in Integer);
   --  Register a special effect function. At mixing time, the channel data is
   --  copied into a buffer and passed through each registered effect function.
   --  After it passes through all the functions, it is mixed into the final
   --  output stream. The copy to buffer is performed once, then each effect
   --  function performs on the output of the previous effect. Understand that
   --  this extra copy to a buffer is not performed if there are no effects
   --  registered for a given chunk, which saves CPU cycles, and any given
   --  effect will be extra cycles, too, so it is crucial that your code run
   --  fast. Also note that the data that your function is given is in the
   --  format of the sound device, and not the format you gave to
   --  SDL.Mixer.Open, although they may in reality be the same. This is an
   --  unfortunate but necessary speed concern. Use SDL.Mixer.Query_Spec to
   --  determine if you can handle the data before you register your effect,
   --  and take appropriate actions.
   --  You may also specify a callback (Effect_Done_Access) that is called when
   --  the channel finishes playing. This gives you a more fine-grained control
   --  than SDL.Mixer.Channels.Finished, in case you need to free effect-
   --  specific resources, etc. If you don't need this, you can specify Null.
   --  You may set the callbacks before or after calling
   --  SDL.Mixer.Channels.Play. Things like SDL.Mixer.Set_Panning are just
   --  internal special effect functions, so if you are using that, you've
   --  already incurred the overhead of a copy to a separate buffer, and that
   --  these effects will be in the queue with any functions you've registered.
   --  The list of registered effects for a channel is reset when a chunk
   --  finishes playing, so you need to explicitly set them with each call to
   --  SDL.Mixer.Channels.Play.
   --  You may also register a special effect function that is to be run after
   --  final mixing occurs. The rules for these callbacks are identical to
   --  those in SDL.Mixer.Effects.Register, but they are run after all the
   --  channels and the music have been mixed into a single stream, whereas
   --  channel-specific effects run on a given channel before any other mixing
   --  occurs. These global effect callbacks are call "posteffects".
   --  Posteffects only have their Effect_Done_Access function called when they
   --  are unregistered (since the main output stream is never "done" in the
   --  same sense as a channel). You must unregister them manually when you've
   --  had enough. Your callback will be told that the channel being mixed is
   --  (Mix_Channel_Post) if the processing is considered a posteffect.
   --
   --  After all these effects have finished processing, the callback
   --  registered through Set_Post_Mix runs, and then the stream goes to the
   --  audio device.
   --
   --  Do never call SDL.Audio.Lock from your callback function!
   --  Raises Mixer_Error if error (no such channel).

   procedure Unregister (Channel : in Channel_Index;
                         Effect  : in Effect_Function_Access);
   --  You may not need to call this explicitly, unless you need to stop an
   --  effect from processing in the middle of a chunk's playback.
   --  Posteffects are never implicitly unregistered as they are for channels,
   --  but they may be explicitly unregistered through this function by
   --  specifying Mix_Channel_Post for a channel.
   --  Raises Mixer_Error if error (no such channel or effect).

   procedure Unregister_All (Channel : in Channel_Index);
   --  You may not need to call this explicitly, unless you need to stop all
   --  effects from processing in the middle of a chunk's playback. Note that
   --  this will also shut off some internal effect processing, since
   --  Set_Panning and others may use this API under the hood. This is called
   --  internally when a channel completes playback.
   --  Posteffects are never implicitly unregistered as they are for channels,
   --  but they may be explicitly unregistered through this function by
   --  specifying Mix_Channel_Post for a channel.
   --  Raises Mixer_Error if error (no such channel).

   procedure Set_Post_Mix (Mix_Function : in Mix_Function_Access;
                           Argument     : in Integer);
   --  Set a function that is called after all mixing is performed.
   --  This can be used to provide real-time visual display of the audio stream
   --  or add a custom mixer filter for the stream data.
   --  Raises Mixer_Error if error.

   procedure Set_Panning (Channel     : in Channel_Index;
                          Left, Right : in Volumen_Type);
   --  Set the panning of a channel. The left and right channels are specified
   --  as integers between 0 and 255, quietest to loudest, respectively.
   --
   --  Technically, this is just individual volume control for a sample with
   --  two (stereo) channels, so it can be used for more than just panning.
   --  If you want real panning, call it like this:
   --
   --  Set_Panning (Channel, Left, 255 - Left);
   --
   --  ...which isn't so hard.
   --
   --  Setting Channel to Mix_Channel_Post registers this as a posteffect, and
   --  the panning will be done to the final mixed stream before passing it on
   --  to the audio device.
   --
   --  This uses the SDL.Mixer.Effects.Register API internally, and returns
   --  without registering the effect function if the audio device is not
   --  configured for stereo output. Setting both (left) and (right) to 255
   --  causes this effect to be unregistered, since that is the data's normal
   --  state.
   --
   --  Raises Mixer_Error if error (no such channel or
   --  SDL.Mixer.Effects.Register fails). Note that an audio device in mono
   --  mode is a no-op, but this call will return successful in that case.

   procedure Set_Distance (Channel  : in Channel_Index;
                           Distance : in Distance_Type);
   --  Set the "distance" of a channel. Distance is an integer from 0 to 255
   --  that specifies the location of the sound in relation to the listener.
   --  Distance 0 is overlapping the listener, and 255 is as far away as possible
   --  A distance of 255 does not guarantee silence; in such a case, you might
   --  want to try changing the chunk's volume, or just cull the sample from the
   --  mixing process with SDL.Mixer.Channels.Halt.
   --  For efficiency, the precision of this effect may be limited (distances 1
   --  through 7 might all produce the same effect, 8 through 15 are equal, etc).
   --  Distance is an integer between 0 and 255 that specifies the space
   --  between the sound and the listener. The larger the number, the further
   --  away the sound is.
   --  Setting Distance to 0 unregisters this effect, since the data would be
   --  unchanged.
   --  If you need more precise positional audio, consider using OpenAL for
   --  spatialized effects instead of SDL_mixer. This is only meant to be a
   --  basic effect for simple "3D" games.
   --
   --  Setting Channel to Mix_Channel_Post registers this as a posteffect, and
   --  the distance attenuation will be done to the final mixed stream before
   --  passing it on to the audio device.
   --
   --  This uses the SDL.Mixer.Effects.Register API internally.
   --
   --  Raises Mixer_Error if error (no such channel or
   --  SDL.Mixer.Effects.Register fails).

   procedure Set_Position (Channel  : in Channel_Index;
                           Angle    : in Angle_Type;
                           Distance : in Distance_Type);
   --  Set the position of a channel. Angle is an integer from 0 to 360, that
   --  specifies the location of the sound in relation to the listener. Angle
   --  will be reduced as neccesary (540 becomes 180 degrees, -100 becomes
   --  260). Angle 0 is due north, and rotates clockwise as the value
   --  increases. For efficiency, the precision of this effect may be limited
   --  Angles 1 through 7 might all produce the same effect, 8 through 15 are
   --  equal, etc).
   --  Distance is an integer between 0 and 255 that specifies the space
   --  between the sound and the listener. The larger the number, the further
   --  away the sound is. Using 255 does not guarantee that the channel will be
   --  culled from the mixing process or be completely silent. For efficiency,
   --  the precision of this effect may be limited (Distance 0 through 5 might
   --  all produce the same effect, 6 through 10 are equal, etc). Setting Angle
   --  and Distance to 0 unregisters this effect, since the data would be
   --  unchanged.
   --
   --  If you need more precise positional audio, consider using OpenAL for
   --  spatialized effects instead of SDL2_mixer. This is only meant to be a
   --  basic effect for simple "3D" games.
   --
   --  If the audio device is configured for mono output, then you won't get
   --  any effectiveness from the angle; however, distance attenuation on the
   --  channel will still occur. While this effect will function with stereo
   --  voices, it makes more sense to use voices with only one channel of
   --  sound, so when they are mixed through this effect, the positioning will
   --  sound correct. You can convert them to mono through SDL before giving
   --  them to the mixer in the first place if you like.
   --
   --  Setting (channel) to Mix_Channel_Post registers this as a posteffect,
   --  and the positioning will be done to the final mixed stream before
   --  passing it on to the audio device.
   --
   --  This is a convenience wrapper over Set_Distance and Set_Panning.
   --
   --  Raises Mixer_Error if error (no such channel or
   --  SDL.Mixer.Effects.Register fails).

   procedure Set_Reverse_Stereo (Channel : in Channel_Index;
                                 Flip    : in Boolean);
   --  Causes a channel to reverse its stereo. This is handy if the user has his
   --  speakers hooked up backwards, or you would like to have a minor bit of
   --  psychedelia in your sound code.  :)  Calling this function with (flip)
   --  set to non-zero reverses the chunks's usual channels. If (flip) is zero,
   --  the effect is unregistered.
   --
   --  This uses the SDL.Mixer.Effects.Register API internally, and thus is
   --  probably more CPU intensive than having the user just plug in his
   --  speakers correctly. Set_Reverse_Stereo returns without registering the
   --  effect function if the audio device is not configured for stereo output.
   --
   --  If you specify Mix_Channel_Post for Channel, then this the effect is
   --  used on the final mixed stream before sending it on to the audio device
   --  (a posteffect).
   --
   --  Raises Mixer_Error if error (no such channel or
   --  SDL.Mixer.Effects.Register fails).
   --  Note that an audio device in mono mode is a no-op, but this call will
   --  return successful in that case.

private

   Mix_Channel_Post : constant Channel_Index := -2;

end SDL.Mixer.Effects;
