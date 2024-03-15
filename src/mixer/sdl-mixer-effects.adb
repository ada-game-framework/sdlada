--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;

package body SDL.Mixer.Effects is
   procedure Register (Channel  : in Ch.Channel_Index;
                       Effect   : in Effect_Function_Access;
                       Done     : in Effect_Done_Access;
                       Argument : in Integer) is
      function Mix_Register_Effect (Channel  : in C.int;
                                    Effect   : in Effect_Function_Access;
                                    Done     : in Effect_Done_Access;
                                    Arg      : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_RegisterEffect";

   begin
      if Mix_Register_Effect (C.int (Channel), Effect, Done, C.int (Argument)) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Register;


   procedure Unregister (Channel : in Ch.Channel_Index;
                         Effect  : in Effect_Function_Access) is
      function Mix_Unregister_Effect (Channel  : in C.int;
                                      Effect   : in Effect_Function_Access) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_UnregisterEffect";
   begin
      if Mix_Unregister_Effect (C.int (Channel), Effect) /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Unregister;


   procedure Unregister_All (Channel : in Ch.Channel_Index) is
      function Mix_Unregister_All_Effects (Channel  : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_UnregisterAllEffects";
   begin
      if Mix_Unregister_All_Effects (C.int (Channel)) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Unregister_All;


   procedure Set_Post_Mix (Mix_Function : in Mix_Function_Access; Argument : in Integer) is
      function Mix_Set_Post_Mix (Mix_Func : in Mix_Function_Access; Argument : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetPostMix";
   begin
      if Mix_Set_Post_Mix (Mix_Function, C.int (Argument)) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Post_Mix;


   procedure Set_Panning (Channel : in Ch.Channel_Index; Left, Right : in Volumen_Type) is
      use Interfaces;
      function Mix_Set_Panning (Channel : in C.int;
                                Left    : in Unsigned_8;
                                Right   : in Unsigned_8) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetPanning";

      Result : constant C.int := Mix_Set_Panning (Channel => C.int (Channel),
                                                  Left    => Unsigned_8 (Left),
                                                  Right   => Unsigned_8 (Right));
   begin
      if Result /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Panning;


   procedure Set_Distance (Channel : in Ch.Channel_Index; Distance : in Distance_Type) is
      use Interfaces;
      function Mix_Set_Distance (Channel  : in C.int; Distance : in Unsigned_8) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetDistance";
   begin
      if Mix_Set_Distance (C.int (Channel), Unsigned_8 (Distance)) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Distance;


   procedure Set_Position (Channel  : in Ch.Channel_Index;
                           Angle    : in Angle_Type;
                           Distance : in Distance_Type) is
      use Interfaces;
      function Mix_Set_Position (Channel  : in C.int;
                                 Angle    : in Integer_16;
                                 Distance : in Unsigned_8) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetPosition";

      Result : constant C.int := Mix_Set_Position (Channel  => C.int (Channel),
                                                   Angle    => Integer_16 (Angle),
                                                   Distance => Unsigned_8 (Distance));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Position;


   procedure Reverse_Stereo (Channel : in Ch.Channel_Index; Flip : in Boolean) is
      function Mix_Set_Reverse_Stereo (Channel : in C.int; Flip : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetReverseStereo";
   begin
      if Mix_Set_Reverse_Stereo (C.int (Channel), Boolean'Pos (Flip)) /= Success then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Reverse_Stereo;
end SDL.Mixer.Effects;
