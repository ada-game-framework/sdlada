--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2020 Jesper Quorning
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

with SDL.Error;

package body SDL.Mixer.Effects is


   procedure Register (Channel  : in Channel_Index;
                       Effect   : in Effect_Function_Access;
                       Done     : in Effect_Done_Access;
                       Argument : in Integer)
   is
      function Mix_Register_Effect (Channel  : in C.int;
                                    Effect   : in Effect_Function_Access;
                                    Done     : in Effect_Done_Access;
                                    Arg      : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_RegisterEffect";
      Result : constant C.int := Mix_Register_Effect (C.int (Channel),
                                                      Effect, Done,
                                                      C.int (Argument));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Register;


   procedure Unregister (Channel : in Channel_Index;
                         Effect  : in Effect_Function_Access)
   is
      function Mix_Unregister_Effect (Channel  : in C.int;
                                      Effect   : in Effect_Function_Access) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_UnregisterEffect";
      Result : constant C.int := Mix_Unregister_Effect (C.int (Channel),
                                                        Effect);
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Unregister;


   procedure Unregister_All (Channel : in Channel_Index)
   is
      function Mix_Unregister_All_Effects (Channel  : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_UnregisterAllEffects";
      Result : constant C.int := Mix_Unregister_All_Effects (C.int (Channel));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Unregister_All;


   procedure Set_Post_Mix (Mix_Function : in Mix_Function_Access; Argument : in Integer)
   is
      function Mix_Set_Post_Mix (Mix_Func : in Mix_Function_Access;
                                 Argument : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetPostMix";
      Result : constant C.int := Mix_Set_Post_Mix (Mix_Function, C.int (Argument));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Post_Mix;


   procedure Set_Panning (Channel : in Channel_Index; Left, Right : in Volumen_Type)
   is
      use Interfaces;
      function Mix_Set_Panning (Channel : in C.int;
                                Left    : in Unsigned_8;
                                Right   : in Unsigned_8) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetPanning";
      Result : constant C.int := Mix_Set_Panning (Channel => C.int (Channel),
                                                  Left    => Unsigned_8 (Left),
                                                  Right   => Unsigned_8 (Right));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Panning;


   procedure Set_Distance (Channel : in Channel_Index; Distance : in Distance_Type)
   is
      use Interfaces;
      function Mix_Set_Distance (Channel  : in C.int;
                                 Distance : in Unsigned_8) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetDistance";
      Result : constant C.int := Mix_Set_Distance (C.int (Channel), Unsigned_8 (Distance));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Distance;


   procedure Set_Position (Channel  : in Channel_Index;
                           Angle    : in Angle_Type;
                           Distance : in Distance_Type)
   is
      use Interfaces;
      function Mix_Set_Position (Channel  : in C.int;
                                 Angle    : in Integer_16;
                                 Distance : in Unsigned_8) return C.int
        with
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


   procedure Set_Reverse_Stereo (Channel : in Channel_Index; Flip : in Boolean)
   is
      function Mix_Set_Reverse_Stereo (Channel : in C.int;
                                       Flip    : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_SetReverseStereo";
      Result : constant C.int := Mix_Set_Reverse_Stereo (C.int (Channel),
                                                         Boolean'Pos (Flip));
   begin
      if Result /= 0 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Set_Reverse_Stereo;


end SDL.Mixer.Effects;
