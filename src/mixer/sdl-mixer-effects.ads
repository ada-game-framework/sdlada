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
--  SDL.Mixer.Effects
--------------------------------------------------------------------------------------------------------------------

with SDL.Mixer.Channels;

package SDL.Mixer.Effects is
   pragma Preelaborate;

   use SDL.Mixer.Channels;

   type Effect_Function_Access is
     access procedure (Channel : in C.int;
                       Stream  : in C.int;
                       Len     : in C.int;
                       Udata   : in C.int)
     with Convention => C;

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
   procedure Unregister (Channel : in Channel_Index;
                         Effect  : in Effect_Function_Access);
   procedure Unregister_All (Channel : in Channel_Index);
   procedure Set_Post_Mix (Mix_Function : in Mix_Function_Access; Argument : in Integer);
   procedure Set_Panning (Channel : in Channel_Index; Left, Right : in Volumen_Type);
   procedure Set_Distance (Channel : in Channel_Index; Distance : in Distance_Type);
   procedure Set_Position (Channel  : in Channel_Index;
                           Angle    : in Angle_Type;
                           Distance : in Distance_Type);
   procedure Set_Reverse_Stereo (Channel : in Channel_Index; Flip : in Boolean);

end SDL.Mixer.Effects;
