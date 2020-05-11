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
--  SDL.Mixer.Channels
--------------------------------------------------------------------------------------------------------------------

package SDL.Mixer.Channels is
   pragma Preelaborate;

   type Channel_Index is new Integer;
   All_Channels : constant Channel_Index := -1;

   type Loop_Count is new Integer;
   Loop_Forever : constant Loop_Count := -1;

   type Time_Ms     is new Integer;
   type Ticks_Count is new Integer;

   procedure Allocate (Channel_Count : in Positive);
   procedure Volume (Channel : in Channel_Index; New_Volume : in Volume_Type);
   procedure Play (Channel : in Channel_Index; Chunk : in Chunk_Type; Loops : in Loop_Count);
   procedure Play_Timed (Channel : in Channel_Index;
                         Chunk   : in Chunk_Type;
                         Loops   : in Loop_Count;
                         Ticks   : in Ticks_Count);
   procedure Fade_In (Channel : in Channel_Index;
                      Chunk   : in Chunk_Type;
                      Loops   : in Loop_Count;
                      Ms      : in Time_Ms);
   procedure Fade_In_Timed (Channel : in Channel_Index;
                            Chunk   : in Chunk_Type;
                            Loops   : in Loop_Count;
                            Ms      : in Time_Ms;
                            Ticks   : in Ticks_Count);
   procedure Pause (Channel : in Channel_Index);
   procedure Resume (Channel : in Channel_Index);
   procedure Halt (Channel : in Channel_Index);
   procedure Expire (Channel : in Channel_Index; Ticks : in Ticks_Count);
   procedure Fade_Out (Channel : in Channel_Index; Ms : in Time_Ms);
--   procedure Finished;
   function Is_Playing (Channel : in Channel_Index) return Boolean;
   function Playing_Count return Natural;
   function Is_Paused (Channel : in Channel_Index) return Boolean;
   function Paused_Count return Natural;
   function Fading (Channel : in Channel_Index) return Fading_Type;
   function Get_Chunk (Channel : in Channel_Index) return Chunk_Type;

end SDL.Mixer.Channels;
