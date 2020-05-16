--------------------------------------------------------------------------------------------------------------------
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

package body SDL.Mixer.Groups is

   ----------------------
   -- Reverse_Channels --
   ----------------------

   procedure Reserve_Channels (Desired_Channels : in     Natural;
                               Got_Channels     :    out Natural)
   is
      function Mix_Reserve_Channels (Num : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_ReserveChannels";
      Got : constant C.int := Mix_Reserve_Channels (C.int (Desired_Channels));
   begin
      Got_Channels := Natural (Got);
   end Reserve_Channels;

   ----------------------
   -- Channel_To_Group --
   ----------------------

   procedure Channel_To_Group (Channel : in Channel_Index; Group : in Group_Number)
   is
      function Mix_Group_Channel (Which : in C.int;
                                  Tag   : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupChannel";
      Status : constant C.int := Mix_Group_Channel (C.int (Channel),
                                                    C.int (Group));
   begin
      if Status /= 1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Channel_To_Group;

   -----------------------
   -- Channels_To_Group --
   -----------------------

   procedure Channels_To_Group (From, To : in Channel_Index; Group : in Group_Number)
   is
      function Mix_Group_Channels (From : in C.int;
                                   To   : in C.int;
                                   Tag  : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupChannels";
      Channel_Count : constant C.int := Mix_Group_Channels (C.int (From),
                                                            C.int (To),
                                                            C.int (Group));
      pragma Unreferenced (Channel_Count);
   begin
      null;
   end Channels_To_Group;

   -----------
   -- Count --
   -----------

   function Count (Group : in Group_Number) return Natural
   is
      function Mix_Group_Count (Tag : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupCount";
      Channel_Count : constant C.int := Mix_Group_Count (C.int (Group));
   begin
      return Natural (Channel_Count);
   end Count;

   ---------------
   -- Available --
   ---------------

   function Available (Group : in Group_Number) return Channel_Index
   is
      function Mix_Group_Available (Tag : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupAvailable";
      Channel_Found : constant C.int := Mix_Group_Available (C.int (Group));
   begin
      if Channel_Found = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      return Channel_Index (Channel_Found);
   end Available;

   ------------
   -- Oldest --
   ------------

   function Oldest (Group : in Group_Number) return Channel_Index
   is
      function Mix_Group_Oldest (Tag : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupOldest";
      Channel_Found : constant C.int := Mix_Group_Oldest (C.int (Group));
   begin
      if Channel_Found = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      return Channel_Index (Channel_Found);
   end Oldest;

   -----------
   -- Newer --
   -----------

   function Newer (Group : in Group_Number) return Channel_Index
   is
      function Mix_Group_Newer (Tag : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupNewer";
      Channel_Found : constant C.int := Mix_Group_Newer (C.int (Group));
   begin
      if Channel_Found = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      return Channel_Index (Channel_Found);
   end Newer;

   --------------
   -- Fade_Out --
   --------------

   procedure Fade_Out (Group : in Group_Number; Ms : in Integer)
   is
      function Mix_Fade_Out_Group (Tag : in C.int;
                                   Ms  : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeOutGroup";
      Channel_Count : constant C.int := Mix_Fade_Out_Group (C.int (Group),
                                                            C.int (Ms));
      pragma Unreferenced (Channel_Count);
   begin
      null;
   end Fade_Out;

   ----------
   -- Halt --
   ----------

   procedure Halt (Group : in Group_Number)
   is
      function Mix_Halt_Group (Tag : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HaltGroup";
      Dummy : constant C.int := Mix_Halt_Group (C.int (Group));
      pragma Unreferenced (Dummy);
   begin
      null;
   end Halt;


end SDL.Mixer.Groups;
