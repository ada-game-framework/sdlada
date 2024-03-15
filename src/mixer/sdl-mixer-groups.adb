--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;

package body SDL.Mixer.Groups is
   procedure Reserve_Channels (Desired_Channels : in     Natural;
                               Got_Channels     :    out Natural) is
      function Mix_Reserve_Channels (Num : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_ReserveChannels";
   begin
      Got_Channels := Natural (Mix_Reserve_Channels (C.int (Desired_Channels)));
   end Reserve_Channels;


   procedure Channel_To_Group (Channel : in Ch.Channel_Index; Group : in Group_Number) is
      function Mix_Group_Channel (Which : in C.int;
                                  Tag   : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupChannel";

      Status : constant C.int := Mix_Group_Channel (C.int (Channel), C.int (Group));
   begin
      if Status /= 1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Channel_To_Group;


   procedure Channels_To_Group (From, To : in Ch.Channel_Index; Group : in Group_Number) is
      function Mix_Group_Channels (From : in C.int;
                                   To   : in C.int;
                                   Tag  : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupChannels";

      Channel_Count : constant C.int := Mix_Group_Channels (C.int (From), C.int (To), C.int (Group));
   begin
      if Channel_Count = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;
   end Channels_To_Group;


   function Count (Group : in Group_Number) return Natural is
      function Mix_Group_Count (Tag : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupCount";
   begin
      return Natural (Mix_Group_Count (C.int (Group)));
   end Count;


   function Available (Group : in Group_Number) return Ch.Channel_Index is
      function Mix_Group_Available (Tag : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupAvailable";

      Channel_Found : constant C.int := Mix_Group_Available (C.int (Group));
   begin
      if Channel_Found = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      return Ch.Channel_Index (Channel_Found);
   end Available;


   function Oldest (Group : in Group_Number) return Ch.Channel_Index is
      function Mix_Group_Oldest (Tag : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupOldest";

      Channel_Found : constant C.int := Mix_Group_Oldest (C.int (Group));
   begin
      if Channel_Found = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      return Ch.Channel_Index (Channel_Found);
   end Oldest;


   function Newer (Group : in Group_Number) return Ch.Channel_Index is
      function Mix_Group_Newer (Tag : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GroupNewer";

      Channel_Found : constant C.int := Mix_Group_Newer (C.int (Group));
   begin
      if Channel_Found = -1 then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      return Ch.Channel_Index (Channel_Found);
   end Newer;


   procedure Fade_Out (Group : in Group_Number; Ms : in Integer) is
      function Mix_Fade_Out_Group (Tag : in C.int;
                                   Ms  : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FadeOutGroup";

      Channel_Count : constant C.int := Mix_Fade_Out_Group (C.int (Group), C.int (Ms));
      pragma Unreferenced (Channel_Count);
   begin
      null;
   end Fade_Out;


   procedure Halt (Group : in Group_Number) is
      function Mix_Halt_Group (Tag : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HaltGroup";

      Dummy : constant C.int := Mix_Halt_Group (C.int (Group));
      pragma Unreferenced (Dummy);
   begin
      null;
   end Halt;
end SDL.Mixer.Groups;
