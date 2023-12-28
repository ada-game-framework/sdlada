--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Mixer.Groups
--------------------------------------------------------------------------------------------------------------------

with SDL.Mixer.Channels;

package SDL.Mixer.Groups is

   use SDL.Mixer.Channels;

   type Group_Number is new Integer;
   All_Group : constant Group_Number := -1;

   procedure Reserve_Channels (Desired_Channels : in     Natural;
                               Got_Channels     :    out Natural);
   --  Reserve the first channels (0 -> n-1) for the application, i.e. don't
   --  allocate them dynamically to the next sample if requested with a -1
   --  value below. Returns the number of reserved channels.

   procedure Channel_To_Group (Channel : in Channel_Index;
                               Group   : in Group_Number);
   --  Assign several consecutive channels to a group

   procedure Channels_To_Group (From, To : in Channel_Index;
                                Group    : in Group_Number);
   --  Finds the first available channel in a group of channels.
   --  Raising Mixer_Error if none are available.

   function Count (Group : in Group_Number) return Natural;
   --  Finds the "oldest" sample playing in a group of channels

   function Available (Group : in Group_Number) return Channel_Index;
   --  Returns the number of channels in a group. This is also a subtle
   --  way to get the total number of channels when 'tag' is -1

   function Oldest (Group : in Group_Number) return Channel_Index;

   function Newer (Group : in Group_Number) return Channel_Index;
   --  Finds the "most recent" (i.e. last) sample playing in a group of
   --  channels

   procedure Fade_Out (Group : in Group_Number;
                       Ms    : in Integer);
   --  Halt a channel, fading it out progressively till it's silent
   --  The Ms parameter indicates the number of milliseconds the fading
   --  will take.

   procedure Halt (Group : in Group_Number);
   --  Halt playing of a particular channel

end SDL.Mixer.Groups;
