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
--  SDL.Mixer.Groups
--------------------------------------------------------------------------------------------------------------------

with SDL.Mixer.Channels;

package SDL.Mixer.Groups is
   pragma Preelaborate;

   use SDL.Mixer.Channels;

   type Group_Number is new Integer;
   All_Group : constant Group_Number := -1;

   procedure Reserve_Channels (Desired_Channels : in     Natural;
                               Got_Channels     :    out Natural);
   procedure Channel_To_Group (Channel : in Channel_Index; Group : in Group_Number);
   procedure Channels_To_Group (From, To : in Channel_Index; Group : in Group_Number);
   function Count (Group : in Group_Number) return Natural;
   function Available (Group : in Group_Number) return Channel_Index;
   function Oldest (Group : in Group_Number) return Channel_Index;
   function Newer (Group : in Group_Number) return Channel_Index;
   procedure Fade_Out (Group : in Group_Number; Ms : in Integer);
   procedure Halt (Group : in Group_Number);

end SDL.Mixer.Groups;
