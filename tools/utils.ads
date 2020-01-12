--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020,  Luke A. Guest
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
--  Utils
--------------------------------------------------------------------------------------------------------------------
package Utils is
   procedure Comment_Dash (Total : in Positive; Indent : in Natural := 0; New_Line : Boolean := True);

   procedure Comment (Indent : in Natural; Text : in String; New_Line : in Boolean := True);

   procedure Output_Field (Text      : in String;
                           Width     : in Integer;
                           Indent    : in Natural;
                           Separator : in String := " ";
                           Truncate  : in Boolean := False);
end Utils;
