--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
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
