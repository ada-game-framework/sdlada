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
with Ada.Text_IO; use Ada.Text_IO;

package body Utils is
   procedure Comment_Dash (Total : in Positive; Indent : in Natural := 0; New_Line : Boolean := True) is
   begin
      if Indent > 0 then
         for Index in 1 .. Indent loop
            Put (' ');
         end loop;
      end if;

      for Index in 1 .. Total loop
         Put ('-');
      end loop;

      if New_Line then
         Ada.Text_IO.New_Line;
      end if;
   end Comment_Dash;

   procedure Comment (Indent : in Natural; Text : in String; New_Line : in Boolean := True) is
   begin
      if Indent > 0 then
         for Index in 1 .. Indent loop
            Put (' ');
         end loop;
      end if;

      if Text = "" then
         Put ("--");
      else
         Put ("--  " & Text);
      end if;

      if New_Line then
         Ada.Text_IO.New_Line;
      end if;
   end Comment;

   procedure Output_Field (Text      : in String;
                           Width     : in Integer;
                           Indent    : in Natural;
                           Separator : in String := " ";
                           Truncate  : in Boolean := False) is
      Field_Index : constant Positive_Count := Col;
   begin
      if Indent > 0 then
         for Index in 1 .. Indent loop
            Put (' ');
         end loop;
      end if;

      if Text'Length + Indent > Width and Truncate then
         Put (Text (Text'First .. Width) & Separator);
      else
         Put (Text & Separator);
      end if;

      Set_Col (Field_Index + Positive_Count (Width + Indent));
   end Output_Field;
end Utils;
