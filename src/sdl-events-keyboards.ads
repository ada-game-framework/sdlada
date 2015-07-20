--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
--  SDL.Events.Keyboards
--
--  Keyboard specific events.
--------------------------------------------------------------------------------------------------------------------
package SDL.Events.Keyboards is
   --  Keyboard events.
   Key_Down                         : constant Event_Types := 16#0000_0300#;
   Key_Up                           : constant Event_Types := Key_Down + 1;
   Text_Editing                     : constant Event_Types := Key_Down + 2;
   Text_Input                       : constant Event_Types := Key_Down + 3;

   -----------------------------------------------------------------------------------------------------------------
   --  Scan codes.
   -----------------------------------------------------------------------------------------------------------------
   type Scan_Codes is range 0 .. 512 with
     Convention => C,
     Size       => 32;

   Scan_Code_Unknown                : constant Scan_Codes := 0;

   Scan_Code_A                      : constant Scan_Codes := 4;
   Scan_Code_B                      : constant Scan_Codes := 5;
   Scan_Code_C                      : constant Scan_Codes := 6;
   Scan_Code_D                      : constant Scan_Codes := 7;
   Scan_Code_E                      : constant Scan_Codes := 8;
   Scan_Code_F                      : constant Scan_Codes := 9;
   Scan_Code_G                      : constant Scan_Codes := 10;
   Scan_Code_H                      : constant Scan_Codes := 11;
   Scan_Code_I                      : constant Scan_Codes := 12;
   Scan_Code_J                      : constant Scan_Codes := 13;
   Scan_Code_K                      : constant Scan_Codes := 14;
   Scan_Code_L                      : constant Scan_Codes := 15;
   Scan_Code_M                      : constant Scan_Codes := 16;
   Scan_Code_N                      : constant Scan_Codes := 17;
   Scan_Code_O                      : constant Scan_Codes := 18;
   Scan_Code_P                      : constant Scan_Codes := 19;
   Scan_Code_Q                      : constant Scan_Codes := 20;
   Scan_Code_R                      : constant Scan_Codes := 21;
   Scan_Code_S                      : constant Scan_Codes := 22;
   Scan_Code_T                      : constant Scan_Codes := 23;
   Scan_Code_U                      : constant Scan_Codes := 24;
   Scan_Code_V                      : constant Scan_Codes := 25;
   Scan_Code_W                      : constant Scan_Codes := 26;
   Scan_Code_X                      : constant Scan_Codes := 27;
   Scan_Code_Y                      : constant Scan_Codes := 28;
   Scan_Code_Z                      : constant Scan_Codes := 29;

   Scan_Code_1                      : constant Scan_Codes := 30;
   Scan_Code_2                      : constant Scan_Codes := 31;
   Scan_Code_3                      : constant Scan_Codes := 32;
   Scan_Code_4                      : constant Scan_Codes := 33;
   Scan_Code_5                      : constant Scan_Codes := 34;
   Scan_Code_6                      : constant Scan_Codes := 35;
   Scan_Code_7                      : constant Scan_Codes := 36;
   Scan_Code_8                      : constant Scan_Codes := 37;
   Scan_Code_9                      : constant Scan_Codes := 38;
   Scan_Code_0                      : constant Scan_Codes := 39;

   Scan_Code_Return                 : constant Scan_Codes := 40;
   Scan_Code_Escape                 : constant Scan_Codes := 41;
   Scan_Code_Backspace              : constant Scan_Codes := 42;
   Scan_Code_Tab                    : constant Scan_Codes := 43;
   Scan_Code_Space                  : constant Scan_Codes := 44;

   Scan_Code_Minus                  : constant Scan_Codes := 45;
   Scan_Code_Equals                 : constant Scan_Codes := 46;
   Scan_Code_Left_Bracket           : constant Scan_Codes := 47;
   Scan_Code_Right_Bracket          : constant Scan_Codes := 48;
   Scan_Code_Back_Slash             : constant Scan_Codes := 49;
   Scan_Code_Non_US_Hash            : constant Scan_Codes := 50;
   Scan_Code_Semi_Colon             : constant Scan_Codes := 51;
   Scan_Code_Apostrophe             : constant Scan_Codes := 52;
   Scan_Code_Grave                  : constant Scan_Codes := 53;
   Scan_Code_Comma                  : constant Scan_Codes := 54;
   Scan_Code_Period                 : constant Scan_Codes := 55;
   Scan_Code_Slash                  : constant Scan_Codes := 56;

   Scan_Code_Caps_Lock              : constant Scan_Codes := 57;

   Scan_Code_F1                     : constant Scan_Codes := 58;
   Scan_Code_F2                     : constant Scan_Codes := 59;
   Scan_Code_F3                     : constant Scan_Codes := 60;
   Scan_Code_F4                     : constant Scan_Codes := 61;
   Scan_Code_F5                     : constant Scan_Codes := 62;
   Scan_Code_F6                     : constant Scan_Codes := 63;
   Scan_Code_F7                     : constant Scan_Codes := 64;
   Scan_Code_F8                     : constant Scan_Codes := 65;
   Scan_Code_F9                     : constant Scan_Codes := 66;
   Scan_Code_F10                    : constant Scan_Codes := 67;
   Scan_Code_F11                    : constant Scan_Codes := 68;
   Scan_Code_F12                    : constant Scan_Codes := 69;

   Scan_Code_Print_Screen           : constant Scan_Codes := 70;
   Scan_Code_Scroll_Lock            : constant Scan_Codes := 71;
   Scan_Code_Pause                  : constant Scan_Codes := 72;
   Scan_Code_Insert                 : constant Scan_Codes := 73;

   Scan_Code_Home                   : constant Scan_Codes := 74;
   Scan_Code_Page_Up                : constant Scan_Codes := 75;
   Scan_Code_Delete                 : constant Scan_Codes := 76;
   Scan_Code_End                    : constant Scan_Codes := 77;
   Scan_Code_Page_Down              : constant Scan_Codes := 78;
   Scan_Code_Right                  : constant Scan_Codes := 79;
   Scan_Code_Left                   : constant Scan_Codes := 80;
   Scan_Code_Down                   : constant Scan_Codes := 81;
   Scan_Code_Up                     : constant Scan_Codes := 82;

   Scan_Code_Num_Lock_Clear         : constant Scan_Codes := 83;

   Scan_Code_KP_Divide              : constant Scan_Codes := 84;
   Scan_Code_KP_Multiply            : constant Scan_Codes := 85;
   Scan_Code_KP_Minus               : constant Scan_Codes := 86;
   Scan_Code_KP_Plus                : constant Scan_Codes := 87;
   Scan_Code_KP_Enter               : constant Scan_Codes := 88;
   Scan_Code_KP_1                   : constant Scan_Codes := 89;
   Scan_Code_KP_2                   : constant Scan_Codes := 90;
   Scan_Code_KP_3                   : constant Scan_Codes := 91;
   Scan_Code_KP_4                   : constant Scan_Codes := 92;
   Scan_Code_KP_5                   : constant Scan_Codes := 93;
   Scan_Code_KP_6                   : constant Scan_Codes := 94;
   Scan_Code_KP_7                   : constant Scan_Codes := 95;
   Scan_Code_KP_8                   : constant Scan_Codes := 96;
   Scan_Code_KP_9                   : constant Scan_Codes := 97;
   Scan_Code_KP_0                   : constant Scan_Codes := 98;
   Scan_Code_KP_Period              : constant Scan_Codes := 99;

   Scan_Code_Non_US_Back_Slash      : constant Scan_Codes := 100;
   Scan_Code_Application            : constant Scan_Codes := 101;
   Scan_Code_Power                  : constant Scan_Codes := 102;
   Scan_Code_KP_Equals              : constant Scan_Codes := 103;
   Scan_Code_F13                    : constant Scan_Codes := 104;
   Scan_Code_F14                    : constant Scan_Codes := 105;
   Scan_Code_F15                    : constant Scan_Codes := 106;
   Scan_Code_F16                    : constant Scan_Codes := 107;
   Scan_Code_F17                    : constant Scan_Codes := 108;
   Scan_Code_F18                    : constant Scan_Codes := 109;
   Scan_Code_F19                    : constant Scan_Codes := 110;
   Scan_Code_F20                    : constant Scan_Codes := 111;
   Scan_Code_F21                    : constant Scan_Codes := 112;
   Scan_Code_F22                    : constant Scan_Codes := 113;
   Scan_Code_F23                    : constant Scan_Codes := 114;
   Scan_Code_F24                    : constant Scan_Codes := 115;
   Scan_Code_Execute                : constant Scan_Codes := 116;
   Scan_Code_Help                   : constant Scan_Codes := 117;
   Scan_Code_Menu                   : constant Scan_Codes := 118;
   Scan_Code_Select                 : constant Scan_Codes := 119;
   Scan_Code_Stop                   : constant Scan_Codes := 120;
   Scan_Code_Again                  : constant Scan_Codes := 121;
   Scan_Code_Undo                   : constant Scan_Codes := 122;
   Scan_Code_Cut                    : constant Scan_Codes := 123;
   Scan_Code_Copy                   : constant Scan_Codes := 124;
   Scan_Code_Paste                  : constant Scan_Codes := 125;
   Scan_Code_Find                   : constant Scan_Codes := 126;
   Scan_Code_Mute                   : constant Scan_Codes := 127;
   Scan_Code_Volume_Up              : constant Scan_Codes := 128;
   Scan_Code_Volume_Down            : constant Scan_Codes := 129;
   --  Scan_Code_Locking_Caps_Lock   : constant Scan_Codes := 130;
   --  Scan_Code_Locking_Num_Lock    : constant Scan_Codes := 131;
   --  Scan_Code_Locking_Scroll_Lock : constant Scan_Codes := 132;
   Scan_Code_KP_Comma               : constant Scan_Codes := 133;
   Scan_Code_KP_Equals_AS400        : constant Scan_Codes := 134;

   Scan_Code_International_1        : constant Scan_Codes := 135;  --  Used on Asian keyboards.
   Scan_Code_International_2        : constant Scan_Codes := 136;
   Scan_Code_International_3        : constant Scan_Codes := 137;  --  Yen
   Scan_Code_International_4        : constant Scan_Codes := 138;
   Scan_Code_International_5        : constant Scan_Codes := 139;
   Scan_Code_International_6        : constant Scan_Codes := 140;
   Scan_Code_International_7        : constant Scan_Codes := 141;
   Scan_Code_International_8        : constant Scan_Codes := 142;
   Scan_Code_International_9        : constant Scan_Codes := 143;
   Scan_Code_Language_1             : constant Scan_Codes := 144;  --  Hangul/En
   Scan_Code_Language_2             : constant Scan_Codes := 145;  --  Hanja con
   Scan_Code_Language_3             : constant Scan_Codes := 146;  --  Katakana.
   Scan_Code_Language_4             : constant Scan_Codes := 147;  --  Hiragana.
   Scan_Code_Language_5             : constant Scan_Codes := 148;  --  Zenkaku/H
   Scan_Code_Language_6             : constant Scan_Codes := 149;  --  Reserved.
   Scan_Code_Language_7             : constant Scan_Codes := 150;  --  Reserved.
   Scan_Code_Language_8             : constant Scan_Codes := 151;  --  Reserved.
   Scan_Code_Language_9             : constant Scan_Codes := 152;  --  Reserved.

   Scan_Code_Alt_Erase              : constant Scan_Codes := 153;  --  Erase-ease.
   Scan_Code_Sys_Req                : constant Scan_Codes := 154;
   Scan_Code_Cancel                 : constant Scan_Codes := 155;
   Scan_Code_Clear                  : constant Scan_Codes := 156;
   Scan_Code_Prior                  : constant Scan_Codes := 157;
   Scan_Code_Return_2               : constant Scan_Codes := 158;
   Scan_Code_Separator              : constant Scan_Codes := 159;
   Scan_Code_Out                    : constant Scan_Codes := 160;
   Scan_Code_Oper                   : constant Scan_Codes := 161;
   Scan_Code_Clear_Again            : constant Scan_Codes := 162;
   Scan_Code_CR_Sel                 : constant Scan_Codes := 163;
   Scan_Code_EX_Sel                 : constant Scan_Codes := 164;

   Scan_Code_KP_00                  : constant Scan_Codes := 176;
   Scan_Code_KP_000                 : constant Scan_Codes := 177;
   Scan_Code_Thousands_Separator    : constant Scan_Codes := 178;
   Scan_Code_Decimal_Separator      : constant Scan_Codes := 179;
   Scan_Code_Currency_Unit          : constant Scan_Codes := 180;
   Scan_Code_Currency_Subunit       : constant Scan_Codes := 181;
   Scan_Code_KP_Left_Parenthesis    : constant Scan_Codes := 182;
   Scan_Code_KP_Right_Parentheesis  : constant Scan_Codes := 183;
   Scan_Code_KP_Left_Brace          : constant Scan_Codes := 184;
   Scan_Code_KP_Right_Brace         : constant Scan_Codes := 185;
   Scan_Code_KP_Tab                 : constant Scan_Codes := 186;
   Scan_Code_KP_Backspace           : constant Scan_Codes := 187;
   Scan_Code_KP_A                   : constant Scan_Codes := 188;
   Scan_Code_KP_B                   : constant Scan_Codes := 189;
   Scan_Code_KP_C                   : constant Scan_Codes := 190;
   Scan_Code_KP_D                   : constant Scan_Codes := 191;
   Scan_Code_KP_E                   : constant Scan_Codes := 192;
   Scan_Code_KP_F                   : constant Scan_Codes := 193;
   Scan_Code_KP_XOR                 : constant Scan_Codes := 194;
   Scan_Code_KP_Power               : constant Scan_Codes := 195;
   Scan_Code_KP_Percent             : constant Scan_Codes := 196;
   Scan_Code_KP_Less                : constant Scan_Codes := 197;
   Scan_Code_KP_Greater             : constant Scan_Codes := 198;
   Scan_Code_KP_Ampersand           : constant Scan_Codes := 199;
   Scan_Code_KP_Double_Ampersand    : constant Scan_Codes := 200;
   Scan_Code_KP_Vertical_Bar        : constant Scan_Codes := 201;
   Scan_Code_KP_Double_Vertical_Bar : constant Scan_Codes := 202;
   Scan_Code_KP_Colon               : constant Scan_Codes := 203;
   Scan_Code_KP_Hash                : constant Scan_Codes := 204;
   Scan_Code_KP_Space               : constant Scan_Codes := 205;
   Scan_Code_KP_At                  : constant Scan_Codes := 206;
   Scan_Code_KP_Exclamation         : constant Scan_Codes := 207;
   Scan_Code_KP_Memory_Store        : constant Scan_Codes := 208;
   Scan_Code_KP_Memory_Recall       : constant Scan_Codes := 209;
   Scan_Code_KP_Memory_Clear        : constant Scan_Codes := 210;
   Scan_Code_KP_Memory_Add          : constant Scan_Codes := 211;
   Scan_Code_KP_Memory_Subtract     : constant Scan_Codes := 212;
   Scan_Code_KP_Memory_Multiply     : constant Scan_Codes := 213;
   Scan_Code_KP_Memory_Divide       : constant Scan_Codes := 214;
   Scan_Code_KP_Plus_Minus          : constant Scan_Codes := 215;
   Scan_Code_KP_Clear               : constant Scan_Codes := 216;
   Scan_Code_KP_Clear_Entry         : constant Scan_Codes := 217;
   Scan_Code_KP_Binary              : constant Scan_Codes := 218;
   Scan_Code_KP_Octal               : constant Scan_Codes := 219;
   Scan_Code_KP_Decimal             : constant Scan_Codes := 220;
   Scan_Code_KP_Hexadecimal         : constant Scan_Codes := 221;

   Scan_Code_Left_Control           : constant Scan_Codes := 224;
   Scan_Code_Left_Shift             : constant Scan_Codes := 225;
   Scan_Code_Left_Alt               : constant Scan_Codes := 226;    --  Alt, option, etc.
   Scan_Code_Left_GUI               : constant Scan_Codes := 227;    --  Windows, Command (Apple), Meta, etc.
   Scan_Code_Right_Control          : constant Scan_Codes := 228;
   Scan_Code_Right_Shift            : constant Scan_Codes := 229;
   Scan_Code_Right_Alt              : constant Scan_Codes := 230;    --  Alt gr, option, etc.
   Scan_Code_Right_GUI              : constant Scan_Codes := 231;    --  Windows, Command (Apple), Meta, etc.

   Scan_Code_Mode                   : constant Scan_Codes := 257;

   --  Usage page in USB document.
   Scan_Code_Audio_Next             : constant Scan_Codes := 258;
   Scan_Code_Audio_Previous         : constant Scan_Codes := 259;
   Scan_Code_Audio_Stop             : constant Scan_Codes := 260;
   Scan_Code_Audio_Play             : constant Scan_Codes := 261;
   Scan_Code_Audio_Mute             : constant Scan_Codes := 262;
   Scan_Code_Media_Select           : constant Scan_Codes := 263;
   Scan_Code_WWW                    : constant Scan_Codes := 264;
   Scan_Code_Mail                   : constant Scan_Codes := 265;
   Scan_Code_Calculator             : constant Scan_Codes := 266;
   Scan_Code_Computer               : constant Scan_Codes := 267;
   Scan_Code_AC_Search              : constant Scan_Codes := 268;
   Scan_Code_AC_Home                : constant Scan_Codes := 269;
   Scan_Code_AC_Back                : constant Scan_Codes := 270;
   Scan_Code_AC_Forward             : constant Scan_Codes := 271;
   Scan_Code_AC_Stop                : constant Scan_Codes := 272;
   Scan_Code_AC_Refresh             : constant Scan_Codes := 273;
   Scan_Code_AC_Bookmarks           : constant Scan_Codes := 274;

   --  Walther keys (for Mac?).
   Scan_Code_Brightness_Up          : constant Scan_Codes := 275;
   Scan_Code_Brightness_Down        : constant Scan_Codes := 276;
   Scan_Code_Display_Switch         : constant Scan_Codes := 277;

   Scan_Code_Illumination_Toggle    : constant Scan_Codes := 278;
   Scan_Code_Illumination_Down      : constant Scan_Codes := 279;
   Scan_Code_Illumination_Up        : constant Scan_Codes := 280;
   Scan_Code_Eject                  : constant Scan_Codes := 281;
   Scan_Code_Sleep                  : constant Scan_Codes := 282;

   Scan_Code_Application_1          : constant Scan_Codes := 283;
   Scan_Code_Application_2          : constant Scan_Codes := 284;

   --  All other scan codes go here.

   Scan_Code_Total                  : constant Scan_Codes := 512;

   function Value (Name : in String) return SDL.Events.Keyboards.Scan_Codes with
     Inline => True;

   function Image (Scan_Code : in SDL.Events.Keyboards.Scan_Codes) return String with
     Inline => True;

   -----------------------------------------------------------------------------------------------------------------
   --  Key codes.
   -----------------------------------------------------------------------------------------------------------------
   type Key_Codes is mod 2 ** 32 with
     Convention => C,
     Size       => 32;

   package Latin_1 renames Ada.Characters.Latin_1;

   --  Warning!! Do not use these 3 items in user code. Use the bindings below.
   Internal_To_Key_Code_Mask        : constant Key_Codes := 16#2000_0000#;

   function Convert is new Ada.Unchecked_Conversion (Source => Scan_Codes, Target => Key_Codes);

   function Internal_To_Key_Code (Code : in Scan_Codes) return Key_Codes is
     (Internal_To_Key_Code_Mask and Convert (Code));

   Code_Unknown                     : constant Key_Codes := Key_Codes'First;
   Code_Return                      : constant Key_Codes := Character'Pos (Latin_1.CR);
   Code_Escape                      : constant Key_Codes := Character'Pos (Latin_1.ESC);
   Code_Backspace                   : constant Key_Codes := Character'Pos (Latin_1.BS);
   Code_Tab                         : constant Key_Codes := Character'Pos (Latin_1.HT);
   Code_Space                       : constant Key_Codes := Character'Pos (Latin_1.Space);
   Code_Exclamation                 : constant Key_Codes := Character'Pos (Latin_1.Exclamation);
   Code_Double_Quote                : constant Key_Codes := Character'Pos (Latin_1.Quotation);
   Code_Hash                        : constant Key_Codes := Character'Pos (Latin_1.Number_Sign);
   Code_Percent                     : constant Key_Codes := Character'Pos (Latin_1.Percent_Sign);
   Code_Dollar                      : constant Key_Codes := Character'Pos (Latin_1.Dollar_Sign);
   Code_Ampersand                   : constant Key_Codes := Character'Pos (Latin_1.Ampersand);
   Code_Quote                       : constant Key_Codes := Character'Pos (Latin_1.Apostrophe);
   Code_Left_Parenthesis            : constant Key_Codes := Character'Pos (Latin_1.Left_Parenthesis);
   Code_Right_Parenthesis           : constant Key_Codes := Character'Pos (Latin_1.Right_Parenthesis);
   Code_Asterisk                    : constant Key_Codes := Character'Pos (Latin_1.Asterisk);
   Code_Plus                        : constant Key_Codes := Character'Pos (Latin_1.Plus_Sign);
   Code_Comma                       : constant Key_Codes := Character'Pos (Latin_1.Comma);
   Code_Minus                       : constant Key_Codes := Character'Pos (Latin_1.Minus_Sign);
   Code_Period                      : constant Key_Codes := Character'Pos (Latin_1.Full_Stop);
   Code_Slash                       : constant Key_Codes := Character'Pos (Latin_1.Solidus);
   Code_0                           : constant Key_Codes := Character'Pos ('0');
   Code_1                           : constant Key_Codes := Character'Pos ('1');
   Code_2                           : constant Key_Codes := Character'Pos ('2');
   Code_3                           : constant Key_Codes := Character'Pos ('3');
   Code_4                           : constant Key_Codes := Character'Pos ('4');
   Code_5                           : constant Key_Codes := Character'Pos ('5');
   Code_6                           : constant Key_Codes := Character'Pos ('6');
   Code_7                           : constant Key_Codes := Character'Pos ('7');
   Code_8                           : constant Key_Codes := Character'Pos ('8');
   Code_9                           : constant Key_Codes := Character'Pos ('9');
   Code_Colon                       : constant Key_Codes := Character'Pos (Latin_1.Colon);
   Code_Semi_Colon                  : constant Key_Codes := Character'Pos (Latin_1.Semicolon);
   Code_Less                        : constant Key_Codes := Character'Pos (Latin_1.Less_Than_Sign);
   Code_Equals                      : constant Key_Codes := Character'Pos (Latin_1.Equals_Sign);
   Code_Greater                     : constant Key_Codes := Character'Pos (Latin_1.Greater_Than_Sign);
   Code_Question                    : constant Key_Codes := Character'Pos (Latin_1.Question);
   Code_At                          : constant Key_Codes := Character'Pos (Latin_1.Commercial_At);

   --  Skip the uppercase letters.

   Code_Left_Bracket                : constant Key_Codes := Character'Pos (Latin_1.Left_Square_Bracket);
   Code_Back_Slash                  : constant Key_Codes := Character'Pos (Latin_1.Reverse_Solidus);
   Code_Right_Bracket               : constant Key_Codes := Character'Pos (Latin_1.Right_Square_Bracket);
   Code_Caret                       : constant Key_Codes := Character'Pos (Latin_1.Circumflex);
   Code_Underscore                  : constant Key_Codes := Character'Pos (Latin_1.Low_Line);
   Code_Back_Quote                  : constant Key_Codes := Character'Pos (Latin_1.Grave);
   Code_A                           : constant Key_Codes := Character'Pos ('a');
   Code_B                           : constant Key_Codes := Character'Pos ('b');
   Code_C                           : constant Key_Codes := Character'Pos ('c');
   Code_D                           : constant Key_Codes := Character'Pos ('d');
   Code_E                           : constant Key_Codes := Character'Pos ('e');
   Code_F                           : constant Key_Codes := Character'Pos ('f');
   Code_G                           : constant Key_Codes := Character'Pos ('g');
   Code_H                           : constant Key_Codes := Character'Pos ('h');
   Code_I                           : constant Key_Codes := Character'Pos ('i');
   Code_J                           : constant Key_Codes := Character'Pos ('j');
   Code_K                           : constant Key_Codes := Character'Pos ('k');
   Code_L                           : constant Key_Codes := Character'Pos ('l');
   Code_M                           : constant Key_Codes := Character'Pos ('m');
   Code_N                           : constant Key_Codes := Character'Pos ('n');
   Code_O                           : constant Key_Codes := Character'Pos ('o');
   Code_P                           : constant Key_Codes := Character'Pos ('p');
   Code_Q                           : constant Key_Codes := Character'Pos ('q');
   Code_R                           : constant Key_Codes := Character'Pos ('r');
   Code_S                           : constant Key_Codes := Character'Pos ('s');
   Code_T                           : constant Key_Codes := Character'Pos ('t');
   Code_U                           : constant Key_Codes := Character'Pos ('u');
   Code_V                           : constant Key_Codes := Character'Pos ('v');
   Code_W                           : constant Key_Codes := Character'Pos ('w');
   Code_X                           : constant Key_Codes := Character'Pos ('x');
   Code_Y                           : constant Key_Codes := Character'Pos ('y');
   Code_Z                           : constant Key_Codes := Character'Pos ('z');

   Code_Caps_Lock                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Caps_Lock);
   Code_F1                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F1);
   Code_F2                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F2);
   Code_F3                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F3);
   Code_F4                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F4);
   Code_F5                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F5);
   Code_F6                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F6);
   Code_F7                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F7);
   Code_F8                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F8);
   Code_F9                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F9);
   Code_F10                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F10);
   Code_F11                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F11);
   Code_F12                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F12);

   Code_Print_Screen                : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Print_Screen);
   Code_Scroll_Lock                 : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Scroll_Lock);
   Code_Pause                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Pause);
   Code_Insert                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Insert);
   Code_Home                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Home);
   Code_Page_Up                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Page_Up);
   Code_Delete                      : constant Key_Codes := Character'Pos (Latin_1.DEL);
   Code_End                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_End);
   Code_Page_Down                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Page_Down);
   Code_Right                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Right);
   Code_Left                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Left);
   Code_Down                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Down);
   Code_Up                          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Up);

   Code_Num_Lock_Clear              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Num_Lock_Clear);
   Code_KP_Divide                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Divide);
   Code_KP_Multiply                 : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Multiply);
   Code_KP_Minus                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Minus);
   Code_KP_Plus                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Plus);
   Code_KP_Enter                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Enter);
   Code_KP_1                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_1);
   Code_KP_2                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_2);
   Code_KP_3                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_3);
   Code_KP_4                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_4);
   Code_KP_5                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_5);
   Code_KP_6                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_6);
   Code_KP_7                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_7);
   Code_KP_8                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_8);
   Code_KP_9                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_9);
   Code_KP_0                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_0);
   Code_KP_Period                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Period);

   Code_Application                 : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Application);
   Code_Power                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Power);
   Code_KP_Equals                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Equals);
   Code_F13                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F13);
   Code_F14                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F14);
   Code_F15                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F15);
   Code_F16                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F16);
   Code_F17                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F17);
   Code_F18                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F18);
   Code_F19                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F19);
   Code_F20                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F20);
   Code_F21                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F21);
   Code_F22                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F22);
   Code_F23                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F23);
   Code_F24                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_F24);
   Code_Execute                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Execute);
   Code_Help                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Help);
   Code_Menu                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Menu);
   Code_Select                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Select);
   Code_Stop                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Stop);
   Code_Again                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Again);
   Code_Undo                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Undo);
   Code_Cut                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Cut);
   Code_Copy                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Copy);
   Code_Paste                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Paste);
   Code_Find                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Find);
   Code_Mute                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Mute);
   Code_Volume_Up                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Volume_Up);
   Code_Volume_Down                 : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Volume_Down);
   Code_KP_Comma                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Comma);
   Code_KP_Equals_AS400             : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Equals_AS400);

   Code_Alt_Erase                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Alt_Erase);
   Code_Sys_Req                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Sys_Req);
   Code_Cancel                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Cancel);
   Code_Clear                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Clear);
   Code_Prior                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Prior);
   Code_Return_2                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Return_2);
   Code_Separator                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Separator);
   Code_Out                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Out);
   Code_Oper                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Oper);
   Code_Clear_Again                 : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Clear_Again);
   Code_CR_Sel                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_CR_Sel);
   Code_Ex_Sel                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_EX_Sel);

   Code_KP_00                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_00);
   Code_KP_000                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_000);
   Code_Thousands_Separator         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Thousands_Separator);
   Code_Decimal_Separator           : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Decimal_Separator);
   Code_Currency_Unit               : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Currency_Unit);
   Code_Currency_Subunit            : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Currency_Subunit);
   Code_KP_Left_Parenthesis         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Left_Parenthesis);
   Code_KP_Right_Parentheesis       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Right_Parentheesis);
   Code_KP_Left_Brace               : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Left_Brace);
   Code_KP_Right_Brace              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Right_Brace);
   Code_KP_Tab                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Tab);
   Code_KP_Backspace                : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Backspace);
   Code_KP_A                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_A);
   Code_KP_B                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_B);
   Code_KP_C                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_C);
   Code_KP_D                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_D);
   Code_KP_E                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_E);
   Code_KP_F                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_F);
   Code_KP_XOR                      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_XOR);
   Code_KP_Power                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Power);
   Code_KP_Percent                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Percent);
   Code_KP_Less                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Less);
   Code_KP_Greater                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Greater);
   Code_KP_Ampersand                : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Ampersand);
   Code_KP_Double_Ampersand         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Double_Ampersand);
   Code_KP_Vertical_Bar             : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Vertical_Bar);
   Code_KP_Double_Vertical_Bar      : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Double_Vertical_Bar);
   Code_KP_Colon                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Colon);
   Code_KP_Hash                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Hash);
   Code_KP_Space                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Space);
   Code_KP_At                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_At);
   Code_KP_Exclamation              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Exclamation);
   Code_KP_Memory_Store             : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Store);
   Code_KP_Memory_Recall            : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Recall);
   Code_KP_Memory_Clear             : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Clear);
   Code_KP_Memory_Add               : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Add);
   Code_KP_Memory_Subtract          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Subtract);
   Code_KP_Memory_Multiply          : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Multiply);
   Code_KP_Memory_Divide            : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Memory_Divide);
   Code_KP_Plus_Minus               : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Plus_Minus);
   Code_KP_Clear                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Clear);
   Code_KP_Clear_Entry              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Clear_Entry);
   Code_KP_Binary                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Binary);
   Code_KP_Octal                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Octal);
   Code_KP_Decimal                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Decimal);
   Code_KP_Hexadecimal              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_KP_Hexadecimal);

   Code_Left_Control                : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Left_Control);
   Code_Left_Shift                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Left_Shift);
   Code_Left_Alt                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Left_Alt);
   Code_Left_GUI                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Left_GUI);
   Code_Right_Control               : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Right_Control);
   Code_Right_Shift                 : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Right_Shift);
   Code_Right_Alt                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Right_Alt);
   Code_Right_GUI                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Right_GUI);

   Code_Mode                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Mode);

   Code_Audio_Next                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Audio_Next);
   Code_Audio_Previous              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Audio_Previous);
   Code_Audio_Stop                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Audio_Stop);
   Code_Audio_Play                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Audio_Play);
   Code_Audio_Mute                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Audio_Mute);
   Code_Media_Select                : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Media_Select);
   Code_WWW                         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_WWW);
   Code_Mail                        : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Mail);
   Code_Calculator                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Calculator);
   Code_Computer                    : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Computer);
   Code_AC_Search                   : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Search);
   Code_AC_Home                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Home);
   Code_AC_Back                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Back);
   Code_AC_Forward                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Forward);
   Code_AC_Stop                     : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Stop);
   Code_AC_Refresh                  : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Refresh);
   Code_AC_Bookmarks                : constant Key_Codes := Internal_To_Key_Code (Scan_Code_AC_Bookmarks);

   Code_Brightness_Down             : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Brightness_Down);
   Code_Brightness_Up               : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Brightness_Up);
   Code_Display_Switch              : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Display_Switch);
   Code_Illumination_Toggle         : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Illumination_Toggle);
   Code_Illumination_Down           : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Illumination_Down);
   Code_Illumination_Up             : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Illumination_Up);
   Code_Eject                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Eject);
   Code_Sleep                       : constant Key_Codes := Internal_To_Key_Code (Scan_Code_Sleep);

   function Value (Name : in String) return SDL.Events.Keyboards.Key_Codes with
     Inline => True;

   function Image (Key_Code : in SDL.Events.Keyboards.Key_Codes) return String with
     Inline => True;

   function To_Key_Code (Scan_Code : in SDL.Events.Keyboards.Scan_Codes) return SDL.Events.Keyboards.Key_Codes with
     Inline => True;

   function To_Scan_Code (Key_Code : in SDL.Events.Keyboards.Key_Codes) return SDL.Events.Keyboards.Scan_Codes with
     Inline => True;

   -----------------------------------------------------------------------------------------------------------------
   --  Key modifiers.
   -----------------------------------------------------------------------------------------------------------------
   type Key_Modifiers is mod 2 ** 16 with
     Convention => C,
     Size       => 16;

   Modifier_None          : constant Key_Modifiers := 16#00_00#;
   Modifier_Left_Shift    : constant Key_Modifiers := 16#00_01#;
   Modifier_Right_Shift   : constant Key_Modifiers := 16#00_02#;
   Modifier_Left_Control  : constant Key_Modifiers := 16#00_40#;
   Modifier_Right_Control : constant Key_Modifiers := 16#00_80#;
   Modifier_Left_Alt      : constant Key_Modifiers := 16#01_00#;
   Modifier_Right_Alt     : constant Key_Modifiers := 16#02_00#;
   Modifier_Left_GUI      : constant Key_Modifiers := 16#04_00#;
   Modifier_Right_GUI     : constant Key_Modifiers := 16#08_00#;
   Modifier_Num           : constant Key_Modifiers := 16#10_00#;
   Modifier_Caps          : constant Key_Modifiers := 16#20_00#;
   Modifier_Mode          : constant Key_Modifiers := 16#40_00#;
   Modifier_Control       : constant Key_Modifiers := Modifier_Left_Control or Modifier_Right_Control;
   Modifier_Shift         : constant Key_Modifiers := Modifier_Left_Shift or Modifier_Right_Shift;
   Modifier_Alt           : constant Key_Modifiers := Modifier_Left_Alt or Modifier_Right_Alt;
   Modifier_GUI           : constant Key_Modifiers := Modifier_Left_GUI or Modifier_Right_GUI;
   Modifier_Reserved      : constant Key_Modifiers := 16#80_00#;

   type Key_Syms is
      record
         Scan_Code : Scan_Codes;
         Key_Code  : Key_Codes;
         Modifiers : Key_Modifiers;
         Unused    : Interfaces.Unsigned_32;
      end record with
     Convention => C;

   type Keyboard_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Key_Up/Down.
         Time_Stamp : Time_Stamps;

         ID         : SDL.Video.Windows.ID;
         State      : Button_State;
         Repeat     : Interfaces.Unsigned_8;
         Padding_2  : Padding_8;
         Padding_3  : Padding_8;
         Key_Sym    : Key_Syms;
      end record with
     Convention => C;

   -----------------------------------------------------------------------------------------------------------------
   --  Text editing events
   -----------------------------------------------------------------------------------------------------------------
   Max_UTF8_Elements             : constant := 31;
   Max_UTF8_Element_Storage_Bits : constant := ((Max_UTF8_Elements + 1) * 8) - 1;

   subtype UTF8_Text_Buffers is Interfaces.C.char_array (0 .. Max_UTF8_Elements);

   type Cursor_Positions is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Text_Lengths is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Text_Editing_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Text_Editing.
         Time_Stamp : Time_Stamps;

         ID         : SDL.Video.Windows.ID;
         Text       : UTF8_Text_Buffers;
         Start      : Cursor_Positions;      --  TODO: Find out why this needs to be a signed value!
         Length     : Text_Lengths;          --  TODO: Again, signed, why?
      end record with
     Convention => C;

   -----------------------------------------------------------------------------------------------------------------
   --  Text input events
   -----------------------------------------------------------------------------------------------------------------
   type Text_Input_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Text_Editing.
         Time_Stamp : Time_Stamps;

         ID         : SDL.Video.Windows.ID;
         Text       : UTF8_Text_Buffers;
      end record with
     Convention => C;

private
   for Key_Syms use
      record
         Scan_Code at 0 * SDL.Word range 0 .. 31;
         Key_Code  at 1 * SDL.Word range 0 .. 31;
         Modifiers at 2 * SDL.Word range 0 .. 15;
         Unused    at 3 * SDL.Word range 0 .. 31;
      end record;

   for Keyboard_Events use
      record
         Event_Type at 0 * SDL.Word range  0  .. 31;
         Time_Stamp at 1 * SDL.Word range  0  .. 31;

         ID         at 2 * SDL.Word range  0  .. 31;
         State      at 3 * SDL.Word range  0  ..  7;
         Repeat     at 3 * SDL.Word range  8  .. 15;
         Padding_2  at 3 * SDL.Word range  16 .. 23;
         Padding_3  at 3 * SDL.Word range  24 .. 31;
      end record;

   for Text_Editing_Events use
      record
         Event_Type at  0 * SDL.Word range  0  .. 31;
         Time_Stamp at  1 * SDL.Word range  0  .. 31;

         ID         at  2 * SDL.Word range  0  .. 31;
         Text       at  3 * SDL.Word range  0  .. Max_UTF8_Element_Storage_Bits; -- 31 characters.
         Start      at 11 * SDL.Word range  0  .. 31;
         Length     at 12 * SDL.Word range  0  .. 31;
      end record;

   for Text_Input_Events use
      record
         Event_Type at  0 * SDL.Word range  0  .. 31;
         Time_Stamp at  1 * SDL.Word range  0  .. 31;

         ID         at  2 * SDL.Word range  0  .. 31;
         Text       at  3 * SDL.Word range  0  .. Max_UTF8_Element_Storage_Bits; -- 31 characters.
      end record;
end SDL.Events.Keyboards;
