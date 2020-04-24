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
--  Gen_Keyboard
--------------------------------------------------------------------------------------------------------------------
--  Generates the SDL.Events.Keyboards.ads file.
--  Makefile should call this and redirect the output to the correct file in $TOP/gen_src/sdl-events-keyboards.ads.
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Utils; use Utils;
with Scancodes; use Scancodes;

procedure Gen_Keyboard is
   package Latin_1 renames Ada.Characters.Latin_1;

   function To_US (Str : in String) return Unbounded_String renames To_Unbounded_String;

   License : constant array (Positive range <>) of Unbounded_String :=
     (To_US ("Copyright (c) 2013-2020,  Luke A. Guest"),
      To_US (""),
      To_US ("This software is provided 'as-is', without any express or implied"),
      To_US ("warranty. In no event will the authors be held liable for any damages"),
      To_US ("arising from the use of this software."),
      To_US (""),
      To_US ("Permission is granted to anyone to use this software for any purpose,"),
      To_US ("including commercial applications, and to alter it and redistribute it"),
      To_US ("freely, subject to the following restrictions:"),
      To_US (""),
      To_US ("   1. The origin of this software must not be misrepresented; you must not"),
      To_US ("   claim that you wrote the original software. If you use this software"),
      To_US ("   in a product, an acknowledgment in the product documentation would be"),
      To_US ("   appreciated but is not required."),
      To_US (""),
      To_US ("   2. Altered source versions must be plainly marked as such, and must not be"),
      To_US ("   misrepresented as being the original software."),
      To_US (""),
      To_US ("   3. This notice may not be removed or altered from any source"),
      To_US ("   distribution."));

   Package_Description : constant array (Positive range <>) of Unbounded_String :=
     (To_US ("SDL.Events.Keyboards"),
      To_US (""),
      To_US ("Keyboard specific events."));

   type Mapping_States is (Output, New_Line, Comment);

   package Scan_Codes_IO is new Integer_IO (Scan_Codes);
   use Scan_Codes_IO;

   type Scan_Code_Mapping is
      record
         State   : Mapping_States   := Output;
         Name    : Unbounded_String := Null_Unbounded_String;
         Code    : Scan_Codes       := Scan_Codes'Last;
         Comment : Unbounded_String := Null_Unbounded_String;
      end record;

   New_Line_Scan_Code : constant Scan_Code_Mapping := (New_Line,
                                                       Null_Unbounded_String,
                                                       Scan_Codes'Last,
                                                       Null_Unbounded_String);

   type Scan_Code_Tables is array (Positive range <>) of Scan_Code_Mapping;

   Scan_Code_Table : constant Scan_Code_Tables :=
     ((Output,   To_US ("Scan_Code_Unknown"),                0,               Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_A"),                      4,               Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_B"),                      5,               Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_C"),                      6,               Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_D"),                      7,               Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_E"),                      8,               Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F"),                      9,               Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_G"),                      10,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_H"),                      11,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_I"),                      12,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_J"),                      13,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_K"),                      14,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_L"),                      15,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_M"),                      16,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_N"),                      17,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_O"),                      18,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_P"),                      19,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Q"),                      20,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_R"),                      21,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_S"),                      22,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_T"),                      23,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_U"),                      24,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_V"),                      25,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_W"),                      26,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_X"),                      27,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Y"),                      28,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Z"),                      29,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_1"),                      30,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_2"),                      31,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_3"),                      32,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_4"),                      33,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_5"),                      34,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_6"),                      35,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_7"),                      36,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_8"),                      37,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_9"),                      38,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_0"),                      39,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Return"),                 40,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Escape"),                 41,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Backspace"),              42,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Tab"),                    43,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Space"),                  44,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Minus"),                  45,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Equals"),                 46,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Left_Bracket"),           47,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Right_Bracket"),          48,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Back_Slash"),             49,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Non_US_Hash"),            50,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Semi_Colon"),             51,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Apostrophe"),             52,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Grave"),                  53,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Comma"),                  54,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Period"),                 55,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Slash"),                  56,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Caps_Lock"),              57,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_F1"),                     58,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F2"),                     59,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F3"),                     60,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F4"),                     61,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F5"),                     62,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F6"),                     63,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F7"),                     64,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F8"),                     65,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F9"),                     66,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F10"),                    67,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F11"),                    68,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F12"),                    69,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Print_Screen"),           70,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Scroll_Lock"),            71,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Pause"),                  72,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Insert"),                 73,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Home"),                   74,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Page_Up"),                75,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Delete"),                 76,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_End"),                    77,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Page_Down"),              78,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Right"),                  79,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Left"),                   80,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Down"),                   81,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Up"),                     82,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Num_Lock_Clear"),         83,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_KP_Divide"),              84,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Multiply"),            85,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Minus"),               86,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Plus"),                87,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Enter"),               88,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_1"),                   89,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_2"),                   90,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_3"),                   91,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_4"),                   92,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_5"),                   93,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_6"),                   94,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_7"),                   95,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_8"),                   96,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_9"),                   97,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_0"),                   98,              Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Period"),              99,              Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Non_US_Back_Slash"),      100,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Application"),            101,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Power"),                  102,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Equals"),              103,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F13"),                    104,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F14"),                    105,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F15"),                    106,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F16"),                    107,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F17"),                    108,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F18"),                    109,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F19"),                    110,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F20"),                    111,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F21"),                    112,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F22"),                    113,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F23"),                    114,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_F24"),                    115,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Execute"),                116,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Help"),                   117,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Menu"),                   118,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Select"),                 119,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Stop"),                   120,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Again"),                  121,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Undo"),                   122,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Cut"),                    123,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Copy"),                   124,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Paste"),                  125,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Find"),                   126,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Mute"),                   127,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Volume_Up"),              128,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Volume_Down"),            129,             Null_Unbounded_String),
      (Comment,  Null_Unbounded_String,                      Scan_Codes'Last,
       To_US ("Scan_Code_Locking_Caps_Lock   : constant Scan_Codes := 130;")),
      (Comment,  Null_Unbounded_String,                      Scan_Codes'Last,
       To_US ("Scan_Code_Locking_Num_Lock    : constant Scan_Codes := 131;")),
      (Comment,  Null_Unbounded_String,                      Scan_Codes'Last,
       To_US ("Scan_Code_Locking_Scroll_Lock : constant Scan_Codes := 132;")),
      (Output,   To_US ("Scan_Code_KP_Comma"),               133,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Equals_AS400"),        134,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_International_1"),        135,             To_US ("Used on Asian keyboards.")),
      (Output,   To_US ("Scan_Code_International_2"),        136,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_International_3"),        137,             To_US ("Yen")),
      (Output,   To_US ("Scan_Code_International_4"),        138,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_International_5"),        139,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_International_6"),        140,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_International_7"),        141,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_International_8"),        142,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_International_9"),        143,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Language_1"),             144,             To_US ("Hangul/En")),
      (Output,   To_US ("Scan_Code_Language_2"),             145,             To_US ("Hanja con")),
      (Output,   To_US ("Scan_Code_Language_3"),             146,             To_US ("Katakana.")),
      (Output,   To_US ("Scan_Code_Language_4"),             147,             To_US ("Hiragana.")),
      (Output,   To_US ("Scan_Code_Language_5"),             148,             To_US ("Zenkaku/H")),
      (Output,   To_US ("Scan_Code_Language_6"),             149,             To_US ("Reserved.")),
      (Output,   To_US ("Scan_Code_Language_7"),             150,             To_US ("Reserved.")),
      (Output,   To_US ("Scan_Code_Language_8"),             151,             To_US ("Reserved.")),
      (Output,   To_US ("Scan_Code_Language_9"),             152,             To_US ("Reserved.")),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Alt_Erase"),              153,             To_US ("Erase-ease.")),
      (Output,   To_US ("Scan_Code_Sys_Req"),                154,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Cancel"),                 155,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Clear"),                  156,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Prior"),                  157,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Return_2"),               158,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Separator"),              159,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Out"),                    160,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Oper"),                   161,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Clear_Again"),            162,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_CR_Sel"),                 163,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_EX_Sel"),                 164,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_KP_00"),                  176,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_000"),                 177,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Thousands_Separator"),    178,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Decimal_Separator"),      179,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Currency_Unit"),          180,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Currency_Subunit"),       181,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Left_Parenthesis"),    182,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Right_Parentheesis"),  183,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Left_Brace"),          184,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Right_Brace"),         185,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Tab"),                 186,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Backspace"),           187,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_A"),                   188,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_B"),                   189,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_C"),                   190,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_D"),                   191,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_E"),                   192,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_F"),                   193,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_XOR"),                 194,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Power"),               195,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Percent"),             196,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Less"),                197,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Greater"),             198,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Ampersand"),           199,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Double_Ampersand"),    200,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Vertical_Bar"),        201,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Double_Vertical_Bar"), 202,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Colon"),               203,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Hash"),                204,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Space"),               205,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_At"),                  206,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Exclamation"),         207,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Store"),        208,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Recall"),       209,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Clear"),        210,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Add"),          211,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Subtract"),     212,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Multiply"),     213,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Memory_Divide"),       214,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Plus_Minus"),          215,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Clear"),               216,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Clear_Entry"),         217,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Binary"),              218,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Octal"),               219,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Decimal"),             220,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_KP_Hexadecimal"),         221,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Left_Control"),           224,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Left_Shift"),             225,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Left_Alt"),               226,             To_US ("Alt, option, etc.")),
      (Output,   To_US ("Scan_Code_Left_GUI"),               227,
       To_US ("Windows, Command (Apple), Meta, etc.")),
      (Output,   To_US ("Scan_Code_Right_Control"),          228,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Right_Shift"),            229,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Right_Alt"),              230,
       To_US ("Alt gr, option, etc.")),
      (Output,   To_US ("Scan_Code_Right_GUI"),              231,
       To_US ("Windows, Command (Apple), Meta, etc.")),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Mode"),                   257,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Comment,  Null_Unbounded_String,                      Scan_Codes'Last, To_US ("Usage page in USB document.")),
      (Output,   To_US ("Scan_Code_Audio_Next"),             258,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Audio_Previous"),         259,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Audio_Stop"),             260,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Audio_Play"),             261,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Audio_Mute"),             262,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Media_Select"),           263,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_WWW"),                    264,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Mail"),                   265,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Calculator"),             266,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Computer"),               267,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Search"),              268,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Home"),                269,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Back"),                270,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Forward"),             271,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Stop"),                272,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Refresh"),             273,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_AC_Bookmarks"),           274,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Comment,  Null_Unbounded_String,                      Scan_Codes'Last, To_US ("Walther keys (for Mac?).")),
      (Output,   To_US ("Scan_Code_Brightness_Up"),          275,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Brightness_Down"),        276,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Display_Switch"),         277,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Illumination_Toggle"),    278,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Illumination_Down"),      279,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Illumination_Up"),        280,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Eject"),                  281,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Sleep"),                  282,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Application_1"),          283,             Null_Unbounded_String),
      (Output,   To_US ("Scan_Code_Application_2"),          284,             Null_Unbounded_String),
      (New_Line_Scan_Code),
      (Comment,  Null_Unbounded_String,                      Scan_Codes'Last, To_US ("All other scan codes go here.")),
      (New_Line_Scan_Code),
      (Output,   To_US ("Scan_Code_Total"),                  512,             Null_Unbounded_String));

   --  See SDL_SCANCODE_TO_KEYCODE in include/SDL_keycode.h
   type Key_Codes is mod 2 ** 32 with
     Convention => C,
     Size       => 32;
   Internal_To_Key_Code_Mask : constant Key_Codes := 16#4000_0000#;

   package Key_Codes_IO is new Modular_IO (Key_Codes);
   use Key_Codes_IO;

   function Convert is new Ada.Unchecked_Conversion (Source => Scan_Codes, Target => Key_Codes);

   function To_Key_Code (Code : in Scan_Codes) return Key_Codes is
     (Internal_To_Key_Code_Mask or Convert (Code));

   type Key_Code_Mapping is
      record
         State : Mapping_States   := Output;
         Name  : Unbounded_String := Null_Unbounded_String;
         Code  : Key_Codes        := Key_Codes'Last;
      end record;

   New_Line_Code : constant Key_Code_Mapping := (New_Line, Null_Unbounded_String, Key_Codes'Last);

   type Key_Code_Tables is array (Positive range <>) of Key_Code_Mapping;

   Key_Code_Table : constant Key_Code_Tables :=
     ((Output,   To_US ("Code_Return"),                 Character'Pos (Latin_1.CR)),
      (Output,   To_US ("Code_Escape"),                 Character'Pos (Latin_1.ESC)),
      (Output,   To_US ("Code_Backspace"),              Character'Pos (Latin_1.BS)),
      (Output,   To_US ("Code_Tab"),                    Character'Pos (Latin_1.HT)),
      (Output,   To_US ("Code_Space"),                  Character'Pos (Latin_1.Space)),
      (Output,   To_US ("Code_Exclamation"),            Character'Pos (Latin_1.Exclamation)),
      (Output,   To_US ("Code_Double_Quote"),           Character'Pos (Latin_1.Quotation)),
      (Output,   To_US ("Code_Hash"),                   Character'Pos (Latin_1.Number_Sign)),
      (Output,   To_US ("Code_Percent"),                Character'Pos (Latin_1.Percent_Sign)),
      (Output,   To_US ("Code_Dollar"),                 Character'Pos (Latin_1.Dollar_Sign)),
      (Output,   To_US ("Code_Ampersand"),              Character'Pos (Latin_1.Ampersand)),
      (Output,   To_US ("Code_Quote"),                  Character'Pos (Latin_1.Apostrophe)),
      (Output,   To_US ("Code_Left_Parenthesis"),       Character'Pos (Latin_1.Left_Parenthesis)),
      (Output,   To_US ("Code_Right_Parenthesis"),      Character'Pos (Latin_1.Right_Parenthesis)),
      (Output,   To_US ("Code_Asterisk"),               Character'Pos (Latin_1.Asterisk)),
      (Output,   To_US ("Code_Plus"),                   Character'Pos (Latin_1.Plus_Sign)),
      (Output,   To_US ("Code_Comma"),                  Character'Pos (Latin_1.Comma)),
      (Output,   To_US ("Code_Minus"),                  Character'Pos (Latin_1.Minus_Sign)),
      (Output,   To_US ("Code_Period"),                 Character'Pos (Latin_1.Full_Stop)),
      (Output,   To_US ("Code_Slash"),                  Character'Pos (Latin_1.Solidus)),
      (Output,   To_US ("Code_0"),                      Character'Pos ('0')),
      (Output,   To_US ("Code_1"),                      Character'Pos ('1')),
      (Output,   To_US ("Code_2"),                      Character'Pos ('2')),
      (Output,   To_US ("Code_3"),                      Character'Pos ('3')),
      (Output,   To_US ("Code_4"),                      Character'Pos ('4')),
      (Output,   To_US ("Code_5"),                      Character'Pos ('5')),
      (Output,   To_US ("Code_6"),                      Character'Pos ('6')),
      (Output,   To_US ("Code_7"),                      Character'Pos ('7')),
      (Output,   To_US ("Code_8"),                      Character'Pos ('8')),
      (Output,   To_US ("Code_9"),                      Character'Pos ('9')),
      (Output,   To_US ("Code_Colon"),                  Character'Pos (Latin_1.Colon)),
      (Output,   To_US ("Code_Semi_Colon"),             Character'Pos (Latin_1.Semicolon)),
      (Output,   To_US ("Code_Less"),                   Character'Pos (Latin_1.Less_Than_Sign)),
      (Output,   To_US ("Code_Equals"),                 Character'Pos (Latin_1.Equals_Sign)),
      (Output,   To_US ("Code_Greater"),                Character'Pos (Latin_1.Greater_Than_Sign)),
      (Output,   To_US ("Code_Question"),               Character'Pos (Latin_1.Question)),
      (Output,   To_US ("Code_At"),                     Character'Pos (Latin_1.Commercial_At)),
      (New_Line_Code),
      (Comment,  To_US ("Skip the uppercase letters."), Key_Codes'Last),
      (New_Line_Code),
      (Output,   To_US ("Code_Left_Bracket"),           Character'Pos (Latin_1.Left_Square_Bracket)),
      (Output,   To_US ("Code_Back_Slash"),             Character'Pos (Latin_1.Reverse_Solidus)),
      (Output,   To_US ("Code_Right_Bracket"),          Character'Pos (Latin_1.Right_Square_Bracket)),
      (Output,   To_US ("Code_Caret"),                  Character'Pos (Latin_1.Circumflex)),
      (Output,   To_US ("Code_Underscore"),             Character'Pos (Latin_1.Low_Line)),
      (Output,   To_US ("Code_Back_Quote"),             Character'Pos (Latin_1.Grave)),
      (Output,   To_US ("Code_A"),                      Character'Pos ('a')),
      (Output,   To_US ("Code_B"),                      Character'Pos ('b')),
      (Output,   To_US ("Code_C"),                      Character'Pos ('c')),
      (Output,   To_US ("Code_D"),                      Character'Pos ('d')),
      (Output,   To_US ("Code_E"),                      Character'Pos ('e')),
      (Output,   To_US ("Code_F"),                      Character'Pos ('f')),
      (Output,   To_US ("Code_G"),                      Character'Pos ('g')),
      (Output,   To_US ("Code_H"),                      Character'Pos ('h')),
      (Output,   To_US ("Code_I"),                      Character'Pos ('i')),
      (Output,   To_US ("Code_J"),                      Character'Pos ('j')),
      (Output,   To_US ("Code_K"),                      Character'Pos ('k')),
      (Output,   To_US ("Code_L"),                      Character'Pos ('l')),
      (Output,   To_US ("Code_M"),                      Character'Pos ('m')),
      (Output,   To_US ("Code_N"),                      Character'Pos ('n')),
      (Output,   To_US ("Code_O"),                      Character'Pos ('o')),
      (Output,   To_US ("Code_P"),                      Character'Pos ('p')),
      (Output,   To_US ("Code_Q"),                      Character'Pos ('q')),
      (Output,   To_US ("Code_R"),                      Character'Pos ('r')),
      (Output,   To_US ("Code_S"),                      Character'Pos ('s')),
      (Output,   To_US ("Code_T"),                      Character'Pos ('t')),
      (Output,   To_US ("Code_U"),                      Character'Pos ('u')),
      (Output,   To_US ("Code_V"),                      Character'Pos ('v')),
      (Output,   To_US ("Code_W"),                      Character'Pos ('w')),
      (Output,   To_US ("Code_X"),                      Character'Pos ('x')),
      (Output,   To_US ("Code_Y"),                      Character'Pos ('y')),
      (Output,   To_US ("Code_Z"),                      Character'Pos ('z')),
      (New_Line_Code),
      (Output,   To_US ("Code_Caps_Lock"),              To_Key_Code (Scan_Code_Caps_Lock)),
      (Output,   To_US ("Code_F1"),                     To_Key_Code (Scan_Code_F1)),
      (Output,   To_US ("Code_F2"),                     To_Key_Code (Scan_Code_F2)),
      (Output,   To_US ("Code_F3"),                     To_Key_Code (Scan_Code_F3)),
      (Output,   To_US ("Code_F4"),                     To_Key_Code (Scan_Code_F4)),
      (Output,   To_US ("Code_F5"),                     To_Key_Code (Scan_Code_F5)),
      (Output,   To_US ("Code_F6"),                     To_Key_Code (Scan_Code_F6)),
      (Output,   To_US ("Code_F7"),                     To_Key_Code (Scan_Code_F7)),
      (Output,   To_US ("Code_F8"),                     To_Key_Code (Scan_Code_F8)),
      (Output,   To_US ("Code_F9"),                     To_Key_Code (Scan_Code_F9)),
      (Output,   To_US ("Code_F10"),                    To_Key_Code (Scan_Code_F10)),
      (Output,   To_US ("Code_F11"),                    To_Key_Code (Scan_Code_F11)),
      (Output,   To_US ("Code_F12"),                    To_Key_Code (Scan_Code_F12)),
      (New_Line_Code),
      (Output,   To_US ("Code_Print_Screen"),           To_Key_Code (Scan_Code_Print_Screen)),
      (Output,   To_US ("Code_Scroll_Lock"),            To_Key_Code (Scan_Code_Scroll_Lock)),
      (Output,   To_US ("Code_Pause"),                  To_Key_Code (Scan_Code_Pause)),
      (Output,   To_US ("Code_Insert"),                 To_Key_Code (Scan_Code_Insert)),
      (Output,   To_US ("Code_Home"),                   To_Key_Code (Scan_Code_Home)),
      (Output,   To_US ("Code_Page_Up"),                To_Key_Code (Scan_Code_Page_Up)),
      (Output,   To_US ("Code_Delete"),                 Character'Pos (Latin_1.DEL)),
      (Output,   To_US ("Code_End"),                    To_Key_Code (Scan_Code_End)),
      (Output,   To_US ("Code_Page_Down"),              To_Key_Code (Scan_Code_Page_Down)),
      (Output,   To_US ("Code_Right"),                  To_Key_Code (Scan_Code_Right)),
      (Output,   To_US ("Code_Left"),                   To_Key_Code (Scan_Code_Left)),
      (Output,   To_US ("Code_Down"),                   To_Key_Code (Scan_Code_Down)),
      (Output,   To_US ("Code_Up"),                     To_Key_Code (Scan_Code_Up)),
      (New_Line_Code),
      (Output,   To_US ("Code_Num_Lock_Clear"),         To_Key_Code (Scan_Code_Num_Lock_Clear)),
      (Output,   To_US ("Code_KP_Divide"),              To_Key_Code (Scan_Code_KP_Divide)),
      (Output,   To_US ("Code_KP_Multiply"),            To_Key_Code (Scan_Code_KP_Multiply)),
      (Output,   To_US ("Code_KP_Minus"),               To_Key_Code (Scan_Code_KP_Minus)),
      (Output,   To_US ("Code_KP_Plus"),                To_Key_Code (Scan_Code_KP_Plus)),
      (Output,   To_US ("Code_KP_Enter"),               To_Key_Code (Scan_Code_KP_Enter)),
      (Output,   To_US ("Code_KP_1"),                   To_Key_Code (Scan_Code_KP_1)),
      (Output,   To_US ("Code_KP_2"),                   To_Key_Code (Scan_Code_KP_2)),
      (Output,   To_US ("Code_KP_3"),                   To_Key_Code (Scan_Code_KP_3)),
      (Output,   To_US ("Code_KP_4"),                   To_Key_Code (Scan_Code_KP_4)),
      (Output,   To_US ("Code_KP_5"),                   To_Key_Code (Scan_Code_KP_5)),
      (Output,   To_US ("Code_KP_6"),                   To_Key_Code (Scan_Code_KP_6)),
      (Output,   To_US ("Code_KP_7"),                   To_Key_Code (Scan_Code_KP_7)),
      (Output,   To_US ("Code_KP_8"),                   To_Key_Code (Scan_Code_KP_8)),
      (Output,   To_US ("Code_KP_9"),                   To_Key_Code (Scan_Code_KP_9)),
      (Output,   To_US ("Code_KP_0"),                   To_Key_Code (Scan_Code_KP_0)),
      (Output,   To_US ("Code_KP_Period"),              To_Key_Code (Scan_Code_KP_Period)),
      (New_Line_Code),
      (Output,   To_US ("Code_Application"),            To_Key_Code (Scan_Code_Application)),
      (Output,   To_US ("Code_Power"),                  To_Key_Code (Scan_Code_Power)),
      (Output,   To_US ("Code_KP_Equals"),              To_Key_Code (Scan_Code_KP_Equals)),
      (Output,   To_US ("Code_F13"),                    To_Key_Code (Scan_Code_F13)),
      (Output,   To_US ("Code_F14"),                    To_Key_Code (Scan_Code_F14)),
      (Output,   To_US ("Code_F15"),                    To_Key_Code (Scan_Code_F15)),
      (Output,   To_US ("Code_F16"),                    To_Key_Code (Scan_Code_F16)),
      (Output,   To_US ("Code_F17"),                    To_Key_Code (Scan_Code_F17)),
      (Output,   To_US ("Code_F18"),                    To_Key_Code (Scan_Code_F18)),
      (Output,   To_US ("Code_F19"),                    To_Key_Code (Scan_Code_F19)),
      (Output,   To_US ("Code_F20"),                    To_Key_Code (Scan_Code_F20)),
      (Output,   To_US ("Code_F21"),                    To_Key_Code (Scan_Code_F21)),
      (Output,   To_US ("Code_F22"),                    To_Key_Code (Scan_Code_F22)),
      (Output,   To_US ("Code_F23"),                    To_Key_Code (Scan_Code_F23)),
      (Output,   To_US ("Code_F24"),                    To_Key_Code (Scan_Code_F24)),
      (Output,   To_US ("Code_Execute"),                To_Key_Code (Scan_Code_Execute)),
      (Output,   To_US ("Code_Help"),                   To_Key_Code (Scan_Code_Help)),
      (Output,   To_US ("Code_Menu"),                   To_Key_Code (Scan_Code_Menu)),
      (Output,   To_US ("Code_Select"),                 To_Key_Code (Scan_Code_Select)),
      (Output,   To_US ("Code_Stop"),                   To_Key_Code (Scan_Code_Stop)),
      (Output,   To_US ("Code_Again"),                  To_Key_Code (Scan_Code_Again)),
      (Output,   To_US ("Code_Undo"),                   To_Key_Code (Scan_Code_Undo)),
      (Output,   To_US ("Code_Cut"),                    To_Key_Code (Scan_Code_Cut)),
      (Output,   To_US ("Code_Copy"),                   To_Key_Code (Scan_Code_Copy)),
      (Output,   To_US ("Code_Paste"),                  To_Key_Code (Scan_Code_Paste)),
      (Output,   To_US ("Code_Find"),                   To_Key_Code (Scan_Code_Find)),
      (Output,   To_US ("Code_Mute"),                   To_Key_Code (Scan_Code_Mute)),
      (Output,   To_US ("Code_Volume_Up"),              To_Key_Code (Scan_Code_Volume_Up)),
      (Output,   To_US ("Code_Volume_Down"),            To_Key_Code (Scan_Code_Volume_Down)),
      (Output,   To_US ("Code_KP_Comma"),               To_Key_Code (Scan_Code_KP_Comma)),
      (Output,   To_US ("Code_KP_Equals_AS400"),        To_Key_Code (Scan_Code_KP_Equals_AS400)),
      (New_Line_Code),
      (Output,   To_US ("Code_Alt_Erase"),              To_Key_Code (Scan_Code_Alt_Erase)),
      (Output,   To_US ("Code_Sys_Req"),                To_Key_Code (Scan_Code_Sys_Req)),
      (Output,   To_US ("Code_Cancel"),                 To_Key_Code (Scan_Code_Cancel)),
      (Output,   To_US ("Code_Clear"),                  To_Key_Code (Scan_Code_Clear)),
      (Output,   To_US ("Code_Prior"),                  To_Key_Code (Scan_Code_Prior)),
      (Output,   To_US ("Code_Return_2"),               To_Key_Code (Scan_Code_Return_2)),
      (Output,   To_US ("Code_Separator"),              To_Key_Code (Scan_Code_Separator)),
      (Output,   To_US ("Code_Out"),                    To_Key_Code (Scan_Code_Out)),
      (Output,   To_US ("Code_Oper"),                   To_Key_Code (Scan_Code_Oper)),
      (Output,   To_US ("Code_Clear_Again"),            To_Key_Code (Scan_Code_Clear_Again)),
      (Output,   To_US ("Code_CR_Sel"),                 To_Key_Code (Scan_Code_CR_Sel)),
      (Output,   To_US ("Code_Ex_Sel"),                 To_Key_Code (Scan_Code_EX_Sel)),
      (New_Line_Code),
      (Output,   To_US ("Code_KP_00"),                  To_Key_Code (Scan_Code_KP_00)),
      (Output,   To_US ("Code_KP_000"),                 To_Key_Code (Scan_Code_KP_000)),
      (Output,   To_US ("Code_Thousands_Separator"),    To_Key_Code (Scan_Code_Thousands_Separator)),
      (Output,   To_US ("Code_Decimal_Separator"),      To_Key_Code (Scan_Code_Decimal_Separator)),
      (Output,   To_US ("Code_Currency_Unit"),          To_Key_Code (Scan_Code_Currency_Unit)),
      (Output,   To_US ("Code_Currency_Subunit"),       To_Key_Code (Scan_Code_Currency_Subunit)),
      (Output,   To_US ("Code_KP_Left_Parenthesis"),    To_Key_Code (Scan_Code_KP_Left_Parenthesis)),
      (Output,   To_US ("Code_KP_Right_Parentheesis"),  To_Key_Code (Scan_Code_KP_Right_Parentheesis)),
      (Output,   To_US ("Code_KP_Left_Brace"),          To_Key_Code (Scan_Code_KP_Left_Brace)),
      (Output,   To_US ("Code_KP_Right_Brace"),         To_Key_Code (Scan_Code_KP_Right_Brace)),
      (Output,   To_US ("Code_KP_Tab"),                 To_Key_Code (Scan_Code_KP_Tab)),
      (Output,   To_US ("Code_KP_Backspace"),           To_Key_Code (Scan_Code_KP_Backspace)),
      (Output,   To_US ("Code_KP_A"),                   To_Key_Code (Scan_Code_KP_A)),
      (Output,   To_US ("Code_KP_B"),                   To_Key_Code (Scan_Code_KP_B)),
      (Output,   To_US ("Code_KP_C"),                   To_Key_Code (Scan_Code_KP_C)),
      (Output,   To_US ("Code_KP_D"),                   To_Key_Code (Scan_Code_KP_D)),
      (Output,   To_US ("Code_KP_E"),                   To_Key_Code (Scan_Code_KP_E)),
      (Output,   To_US ("Code_KP_F"),                   To_Key_Code (Scan_Code_KP_F)),
      (Output,   To_US ("Code_KP_XOR"),                 To_Key_Code (Scan_Code_KP_XOR)),
      (Output,   To_US ("Code_KP_Power"),               To_Key_Code (Scan_Code_KP_Power)),
      (Output,   To_US ("Code_KP_Percent"),             To_Key_Code (Scan_Code_KP_Percent)),
      (Output,   To_US ("Code_KP_Less"),                To_Key_Code (Scan_Code_KP_Less)),
      (Output,   To_US ("Code_KP_Greater"),             To_Key_Code (Scan_Code_KP_Greater)),
      (Output,   To_US ("Code_KP_Ampersand"),           To_Key_Code (Scan_Code_KP_Ampersand)),
      (Output,   To_US ("Code_KP_Double_Ampersand"),    To_Key_Code (Scan_Code_KP_Double_Ampersand)),
      (Output,   To_US ("Code_KP_Vertical_Bar"),        To_Key_Code (Scan_Code_KP_Vertical_Bar)),
      (Output,   To_US ("Code_KP_Double_Vertical_Bar"), To_Key_Code (Scan_Code_KP_Double_Vertical_Bar)),
      (Output,   To_US ("Code_KP_Colon"),               To_Key_Code (Scan_Code_KP_Colon)),
      (Output,   To_US ("Code_KP_Hash"),                To_Key_Code (Scan_Code_KP_Hash)),
      (Output,   To_US ("Code_KP_Space"),               To_Key_Code (Scan_Code_KP_Space)),
      (Output,   To_US ("Code_KP_At"),                  To_Key_Code (Scan_Code_KP_At)),
      (Output,   To_US ("Code_KP_Exclamation"),         To_Key_Code (Scan_Code_KP_Exclamation)),
      (Output,   To_US ("Code_KP_Memory_Store"),        To_Key_Code (Scan_Code_KP_Memory_Store)),
      (Output,   To_US ("Code_KP_Memory_Recall"),       To_Key_Code (Scan_Code_KP_Memory_Recall)),
      (Output,   To_US ("Code_KP_Memory_Clear"),        To_Key_Code (Scan_Code_KP_Memory_Clear)),
      (Output,   To_US ("Code_KP_Memory_Add"),          To_Key_Code (Scan_Code_KP_Memory_Add)),
      (Output,   To_US ("Code_KP_Memory_Subtract"),     To_Key_Code (Scan_Code_KP_Memory_Subtract)),
      (Output,   To_US ("Code_KP_Memory_Multiply"),     To_Key_Code (Scan_Code_KP_Memory_Multiply)),
      (Output,   To_US ("Code_KP_Memory_Divide"),       To_Key_Code (Scan_Code_KP_Memory_Divide)),
      (Output,   To_US ("Code_KP_Plus_Minus"),          To_Key_Code (Scan_Code_KP_Plus_Minus)),
      (Output,   To_US ("Code_KP_Clear"),               To_Key_Code (Scan_Code_KP_Clear)),
      (Output,   To_US ("Code_KP_Clear_Entry"),         To_Key_Code (Scan_Code_KP_Clear_Entry)),
      (Output,   To_US ("Code_KP_Binary"),              To_Key_Code (Scan_Code_KP_Binary)),
      (Output,   To_US ("Code_KP_Octal"),               To_Key_Code (Scan_Code_KP_Octal)),
      (Output,   To_US ("Code_KP_Decimal"),             To_Key_Code (Scan_Code_KP_Decimal)),
      (Output,   To_US ("Code_KP_Hexadecimal"),         To_Key_Code (Scan_Code_KP_Hexadecimal)),
      (New_Line_Code),
      (Output,   To_US ("Code_Left_Control"),           To_Key_Code (Scan_Code_Left_Control)),
      (Output,   To_US ("Code_Left_Shift"),             To_Key_Code (Scan_Code_Left_Shift)),
      (Output,   To_US ("Code_Left_Alt"),               To_Key_Code (Scan_Code_Left_Alt)),
      (Output,   To_US ("Code_Left_GUI"),               To_Key_Code (Scan_Code_Left_GUI)),
      (Output,   To_US ("Code_Right_Control"),          To_Key_Code (Scan_Code_Right_Control)),
      (Output,   To_US ("Code_Right_Shift"),            To_Key_Code (Scan_Code_Right_Shift)),
      (Output,   To_US ("Code_Right_Alt"),              To_Key_Code (Scan_Code_Right_Alt)),
      (Output,   To_US ("Code_Right_GUI"),              To_Key_Code (Scan_Code_Right_GUI)),
      (New_Line_Code),
      (Output,   To_US ("Code_Mode"),                   To_Key_Code (Scan_Code_Mode)),
      (New_Line_Code),
      (Output,   To_US ("Code_Audio_Next"),             To_Key_Code (Scan_Code_Audio_Next)),
      (Output,   To_US ("Code_Audio_Previous"),         To_Key_Code (Scan_Code_Audio_Previous)),
      (Output,   To_US ("Code_Audio_Stop"),             To_Key_Code (Scan_Code_Audio_Stop)),
      (Output,   To_US ("Code_Audio_Play"),             To_Key_Code (Scan_Code_Audio_Play)),
      (Output,   To_US ("Code_Audio_Mute"),             To_Key_Code (Scan_Code_Audio_Mute)),
      (Output,   To_US ("Code_Media_Select"),           To_Key_Code (Scan_Code_Media_Select)),
      (Output,   To_US ("Code_WWW"),                    To_Key_Code (Scan_Code_WWW)),
      (Output,   To_US ("Code_Mail"),                   To_Key_Code (Scan_Code_Mail)),
      (Output,   To_US ("Code_Calculator"),             To_Key_Code (Scan_Code_Calculator)),
      (Output,   To_US ("Code_Computer"),               To_Key_Code (Scan_Code_Computer)),
      (Output,   To_US ("Code_AC_Search"),              To_Key_Code (Scan_Code_AC_Search)),
      (Output,   To_US ("Code_AC_Home"),                To_Key_Code (Scan_Code_AC_Home)),
      (Output,   To_US ("Code_AC_Back"),                To_Key_Code (Scan_Code_AC_Back)),
      (Output,   To_US ("Code_AC_Forward"),             To_Key_Code (Scan_Code_AC_Forward)),
      (Output,   To_US ("Code_AC_Stop"),                To_Key_Code (Scan_Code_AC_Stop)),
      (Output,   To_US ("Code_AC_Refresh"),             To_Key_Code (Scan_Code_AC_Refresh)),
      (Output,   To_US ("Code_AC_Bookmarks"),           To_Key_Code (Scan_Code_AC_Bookmarks)),
      (New_Line_Code),
      (Output,   To_US ("Code_Brightness_Down"),        To_Key_Code (Scan_Code_Brightness_Down)),
      (Output,   To_US ("Code_Brightness_Up"),          To_Key_Code (Scan_Code_Brightness_Up)),
      (Output,   To_US ("Code_Display_Switch"),         To_Key_Code (Scan_Code_Display_Switch)),
      (Output,   To_US ("Code_Illumination_Toggle"),    To_Key_Code (Scan_Code_Illumination_Toggle)),
      (Output,   To_US ("Code_Illumination_Down"),      To_Key_Code (Scan_Code_Illumination_Down)),
      (Output,   To_US ("Code_Illumination_Up"),        To_Key_Code (Scan_Code_Illumination_Up)),
      (Output,   To_US ("Code_Eject"),                  To_Key_Code (Scan_Code_Eject)),
      (Output,   To_US ("Code_Sleep"),                  To_Key_Code (Scan_Code_Sleep)));
begin
   Comment (Indent   => 0,
            Text     => "Automatically generated, do not edit.");

   Comment_Dash (117);

   for Line of License loop
      Comment (Indent => 0, Text => To_String (Line));
   end loop;

   Comment_Dash (117);

   for Line of Package_Description loop
      Comment (Indent => 0, Text => To_String (Line));
   end loop;

   Comment_Dash (117);

   Put_Line ("package SDL.Events.Keyboards is");

   Put_Line ("   --  Keyboard events.");
   Put_Line ("   Key_Down                         : constant Event_Types := 16#0000_0300#;");
   Put_Line ("   Key_Up                           : constant Event_Types := Key_Down + 1;");
   Put_Line ("   Text_Editing                     : constant Event_Types := Key_Down + 2;");
   Put_Line ("   Text_Input                       : constant Event_Types := Key_Down + 3;");
   New_Line;

   --  Output the scan codes.
   Comment_Dash (Total => 114, Indent => 3);
   Put_Line ("   --  Scan codes.");
   Comment_Dash (Total => 114, Indent => 3);
   Put_Line ("   type Scan_Codes is range 0 .. 512 with");
   Put_Line ("     Convention => C,");
   Put_Line ("     Size       => 32;");
   New_Line;

   for Code of Scan_Code_Table loop
      case Code.State is
         when Output =>
            Output_Field (Text => To_String (Code.Name), Width => 33, Indent => 3);
            Put (": constant Scan_Codes := ");
            Put (Code.Code, Width => 0);
            Put (Latin_1.Semicolon);

            if Code.Comment /= Null_Unbounded_String then
               Put ("  --  " & To_String (Code.Comment));
            end if;

            New_Line;

         when New_Line =>
            New_Line;

         when Comment =>
            Comment (Indent => 3, Text => To_String (Code.Comment));
      end case;
   end loop;

   New_Line;
   Put_Line ("   function Value (Name : in String) return SDL.Events.Keyboards.Scan_Codes with");
   Put_Line ("     Inline => True;");
   New_Line;
   Put_Line ("   function Image (Scan_Code : in SDL.Events.Keyboards.Scan_Codes) return String with");
   Put_Line ("     Inline => True;");
   New_Line;

   Comment_Dash (Total => 114, Indent => 3);
   Comment (Indent => 3, Text => "Key codes.");
   Comment_Dash (Total => 114, Indent => 3);

   Put_Line ("   type Key_Codes is mod 2 ** 32 with");
   Put_Line ("     Convention => C,");
   Put_Line ("     Size       => 32;");
   New_Line;

   for Code of Key_Code_Table loop
      case Code.State is
         when Output =>
            Output_Field (Text => To_String (Code.Name), Width => 33, Indent => 3);
            Put (": constant Key_Codes := ");
            Put (Code.Code, Width => 12, Base => 16);
            Put (Latin_1.Semicolon);
            New_Line;

         when New_Line =>
            New_Line;

         when Comment =>
            Comment (Indent => 3, Text => To_String (Code.Name));
      end case;
   end loop;

   New_Line;
   Put_Line ("   function Value (Name : in String) return SDL.Events.Keyboards.Key_Codes with");
   Put_Line ("     Inline => True;");
   New_Line;
   Put_Line ("   function Image (Key_Code : in SDL.Events.Keyboards.Key_Codes) return String with");
   Put_Line ("     Inline => True;");
   New_Line;
   Put_Line ("   function To_Key_Code (Scan_Code : in SDL.Events.Keyboards.Scan_Codes) return " &
               "SDL.Events.Keyboards.Key_Codes with");
   Put_Line ("     Inline => True;");
   New_Line;
   Put_Line ("   function To_Scan_Code (Key_Code : in SDL.Events.Keyboards.Key_Codes) return " &
               "SDL.Events.Keyboards.Scan_Codes with");
   Put_Line ("     Inline => True;");
   New_Line;

   Comment_Dash (Total => 114, Indent => 3);
   Comment (Indent => 3, Text => "Key modifiers.");
   Comment_Dash (Total => 114, Indent => 3);
   Put_Line ("   type Key_Modifiers is mod 2 ** 16 with");
   Put_Line ("     Convention => C,");
   Put_Line ("     Size       => 16;");
   New_Line;
   Put_Line ("   Modifier_None          : constant Key_Modifiers := 16#00_00#;");
   Put_Line ("   Modifier_Left_Shift    : constant Key_Modifiers := 16#00_01#;");
   Put_Line ("   Modifier_Right_Shift   : constant Key_Modifiers := 16#00_02#;");
   Put_Line ("   Modifier_Left_Control  : constant Key_Modifiers := 16#00_40#;");
   Put_Line ("   Modifier_Right_Control : constant Key_Modifiers := 16#00_80#;");
   Put_Line ("   Modifier_Left_Alt      : constant Key_Modifiers := 16#01_00#;");
   Put_Line ("   Modifier_Right_Alt     : constant Key_Modifiers := 16#02_00#;");
   Put_Line ("   Modifier_Left_GUI      : constant Key_Modifiers := 16#04_00#;");
   Put_Line ("   Modifier_Right_GUI     : constant Key_Modifiers := 16#08_00#;");
   Put_Line ("   Modifier_Num           : constant Key_Modifiers := 16#10_00#;");
   Put_Line ("   Modifier_Caps          : constant Key_Modifiers := 16#20_00#;");
   Put_Line ("   Modifier_Mode          : constant Key_Modifiers := 16#40_00#;");
   Put_Line ("   Modifier_Control       : constant Key_Modifiers := Modifier_Left_Control or Modifier_Right_Control;");
   Put_Line ("   Modifier_Shift         : constant Key_Modifiers := Modifier_Left_Shift or Modifier_Right_Shift;");
   Put_Line ("   Modifier_Alt           : constant Key_Modifiers := Modifier_Left_Alt or Modifier_Right_Alt;");
   Put_Line ("   Modifier_GUI           : constant Key_Modifiers := Modifier_Left_GUI or Modifier_Right_GUI;");
   Put_Line ("   Modifier_Reserved      : constant Key_Modifiers := 16#80_00#;");
   New_Line;
   Put_Line ("   type Key_Syms is");
   Put_Line ("      record");
   Put_Line ("         Scan_Code : Scan_Codes;");
   Put_Line ("         Key_Code  : Key_Codes;");
   Put_Line ("         Modifiers : Key_Modifiers;");
   Put_Line ("         Unused    : Interfaces.Unsigned_32;");
   Put_Line ("      end record with");
   Put_Line ("     Convention => C;");
   New_Line;
   Put_Line ("   type Keyboard_Events is");
   Put_Line ("      record");
   Put_Line ("         Event_Type : Event_Types;           --  Will be set to Key_Up/Down.");
   Put_Line ("         Time_Stamp : Time_Stamps;");
   New_Line;
   Put_Line ("         ID         : SDL.Video.Windows.ID;");
   Put_Line ("         State      : Button_State;");
   Put_Line ("         Repeat     : Interfaces.Unsigned_8;");
   Put_Line ("         Padding_2  : Padding_8;");
   Put_Line ("         Padding_3  : Padding_8;");
   Put_Line ("         Key_Sym    : Key_Syms;");
   Put_Line ("      end record with");
   Put_Line ("     Convention => C;");
   New_Line;

   Comment_Dash (Total => 114, Indent => 3);
   Comment (Indent => 3, Text => "Text editing events.");
   Comment_Dash (Total => 114, Indent => 3);
   Put_Line ("   Max_UTF8_Elements             : constant := 31;");
   Put_Line ("   Max_UTF8_Element_Storage_Bits : constant := ((Max_UTF8_Elements + 1) * 8) - 1;");
   New_Line;
   Put_Line ("   subtype UTF8_Text_Buffers is Interfaces.C.char_array (0 .. Max_UTF8_Elements);");
   New_Line;
   Put_Line ("   type Cursor_Positions is range -2 ** 31 .. 2 ** 31 - 1 with");
   Put_Line ("     Convention => C,");
   Put_Line ("     Size       => 32;");
   New_Line;
   Put_Line ("   type Text_Lengths is range -2 ** 31 .. 2 ** 31 - 1 with");
   Put_Line ("     Convention => C,");
   Put_Line ("     Size       => 32;");
   New_Line;
   Put_Line ("   type Text_Editing_Events is");
   Put_Line ("      record");
   Put_Line ("         Event_Type : Event_Types;           --  Will be set to Text_Editing.");
   Put_Line ("         Time_Stamp : Time_Stamps;");
   New_Line;
   Put_Line ("         ID         : SDL.Video.Windows.ID;");
   Put_Line ("         Text       : UTF8_Text_Buffers;");
   Put_Line ("         Start      : Cursor_Positions;      --  TODO: Find out why this needs to be a signed value!");
   Put_Line ("         Length     : Text_Lengths;          --  TODO: Again, signed, why?");
   Put_Line ("      end record with");
   Put_Line ("     Convention => C;");
   New_Line;

   Comment_Dash (Total => 114, Indent => 3);
   Comment (Indent => 3, Text => "Text input events.");
   Comment_Dash (Total => 114, Indent => 3);
   Put_Line ("   type Text_Input_Events is");
   Put_Line ("      record");
   Put_Line ("         Event_Type : Event_Types;           --  Will be set to Text_Editing.");
   Put_Line ("         Time_Stamp : Time_Stamps;");
   New_Line;
   Put_Line ("         ID         : SDL.Video.Windows.ID;");
   Put_Line ("         Text       : UTF8_Text_Buffers;");
   Put_Line ("      end record with");
   Put_Line ("     Convention => C;");
   New_Line;

   Put_Line ("private");
   Put_Line ("   for Key_Syms use");
   Put_Line ("      record");
   Put_Line ("         Scan_Code at 0 * SDL.Word range 0 .. 31;");
   Put_Line ("         Key_Code  at 1 * SDL.Word range 0 .. 31;");
   Put_Line ("         Modifiers at 2 * SDL.Word range 0 .. 15;");
   Put_Line ("         Unused    at 3 * SDL.Word range 0 .. 31;");
   Put_Line ("      end record;");
   New_Line;
   Put_Line ("   for Keyboard_Events use");
   Put_Line ("      record");
   Put_Line ("         Event_Type at 0 * SDL.Word range  0  .. 31;");
   Put_Line ("         Time_Stamp at 1 * SDL.Word range  0  .. 31;");
   New_Line;
   Put_Line ("         ID         at 2 * SDL.Word range  0  .. 31;");
   Put_Line ("         State      at 3 * SDL.Word range  0  ..  7;");
   Put_Line ("         Repeat     at 3 * SDL.Word range  8  .. 15;");
   Put_Line ("         Padding_2  at 3 * SDL.Word range  16 .. 23;");
   Put_Line ("         Padding_3  at 3 * SDL.Word range  24 .. 31;");
   Put_Line ("      end record;");
   New_Line;
   Put_Line ("   for Text_Editing_Events use");
   Put_Line ("      record");
   Put_Line ("         Event_Type at  0 * SDL.Word range  0  .. 31;");
   Put_Line ("         Time_Stamp at  1 * SDL.Word range  0  .. 31;");
   New_Line;
   Put_Line ("         ID         at  2 * SDL.Word range  0  .. 31;");
   Put_Line ("         Text       at  3 * SDL.Word range  0  .. Max_UTF8_Element_Storage_Bits; -- 31 characters.");
   Put_Line ("         Start      at 11 * SDL.Word range  0  .. 31;");
   Put_Line ("         Length     at 12 * SDL.Word range  0  .. 31;");
   Put_Line ("      end record;");
   New_Line;
   Put_Line ("   for Text_Input_Events use");
   Put_Line ("      record");
   Put_Line ("         Event_Type at  0 * SDL.Word range  0  .. 31;");
   Put_Line ("         Time_Stamp at  1 * SDL.Word range  0  .. 31;");
   New_Line;
   Put_Line ("         ID         at  2 * SDL.Word range  0  .. 31;");
   Put_Line ("         Text       at  3 * SDL.Word range  0  .. Max_UTF8_Element_Storage_Bits; -- 31 characters.");
   Put_Line ("      end record;");
   Put_Line ("end SDL.Events.Keyboards;");
end Gen_Keyboard;
