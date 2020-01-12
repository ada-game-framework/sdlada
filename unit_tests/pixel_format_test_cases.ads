--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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
--  Pixel_Format_Test_Cases
--
--  Tests to check whether the memory layout of the pixel formats is correct.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with AUnit; use AUnit;
with AUnit.Simple_Test_Cases;

package Pixel_Format_Test_Cases is
   type Pixel_Format_Test_Case is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding
   function Name (Test : Pixel_Format_Test_Case) return Message_String;

   overriding
   procedure Run_Test (Test : in out Pixel_Format_Test_Case);
private
   package C renames Interfaces.C;

   C_Unknown : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_unknown";

   C_Index_1_LSB : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_index1lsb";

   C_Index_1_MSB : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_index1msb";

   C_Index_4_LSB : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_index4lsb";

   C_Index_4_MSB : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_index4msb";

   C_Index_8 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_index8";

   C_RGB_332 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgb332";

   C_RGB_444 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgb444";

   C_RGB_555 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgb555";

   C_BGR_555 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgr555";

   C_ARGB_4444 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_argb4444";

   C_RGBA_4444 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgba4444";

   C_ABGR_4444 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_abgr4444";

   C_BGRA_4444 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgra4444";

   C_ARGB_1555 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_argb1555";

   C_RGBA_5551 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgba5551";

   C_ABGR_1555 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_abgr1555";

   C_BGRA_5551 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgra5551";

   C_RGB_565 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgb565";

   C_BGR_565 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgr565";

   C_RGB_24 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgb24";

   C_BGR_24 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgr24";

   C_RGB_888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgb888";

   C_RGBX_8888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgbx8888";

   C_BGR_888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgr888";

   C_BGRX_8888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgrx8888";

   C_ARGB_8888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_argb8888";

   C_RGBA_8888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_rgba8888";

   C_ABGR_8888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_abgr8888";

   C_BGRA_8888 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_bgra8888";

   C_ARGB_2101010 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_argb2101010";

   C_YV_12 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_yv12";

   C_IYUV : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_iyuv";

   C_YUY_2 : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_yuy2";

   C_UYVY : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_uyvy";

   C_YVYU : constant C.int with
     Import        => True,
     Convention    => C,
     External_Name => "c_yvyu";
end Pixel_Format_Test_Cases;
