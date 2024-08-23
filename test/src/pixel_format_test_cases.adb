--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  Pixel_Format_Test_Cases
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;
with SDL.Video.Pixel_Formats; use SDL.Video.Pixel_Formats;
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Text_IO; -- use Ada.Text_Io;

package body Pixel_Format_Test_Cases is
   overriding
   function Name (Test : Pixel_Format_Test_Case) return Message_String is
      pragma Unreferenced (Test);  --  TODO: Fix me!
   begin
      return Format ("Pixel format test");
   end Name;

   use type C.int;

   function To_int is new Ada.Unchecked_Conversion (Source => Pixel_Format_Names, Target => C.int);
   package int_IO is new Ada.Text_IO.Integer_Io (C.int);
   use int_IO;

   function To_Binary (Num : in C.int) return String is
      Result : String (1 .. 100);
   begin
      Put (Result, Num, 2);

      return Trim (Result, Left);
   end To_Binary;

   overriding
   procedure Run_Test (Test : in out Pixel_Format_Test_Case) is
      pragma Unreferenced (Test);  --  TODO: Fix me!
   begin
      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_Unknown));
         C_Value   : constant String := To_Binary (C_Unknown);
         Error     : constant String :=
           "Pixel_Format_Unknown (" & Ada_Value & ") /= C_Index_Unknown (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("Unknown = " & To_int (Pixel_Format_Unknown)'Image &
                               " (" & C_Unknown'Image & ") - " &
                               Image (Pixel_Format_Unknown));

         Assert (To_int (Pixel_Format_Unknown) = C_Unknown, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_Index_1_LSB));
         C_Value   : constant String := To_Binary (C_Index_1_LSB);
         Error     : constant String :=
           "Pixel_Format_Index_1_LSB (" & Ada_Value & ") /= C_Index_1_LSB (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("Index_1_LSB = " & To_int (Pixel_Format_Index_1_LSB)'Image &
                               " (" & C_Index_1_LSB'Image & ") - " &
                               Image (Pixel_Format_Index_1_LSB));

         Assert (To_int (Pixel_Format_Index_1_LSB) = C_Index_1_LSB, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_Index_1_MSB));
         C_Value   : constant String := To_Binary (C_Index_1_MSB);
         Error     : constant String :=
           "Pixel_Format_Index_1_MSB (" & Ada_Value & ") /= C_Index_1_MSB (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("Index_1_MSB = " & To_int (Pixel_Format_Index_1_MSB)'Image &
                               " (" & C_Index_1_MSB'Image & ") - " &
                               Image (Pixel_Format_Index_1_MSB));

         Assert (To_int (Pixel_Format_Index_1_MSB) = C_Index_1_MSB, Error);
      end;


      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_Index_4_LSB));
         C_Value   : constant String := To_Binary (C_Index_4_LSB);
         Error     : constant String :=
           "Pixel_Format_Index_4_LSB (" & Ada_Value & ") /= C_Index_4_LSB (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("Index_4_LSB = " & To_int (Pixel_Format_Index_4_LSB)'Image &
                               " (" & C_Index_4_LSB'Image & ") - " &
                               Image (Pixel_Format_Index_4_LSB));

         Assert (To_int (Pixel_Format_Index_4_LSB) = C_Index_4_LSB, Error);
      end;

      --  Put (To => Ada_Value, Item => To_int (Pixel_Format_Index_4_MSB), Base => 16);
      --  Put (To => C_Value, Item => C_Index_4_MSB, Base => 16);

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_Index_4_MSB));
         C_Value   : constant String := To_Binary (C_Index_4_MSB);
         Error     : constant String :=
           "Pixel_Format_Index_4_MSB (" & Ada_Value & ") /= C_Index_4_MSB (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("Index_4_MSB = " & To_int (Pixel_Format_Index_4_MSB)'Image &
                               " (" & C_Index_4_MSB'Image & ") - " &
                               Image (Pixel_Format_Index_4_MSB));

         Assert (To_int (Pixel_Format_Index_4_MSB) = C_Index_4_MSB, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_Index_8));
         C_Value   : constant String := To_Binary (C_Index_8);
         Error     : constant String :=
           "Pixel_Format_Index_8 (" & Ada_Value & ") /= C_Index_8 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("Index_8 = " & To_int (Pixel_Format_Index_8)'Image &
                               " (" & C_Index_8'Image & ") - " &
                               Image (Pixel_Format_Index_8));

         Assert (To_int (Pixel_Format_Index_8) = C_Index_8, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGB_332));
         C_Value   : constant String := To_Binary (C_RGB_332);
         Error     : constant String :=
           "Pixel_Format_RGB_332 (" & Ada_Value & ") /= C_RGB_332 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGB_332 = " & To_int (Pixel_Format_RGB_332)'Image &
                               " (" & C_RGB_332'Image & ") - " &
                               Image (Pixel_Format_RGB_332));

         Assert (To_int (Pixel_Format_RGB_332) = C_RGB_332, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGB_444));
         C_Value   : constant String := To_Binary (C_RGB_444);
         Error     : constant String :=
           "Pixel_Format_RGB_444 (" & Ada_Value & ") /= C_RGB_444 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGB_444 = " & To_int (Pixel_Format_RGB_444)'Image &
                               " (" & C_RGB_444'Image & ") - " &
                               Image (Pixel_Format_RGB_444));

         Assert (To_int (Pixel_Format_RGB_444) = C_RGB_444, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGB_555));
         C_Value   : constant String := To_Binary (C_RGB_555);
         Error     : constant String :=
           "Pixel_Format_RGB_555 (" & Ada_Value & ") /= C_RGB_555 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGB_555 = " & To_int (Pixel_Format_RGB_555)'Image &
                               " (" & C_RGB_555'Image & ") - " &
                               Image (Pixel_Format_RGB_555));

         Assert (To_int (Pixel_Format_RGB_555) = C_RGB_555, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGR_555));
         C_Value   : constant String := To_Binary (C_BGR_555);
         Error     : constant String :=
           "Pixel_Format_BGR_555 (" & Ada_Value & ") /= C_BGR_555 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGR_555 = " & To_int (Pixel_Format_BGR_555)'Image &
                               " (" & C_BGR_555'Image & ") - " &
                               Image (Pixel_Format_BGR_555));

         Assert (To_int (Pixel_Format_BGR_555) = C_BGR_555, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ARGB_4444));
         C_Value   : constant String := To_Binary (C_ARGB_4444);
         Error     : constant String :=
           "Pixel_Format_ARGB_4444 (" & Ada_Value & ") /= C_ARGB_4444 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ARGB_4444 = " & To_int (Pixel_Format_ARGB_4444)'Image &
                               " (" & C_ARGB_4444'Image & ") - " &
                               Image (Pixel_Format_ARGB_4444));

         Assert (To_int (Pixel_Format_ARGB_4444) = C_ARGB_4444, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGBA_4444));
         C_Value   : constant String := To_Binary (C_RGBA_4444);
         Error     : constant String :=
           "Pixel_Format_RGBA_4444 (" & Ada_Value & ") /= C_RGBA_4444 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGBA_4444 = " & To_int (Pixel_Format_RGBA_4444)'Image &
                               " (" & C_RGBA_4444'Image & ") - " &
                               Image (Pixel_Format_RGBA_4444));

         Assert (To_int (Pixel_Format_RGBA_4444) = C_RGBA_4444, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ABGR_4444));
         C_Value   : constant String := To_Binary (C_ABGR_4444);
         Error     : constant String :=
           "Pixel_Format_ABGR_4444 (" & Ada_Value & ") /= C_ABGR_4444 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ABGR_4444 = " & To_int (Pixel_Format_ABGR_4444)'Image &
                               " (" & C_ABGR_4444'Image & ") - " &
                               Image (Pixel_Format_ABGR_4444));

         Assert (To_int (Pixel_Format_ABGR_4444) = C_ABGR_4444, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGRA_4444));
         C_Value   : constant String := To_Binary (C_BGRA_4444);
         Error     : constant String :=
           "Pixel_Format_BGRA_4444 (" & Ada_Value & ") /= C_BGRA_4444 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGRA_4444 = " & To_int (Pixel_Format_BGRA_4444)'Image &
                               " (" & C_BGRA_4444'Image & ") - " &
                               Image (Pixel_Format_BGRA_4444));

         Assert (To_int (Pixel_Format_BGRA_4444) = C_BGRA_4444, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ARGB_1555));
         C_Value   : constant String := To_Binary (C_ARGB_1555);
         Error     : constant String :=
           "Pixel_Format_ARGB_1555 (" & Ada_Value & ") /= C_ARGB_1555 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ARGB_1555 = " & To_int (Pixel_Format_ARGB_1555)'Image &
                               " (" & C_ARGB_1555'Image & ") - " &
                               Image (Pixel_Format_ARGB_1555));

         Assert (To_int (Pixel_Format_ARGB_1555) = C_ARGB_1555, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGBA_5551));
         C_Value   : constant String := To_Binary (C_RGBA_5551);
         Error     : constant String :=
           "Pixel_Format_RGBA_5551 (" & Ada_Value & ") /= C_RGBA_5551 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGBA_5551 = " & To_int (Pixel_Format_RGBA_5551)'Image &
                               " (" & C_RGBA_5551'Image & ") - " &
                               Image (Pixel_Format_RGBA_5551));

         Assert (To_int (Pixel_Format_RGBA_5551) = C_RGBA_5551, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ABGR_1555));
         C_Value   : constant String := To_Binary (C_ABGR_1555);
         Error     : constant String :=
           "Pixel_Format_ABGR_1555 (" & Ada_Value & ") /= C_ABGR_1555 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ABGR_1555 = " & To_int (Pixel_Format_ABGR_1555)'Image &
                               " (" & C_ABGR_1555'Image & ") - " &
                               Image (Pixel_Format_ABGR_1555));

         Assert (To_int (Pixel_Format_ABGR_1555) = C_ABGR_1555, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGRA_5551));
         C_Value   : constant String := To_Binary (C_BGRA_5551);
         Error     : constant String :=
           "Pixel_Format_BGRA_5551 (" & Ada_Value & ") /= C_BGRA_5551 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGRA_5551 = " & To_int (Pixel_Format_BGRA_5551)'Image &
                               " (" & C_BGRA_5551'Image & ") - " &
                               Image (Pixel_Format_BGRA_5551));

         Assert (To_int (Pixel_Format_BGRA_5551) = C_BGRA_5551, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGB_565));
         C_Value   : constant String := To_Binary (C_RGB_565);
         Error     : constant String :=
           "Pixel_Format_RGB_565 (" & Ada_Value & ") /= C_RGB_565 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGB_565 = " & To_int (Pixel_Format_RGB_565)'Image &
                               " (" & C_RGB_565'Image & ") - " &
                               Image (Pixel_Format_RGB_565));

         Assert (To_int (Pixel_Format_RGB_565) = C_RGB_565, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGR_565));
         C_Value   : constant String := To_Binary (C_BGR_565);
         Error     : constant String :=
           "Pixel_Format_BGR_565 (" & Ada_Value & ") /= C_BGR_565 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGR_565 = " & To_int (Pixel_Format_BGR_565)'Image &
                               " (" & C_BGR_565'Image & ") - " &
                               Image (Pixel_Format_BGR_565));

         Assert (To_int (Pixel_Format_BGR_565) = C_BGR_565, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGB_24));
         C_Value   : constant String := To_Binary (C_RGB_24);
         Error     : constant String :=
           "Pixel_Format_RGB_24 (" & Ada_Value & ") /= C_RGB_24 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGB_24 = " & To_int (Pixel_Format_RGB_24)'Image &
                               " (" & C_RGB_24'Image & ") - " &
                               Image (Pixel_Format_RGB_24));

         Assert (To_int (Pixel_Format_RGB_24) = C_RGB_24, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGR_24));
         C_Value   : constant String := To_Binary (C_BGR_24);
         Error     : constant String :=
           "Pixel_Format_BGR_24 (" & Ada_Value & ") /= C_BGR_24 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGR_24 = " & To_int (Pixel_Format_BGR_24)'Image &
                               " (" & C_BGR_24'Image & ") - " &
                               Image (Pixel_Format_BGR_24));

         Assert (To_int (Pixel_Format_BGR_24) = C_BGR_24, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGB_888));
         C_Value   : constant String := To_Binary (C_RGB_888);
         Error     : constant String :=
           "Pixel_Format_RGB_888 (" & Ada_Value & ") /= C_RGB_888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGB_888 = " & To_int (Pixel_Format_RGB_888)'Image &
                               " (" & C_RGB_888'Image & ") - " &
                               Image (Pixel_Format_RGB_888));

         Assert (To_int (Pixel_Format_RGB_888) = C_RGB_888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGBX_8888));
         C_Value   : constant String := To_Binary (C_RGBX_8888);
         Error     : constant String :=
           "Pixel_Format_RGBX_8888 (" & Ada_Value & ") /= C_RGBX_8888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGBX_8888 = " & To_int (Pixel_Format_RGBX_8888)'Image &
                               " (" & C_RGBX_8888'Image & ") - " &
                               Image (Pixel_Format_RGBX_8888));

         Assert (To_int (Pixel_Format_RGBX_8888) = C_RGBX_8888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGR_888));
         C_Value   : constant String := To_Binary (C_BGR_888);
         Error     : constant String :=
           "Pixel_Format_BGR_888 (" & Ada_Value & ") /= C_BGR_888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGR_888 = " & To_int (Pixel_Format_BGR_888)'Image &
                               " (" & C_BGR_888'Image & ") - " &
                               Image (Pixel_Format_BGR_888));

         Assert (To_int (Pixel_Format_BGR_888) = C_BGR_888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGRX_8888));
         C_Value   : constant String := To_Binary (C_BGRX_8888);
         Error     : constant String :=
           "Pixel_Format_BGRX_8888 (" & Ada_Value & ") /= C_BGRX_8888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGRX_8888 = " & To_int (Pixel_Format_BGRX_8888)'Image &
                               " (" & C_BGRX_8888'Image & ") - " &
                               Image (Pixel_Format_BGRX_8888));

         Assert (To_int (Pixel_Format_BGRX_8888) = C_BGRX_8888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ARGB_8888));
         C_Value   : constant String := To_Binary (C_ARGB_8888);
         Error     : constant String :=
           "Pixel_Format_ARGB_8888 (" & Ada_Value & ") /= C_ARGB_8888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ARGB_8888 = " & To_int (Pixel_Format_ARGB_8888)'Image &
                               " (" & C_ARGB_8888'Image & ") - " &
                               Image (Pixel_Format_ARGB_8888));

         Assert (To_int (Pixel_Format_ARGB_8888) = C_ARGB_8888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_RGBA_8888));
         C_Value   : constant String := To_Binary (C_RGBA_8888);
         Error     : constant String :=
           "Pixel_Format_RGBA_8888 (" & Ada_Value & ") /= C_RGBA_8888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("RGBA_8888 = " & To_int (Pixel_Format_RGBA_8888)'Image &
                               " (" & C_RGBA_8888'Image & ") - " &
                               Image (Pixel_Format_RGBA_8888));

         Assert (To_int (Pixel_Format_RGBA_8888) = C_RGBA_8888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ABGR_8888));
         C_Value   : constant String := To_Binary (C_ABGR_8888);
         Error     : constant String :=
           "Pixel_Format_ABGR_8888 (" & Ada_Value & ") /= C_ABGR_8888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ABGR_8888 = " & To_int (Pixel_Format_ABGR_8888)'Image &
                               " (" & C_ABGR_8888'Image & ") - " &
                               Image (Pixel_Format_ABGR_8888));

         Assert (To_int (Pixel_Format_ABGR_8888) = C_ABGR_8888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_BGRA_8888));
         C_Value   : constant String := To_Binary (C_BGRA_8888);
         Error     : constant String :=
           "Pixel_Format_BGRA_8888 (" & Ada_Value & ") /= C_BGRA_8888 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("BGRA_8888 = " & To_int (Pixel_Format_BGRA_8888)'Image &
                               " (" & C_BGRA_8888'Image & ") - " &
                               Image (Pixel_Format_BGRA_8888));

         Assert (To_int (Pixel_Format_BGRA_8888) = C_BGRA_8888, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_ARGB_2101010));
         C_Value   : constant String := To_Binary (C_ARGB_2101010);
         Error     : constant String :=
           "Pixel_Format_ARGB_2101010 (" & Ada_Value & ") /= C_ARGB_2101010 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("ARGB_2101010 = " & To_int (Pixel_Format_ARGB_2101010)'Image &
                               " (" & C_ARGB_2101010'Image & ") - " &
                               Image (Pixel_Format_ARGB_2101010));

         Assert (To_int (Pixel_Format_ARGB_2101010) = C_ARGB_2101010, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_YV_12));
         C_Value   : constant String := To_Binary (C_YV_12);
         Error     : constant String :=
           "Pixel_Format_YV_12 (" & Ada_Value & ") /= C_YV_12 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("YV_12 = " & To_int (Pixel_Format_YV_12)'Image &
                               " (" & C_YV_12'Image & ") - " &
                               Image (Pixel_Format_YV_12));

         Assert (To_int (Pixel_Format_YV_12) = C_YV_12, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_IYUV));
         C_Value   : constant String := To_Binary (C_IYUV);
         Error     : constant String :=
           "Pixel_Format_IYUV (" & Ada_Value & ") /= C_IYUV (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("IYUV = " & To_int (Pixel_Format_IYUV)'Image &
                               " (" & C_IYUV'Image & ") - " &
                               Image (Pixel_Format_IYUV));

         Assert (To_int (Pixel_Format_IYUV) = C_IYUV, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_YUY_2));
         C_Value   : constant String := To_Binary (C_YUY_2);
         Error     : constant String :=
           "Pixel_Format_YUY_2 (" & Ada_Value & ") /= C_YUY_2 (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("YUY_2 = " & To_int (Pixel_Format_YUY_2)'Image &
                               " (" & C_YUY_2'Image & ") - " &
                               Image (Pixel_Format_YUY_2));

         Assert (To_int (Pixel_Format_YUY_2) = C_YUY_2, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_UYVY));
         C_Value   : constant String := To_Binary (C_UYVY);
         Error     : constant String :=
           "Pixel_Format_UYVY (" & Ada_Value & ") /= C_UYVY (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("UYVY = " & To_int (Pixel_Format_UYVY)'Image &
                               " (" & C_UYVY'Image & ") - " &
                               Image (Pixel_Format_UYVY));

         Assert (To_int (Pixel_Format_UYVY) = C_UYVY, Error);
      end;

      declare
         Ada_Value : constant String := To_Binary (To_int (Pixel_Format_YVYU));
         C_Value   : constant String := To_Binary (C_YVYU);
         Error     : constant String :=
           "Pixel_Format_YVYU (" & Ada_Value & ") /= C_YVYU (" & C_Value & ")";
      begin
         --  Put_Line (Error);
         Ada.Text_IO.Put_Line ("YVYU = " & To_int (Pixel_Format_YVYU)'Image &
                               " (" & C_YVYU'Image & ") - " &
                               Image (Pixel_Format_YVYU));

         Assert (To_int (Pixel_Format_YVYU) = C_YVYU, Error);
      end;
   end Run_Test;
end Pixel_Format_Test_Cases;
