--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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
--  SDL.Video.Pixel_Formats
--
--  Description of various pixel formats.
--------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Interfaces;
with Interfaces.C;
with SDL.Video.Palettes;

package SDL.Video.Pixel_Formats is
   package C renames Interfaces.C;

   type Pixel_Types is
     (Unknown,
      Index_1,
      Index_4,
      Index_8,
      Packed_8,
      Packed_16,
      Packed_32,
      Array_U8,
      Array_U16,
      Array_U32,
      Array_F16,
      Array_F32) with
     Convention => C;

   --  Bitmap pixel order, high bit -> low bit.
   type Bitmap_Pixel_Order is (None, Little_Endian, Big_Endian) with
     Convention => C;

   --  Packed component order, high bit -> low bit.
   type Packed_Component_Order is
     (None,
      XRGB,
      RGBX,
      ARGB,
      RGBA,
      XBGR,
      BGRX,
      ABGR,
      BGRA) with
     Convention => C;

   --  Array component order, low byte -> high byte.
   type Array_Component_Order is (None, RGB, RGBA, ARGB, BGR, BGRA, ABGR);

   --  Describe how the components are laid out in bit form.
   type Packed_Component_Layout is
     (None,
      Bits_332,
      Bits_4444,
      Bits_1555,
      Bits_5551,
      Bits_565,
      Bits_8888,
      Bits_2101010,
      Bits_1010102) with
     Convention => C;

   type Bits_Per_Pixels is range 0 .. 32 with
     Static_Predicate => Bits_Per_Pixels in 0 | 1 | 4 | 8 | 12 | 15 | 16 | 24 | 32,
     Convention       => C;

   Bits_Per_Pixel_Error : constant Bits_Per_Pixels := 0;

   type Bytes_Per_Pixels is range 0 .. 4 with
     Convention => C;

   Bytes_Per_Pixel_Error : constant Bytes_Per_Pixels := Bytes_Per_Pixels'First;

   --   29 28   24   20   16        8        0
   --  000 1  ptpt popo llll bibibibi bybybyby
   --
   --  or
   --
   --        24       16        8        0
   --  DDDDDDDD CCCCCCCC BBBBBBBB AAAAAAAA

   type Index_Order_Padding is range 0 .. 1 with
     Convention => C;

   type Pixel_Orders (Pixel_Type : Pixel_Types := Unknown) is
      record
         case Pixel_Type is
            when Index_1 | Index_4 | Index_8 =>
               Indexed_Order : Bitmap_Pixel_Order;
               Indexed_Pad   : Index_Order_Padding;

            when Packed_8 | Packed_16 | Packed_32 =>
               Packed_Order  : Packed_Component_Order;

            when Array_U8 | Array_U16 | Array_U32 | Array_F16 | Array_F32 =>
               Array_Order   : Array_Component_Order;

            when others =>
               null;
         end case;
      end record with
     Unchecked_Union => True,
     Convention      => C,
     Size            => 4;

   for Pixel_Orders use
      record
         Indexed_Order at 0 range 0 .. 2; --  This was 2 as that is the max size required but it causes a bit set bug!
         Indexed_Pad   at 0 range 3 .. 3;
         Packed_Order  at 0 range 0 .. 3;
         Array_Order   at 0 range 0 .. 3;
      end record;

   type Planar_Pixels is
      record
         A : Character;
         B : Character;
         C : Character;
         D : Character;
      end record with
     Size            => 32,
     Convention      => C;

   for Planar_Pixels use
      record
         A at 0 range  0 ..  7;
         B at 0 range  8 .. 15;
         C at 0 range 16 .. 23;
         D at 0 range 24 .. 31;
      end record;

   type Non_Planar_Pixel_Padding is range 0 .. 7 with
     Convention => C;

   type Non_Planar_Pixels is
      record
         Bytes_Per_Pixel : Bytes_Per_Pixels;
         Bits_Per_Pixel  : Bits_Per_Pixels;
         Layout          : Packed_Component_Layout;
         Pixel_Order     : Pixel_Orders;
         Pixel_Type      : Pixel_Types;
         Flag            : Boolean;
         Padding         : Non_Planar_Pixel_Padding;
      end record with
     Size            => 32,
     Convention      => C;

   for Non_Planar_Pixels use
      record
         Bytes_Per_Pixel at 0 range  0 ..  7;
         Bits_Per_Pixel  at 0 range  8 .. 15;
         Layout          at 0 range 16 .. 19;
         Pixel_Order     at 0 range 20 .. 23;
         Pixel_Type      at 0 range 24 .. 27;
         Flag            at 0 range 28 .. 28;
         Padding         at 0 range 29 .. 31;
      end record;

   type Pixel_Format_Names (Planar : Boolean := False) is
      record
         case Planar is
            when True =>
               Planar_Format     : Planar_Pixels;
            when False =>
               Non_Planar_Format : Non_Planar_Pixels;
         end case;
      end record with
     Unchecked_Union => True,
     Size            => 32,
     Convention      => C;

   Pixel_Format_Unknown     : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar        => True,
                         Planar_Format => Planar_Pixels'
                           (others => Ada.Characters.Latin_1.NUL));

   Pixel_Format_Index_1_LSB : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Index_1,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type    => Index_1,
                               Indexed_Order => Little_Endian,
                               Indexed_Pad   => Index_Order_Padding'First),
                            Layout          => None,
                            Bits_Per_Pixel  => 1,
                            Bytes_Per_Pixel => 0));

   Pixel_Format_Index_1_MSB : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Index_1,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type    => Index_1,
                               Indexed_Order => Big_Endian,
                               Indexed_Pad   => Index_Order_Padding'First),
                            Layout          => None,
                            Bits_Per_Pixel  => 1,
                            Bytes_Per_Pixel => 0));

   Pixel_Format_Index_4_LSB : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Index_4,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type    => Index_4,
                               Indexed_Order => Little_Endian,
                               Indexed_Pad   => Index_Order_Padding'First),
                            Layout          => None,
                            Bits_Per_Pixel  => 4,
                            Bytes_Per_Pixel => 0));

   Pixel_Format_Index_4_MSB : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Index_4,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type    => Index_4,
                               Indexed_Order => Big_Endian,
                               Indexed_Pad   => Index_Order_Padding'First),
                            Layout          => None,
                            Bits_Per_Pixel  => 4,
                            Bytes_Per_Pixel => 0));

   Pixel_Format_Index_8 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Index_8,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type    => Index_8,
                               Indexed_Order => None,
                               Indexed_Pad   => Index_Order_Padding'First),
                            Layout          => None,
                            Bits_Per_Pixel  => 8,
                            Bytes_Per_Pixel => 1));

   Pixel_Format_RGB_332 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_8,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_8,
                               Packed_Order => XRGB),
                            Layout          => Bits_332,
                            Bits_Per_Pixel  => 8,
                            Bytes_Per_Pixel => 1));

   Pixel_Format_RGB_444 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => XRGB),
                            Layout          => Bits_4444,
                            Bits_Per_Pixel  => 12,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_RGB_555 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => XRGB),
                            Layout          => Bits_1555,
                            Bits_Per_Pixel  => 15,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_BGR_555 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => XBGR),
                            Layout          => Bits_1555,
                            Bits_Per_Pixel  => 15,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_ARGB_4444 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => ARGB),
                            Layout          => Bits_4444,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_RGBA_4444 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => RGBA),
                            Layout          => Bits_4444,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_ABGR_4444 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => ABGR),
                            Layout          => Bits_4444,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_BGRA_4444 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => BGRA),
                            Layout          => Bits_4444,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_ARGB_1555 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => ARGB),
                            Layout          => Bits_1555,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_RGBA_5551 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => RGBA),
                            Layout          => Bits_5551,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_ABGR_1555 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => ABGR),
                            Layout          => Bits_1555,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_BGRA_5551 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => BGRA),
                            Layout          => Bits_5551,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_RGB_565 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => XRGB),
                            Layout          => Bits_565,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_BGR_565 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_16,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_16,
                               Packed_Order => XBGR),
                            Layout          => Bits_565,
                            Bits_Per_Pixel  => 16,
                            Bytes_Per_Pixel => 2));

   Pixel_Format_RGB_24 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Array_U8,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type  => Array_U8,
                               Array_Order => RGB),
                            Layout          => None,
                            Bits_Per_Pixel  => 24,
                            Bytes_Per_Pixel => 3));

   Pixel_Format_BGR_24 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Array_U8,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type  => Array_U8,
                               Array_Order => BGR),
                            Layout          => None,
                            Bits_Per_Pixel  => 24,
                            Bytes_Per_Pixel => 3));

   Pixel_Format_RGB_888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => XRGB),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 24,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_RGBX_8888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => RGBX),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 24,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_BGR_888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => XBGR),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 24,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_BGRX_8888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => BGRX),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 24,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_ARGB_8888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => ARGB),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 32,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_RGBA_8888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => RGBA),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 32,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_ABGR_8888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => ABGR),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 32,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_BGRA_8888 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => BGRA),
                            Layout          => Bits_8888,
                            Bits_Per_Pixel  => 32,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_ARGB_2101010 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar            => False,
                         Non_Planar_Format => Non_Planar_Pixels'
                           (Padding         => Non_Planar_Pixel_Padding'First,
                            Flag            => True,
                            Pixel_Type      => Packed_32,
                            Pixel_Order     => Pixel_Orders'
                              (Pixel_Type   => Packed_32,
                               Packed_Order => ARGB),
                            Layout          => Bits_2101010,
                            Bits_Per_Pixel  => 32,
                            Bytes_Per_Pixel => 4));

   Pixel_Format_YV_12 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar        => True,
                         Planar_Format => Planar_Pixels'
                           (A => 'Y',
                            B => 'V',
                            C => '1',
                            D => '2'));

   Pixel_Format_IYUV : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar        => True,
                         Planar_Format => Planar_Pixels'
                           (A => 'I',
                            B => 'Y',
                            C => 'U',
                            D => 'V'));

   Pixel_Format_YUY_2 : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar        => True,
                         Planar_Format => Planar_Pixels'
                           (A => 'Y',
                            B => 'U',
                            C => 'Y',
                            D => '2'));

   Pixel_Format_UYVY : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar        => True,
                         Planar_Format => Planar_Pixels'
                           (A => 'U',
                            B => 'Y',
                            C => 'V',
                            D => 'Y'));

   Pixel_Format_YVYU : constant Pixel_Format_Names :=
     Pixel_Format_Names'(Planar        => True,
                         Planar_Format => Planar_Pixels'
                           (A => 'Y',
                            B => 'V',
                            C => 'Y',
                            D => 'U'));

   type Colour_Mask is mod 2 ** 32 with
     Convention => C;

   type Private_Pixel_Format is private;

   type Pixel_Format is
      record
         Format       : Pixel_Format_Names;
         Palette      : Palettes.Palette_Access;
         Bits         : Bits_Per_Pixels;
         Bytes        : Bytes_Per_Pixels;
         Padding      : Interfaces.Unsigned_16;
         Red_Mask     : Colour_Mask;
         Green_Mask   : Colour_Mask;
         Blue_Mask    : Colour_Mask;
         Alpha_Mask   : Colour_Mask;

         --  This is mainly padding to make sure the record size matches what is expected from C.
         Private_Part : Private_Pixel_Format;
      end record with
     Convention => C;

   --  TODO: Possibly change this to a controlled type.
   type Pixel_Format_Access is access all Pixel_Format with
     Convention => C;

   function Create (Format : in Pixel_Format_Names) return Pixel_Format_Access with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_AllocFormat";

   procedure Free (Format : in Pixel_Format_Access) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_FreeFormat";

   function Image (Format : in Pixel_Format_Names) return String;
   --  Import        => True,
   --  Convention    => C,
   --  External_Name => "SDL_GetPixelFormatName";

   procedure To_Components
     (Pixel  : in  Interfaces.Unsigned_32;
      Format : in  Pixel_Format_Access;
      Red    : out Palettes.Colour_Component;
      Green  : out Palettes.Colour_Component;
      Blue   : out Palettes.Colour_Component) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetRGB";

   procedure To_Components
     (Pixel  : in  Interfaces.Unsigned_32;
      Format : in  Pixel_Format_Access;
      Red    : out Palettes.Colour_Component;
      Green  : out Palettes.Colour_Component;
      Blue   : out Palettes.Colour_Component;
      Alpha  : out Palettes.Colour_Component) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetRGBA";

   function To_Pixel
     (Format : in Pixel_Format_Access;
      Red    : in Palettes.Colour_Component;
      Green  : in Palettes.Colour_Component;
      Blue   : in Palettes.Colour_Component) return Interfaces.Unsigned_32 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_MapRGB";

   function To_Pixel
     (Format : in Pixel_Format_Access;
      Red    : in Palettes.Colour_Component;
      Green  : in Palettes.Colour_Component;
      Blue   : in Palettes.Colour_Component;
      Alpha  : in Palettes.Colour_Component) return Interfaces.Unsigned_32 with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_MapRGBA";

   function To_Name
     (Bits       : in Bits_Per_Pixels;
      Red_Mask   : in Colour_Mask;
      Green_Mask : in Colour_Mask;
      Blue_Mask  : in Colour_Mask;
      Alpha_Mask : in Colour_Mask) return Pixel_Format_Names with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_MasksToPixelFormatEnum";

   function To_Masks
     (Format     : in  Pixel_Format_Names;
      Bits       : out Bits_Per_Pixels;
      Red_Mask   : out Colour_Mask;
      Green_Mask : out Colour_Mask;
      Blue_Mask  : out Colour_Mask;
      Alpha_Mask : out Colour_Mask) return Boolean with
     Inline => True;

   --  Gamma
   type Gamma_Value is mod 2 ** 16 with
     Convention => C;

   type Gamma_Ramp is array (Integer range 1 .. 256) of Gamma_Value with
     Convention => C;

   procedure Calculate (Gamma : in Float; Ramp : out Gamma_Ramp) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_CalculateGammaRamp";
private
   --  The following fields are defined as "internal use" in the SDL docs.
   type Private_Pixel_Format is
      record
         Rred_Loss   : Interfaces.Unsigned_8;
         Green_Loss  : Interfaces.Unsigned_8;
         Blue_Loss   : Interfaces.Unsigned_8;
         Alpha_Loss  : Interfaces.Unsigned_8;
         Red_Shift   : Interfaces.Unsigned_8;
         Green_Shift : Interfaces.Unsigned_8;
         Blue_Shift  : Interfaces.Unsigned_8;
         Alpha_Shift : Interfaces.Unsigned_8;
         Ref_Count   : C.int;
         Next        : Pixel_Format_Access;
      end record with
     Convention => C;
end SDL.Video.Pixel_Formats;
