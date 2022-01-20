--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2021, Eduard Llamosí
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
--  SDL.Audio.Frame_Formats
--
--  Access to audio sample data.
--------------------------------------------------------------------------------------------------------------------
with System; use System;

package SDL.Audio.Sample_Formats is

   type Sample_Bit_Size is mod 2 ** 8 with
     Convention => C;

   type Sample_Endianness is (Little_Endian, Big_Endian) with
     Convention => C;

   System_Endianness : constant Sample_Endianness :=
     (if System.Default_Bit_Order = System.High_Order_First
      then Big_Endian
      else Little_Endian);

   type Sample_Format is record
      Bit_Size   : Sample_Bit_Size;
      Float      : Boolean;
      Endianness : Sample_Endianness := Little_Endian;
      Signed     : Boolean;
   end record with
     Convention => C,
     Size       => 16;
   for Sample_Format use record
      Bit_Size   at 0 range 0 .. 7;
      Float      at 1 range 0 .. 0;
      Endianness at 1 range 4 .. 4;
      Signed     at 1 range 7 .. 7;
   end record;

   --
   --  Audio format flags
   --
   --  Defaults to LSB byte order.
   --

   --  Unsigned 8-bit samples
   Sample_Format_U8 : constant Sample_Format :=
     (Bit_Size   => 8,
      Float      => False,
      Endianness => Little_Endian,
      Signed     => False);
   --  Signed 8-bit samples
   Sample_Format_S8 : constant Sample_Format :=
     (Bit_Size   => 8,
      Float      => False,
      Endianness => Little_Endian,
      Signed     => True);
   --  Unsigned 16-bit samples
   Sample_Format_U16LSB : constant Sample_Format :=
     (Bit_Size   => 16,
      Float      => False,
      Endianness => Little_Endian,
      Signed     => False);
   --  Signed 16-bit samples
   Sample_Format_S16LSB : constant Sample_Format :=
     (Bit_Size   => 16,
      Float      => False,
      Endianness => Little_Endian,
      Signed     => True);
   --  As above, but big-endian byte order
   Sample_Format_U16MSB : constant Sample_Format :=
     (Bit_Size   => 16,
      Float      => False,
      Endianness => Big_Endian,
      Signed     => False);
   --  As above, but big-endian byte order
   Sample_Format_S16MSB : constant Sample_Format :=
     (Bit_Size   => 16,
      Float      => False,
      Endianness => Big_Endian,
      Signed     => True);
   Sample_Format_U16 : constant Sample_Format := Sample_Format_U16LSB;
   Sample_Format_S16 : constant Sample_Format := Sample_Format_S16LSB;

   --
   --  int32 support
   --

   --  32-bit integer samples
   Sample_Format_S32LSB : constant Sample_Format :=
     (Bit_Size   => 32,
      Float      => False,
      Endianness => Little_Endian,
      Signed     => False);
   --  As above, but big-endian byte order
   Sample_Format_S32MSB : constant Sample_Format :=
     (Bit_Size   => 32,
      Float      => False,
      Endianness => Big_Endian,
      Signed     => True);
   Sample_Format_S32 : constant Sample_Format := Sample_Format_S32LSB;

   --
   --  float32 support
   --

   --  32-bit floating point samples
   Sample_Format_F32LSB : constant Sample_Format :=
     (Bit_Size   => 32,
      Float      => True,
      Endianness => Little_Endian,
      Signed     => True);
   --  As above, but big-endian byte order
   Sample_Format_F32MSB : constant Sample_Format :=
     (Bit_Size   => 32,
      Float      => True,
      Endianness => Big_Endian,
      Signed     => True);

   --
   --  Native audio byte ordering
   --

   Sample_Format_U16SYS : constant Sample_Format :=
     (Bit_Size   => 16,
      Float      => False,
      Endianness => System_Endianness,
      Signed     => False);
   Sample_Format_S16SYS : constant Sample_Format :=
     (Bit_Size   => 16,
      Float      => False,
      Endianness => System_Endianness,
      Signed     => True);
   Sample_Format_U32SYS : constant Sample_Format :=
     (Bit_Size   => 32,
      Float      => False,
      Endianness => System_Endianness,
      Signed     => False);
   Sample_Format_S32SYS : constant Sample_Format :=
     (Bit_Size   => 32,
      Float      => False,
      Endianness => System_Endianness,
      Signed     => True);

end SDL.Audio.Sample_Formats;
