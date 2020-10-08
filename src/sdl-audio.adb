--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2020, Luke A. Guest
--  Contributed by Vinzent "Jellix" Saranen
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

with Interfaces.C;

with SDL.RWops;

use type Interfaces.C.int;

package body SDL.Audio is

   --  Used internally to check result of Load_WAV.
   type Audio_Spec_Ptr is access all Audio_Spec;
   pragma Convention (Convention => C,
                      Entity     => Audio_Spec_Ptr);

   ---------------------------------------------------------------------
   --  C_Build_CVT
   ---------------------------------------------------------------------
   function C_Build_CVT (CVT          : in System.Address; --  in out Conversion;
                         Src_Format   : in Format_Id;
                         Src_Channels : in Interfaces.Unsigned_8;
                         Src_Rate     : in Interfaces.C.int;
                         Dst_Format   : in Format_Id;
                         Dst_Channels : in Interfaces.Unsigned_8;
                         Dst_Rate     : in Interfaces.C.int) return Interfaces.C.int;
   pragma Import (Convention    => C,
                  Entity        => C_Build_CVT,
                  External_Name => "SDL_BuildAudioCVT");

   ---------------------------------------------------------------------
   --  C_Convert
   ---------------------------------------------------------------------
   function C_Convert (CVT : in System.Address) --  in out Conversion
                       return Interfaces.C.int;
   pragma Import (Convention    => C,
                  Entity        => C_Convert,
                  External_Name => "SDL_ConvertAudio");

   ---------------------------------------------------------------------
   -- C_Free_WAV
   ---------------------------------------------------------------------
   procedure C_Free_WAV (Audio_Buf : in Audio_Buffer);
   pragma Import (Convention    => C,
                  Entity        => C_Free_WAV,
                  External_Name => "SDL_FreeWAV");

   ---------------------------------------------------------------------
   -- C_Load_WAV
   ---------------------------------------------------------------------
   function C_Load_WAV (Src      : in RWOps.RWops;
                        Free_Src : in Bool;
                        Spec     : in System.Address; --  in out Audio_Spec
                        Buf      : in System.Address; --     out Audio_Buffer
                        Len      : in System.Address) --     out UInt32
                        return Audio_Spec_Ptr;
   pragma Import (Convention    => C,
                  Entity        => C_Load_WAV,
                  External_Name => "SDL_LoadWAV_RW");

   ---------------------------------------------------------------------
   -- C_Open
   ---------------------------------------------------------------------
   function C_Open (Desired  : in System.Address;
                    Obtained : in System.Address) return Interfaces.C.int;
   pragma Import (Convention    => C,
                  Entity        => C_Open,
                  External_Name => "SDL_OpenAudio");

   ---------------------------------------------------------------------
   --  Build_CVT
   ---------------------------------------------------------------------
   procedure Build_CVT (CVT          : in out Conversion;
                        Src_Format   : in     Format_Id;
                        Src_Channels : in     Interfaces.Unsigned_8;
                        Src_Rate     : in     Interfaces.C.int;
                        Dst_Format   : in     Format_Id;
                        Dst_Channels : in     Interfaces.Unsigned_8;
                        Dst_Rate     : in     Interfaces.C.int;
                        Success      :    out Boolean)
   is
      C_Result : Interfaces.C.int;
   begin
      C_Result := C_Build_CVT (CVT          => CVT'Address,
                               Src_Format   => Src_Format,
                               Src_Channels => Src_Channels,
                               Src_Rate     => Src_Rate,
                               Dst_Format   => Dst_Format,
                               Dst_Channels => Dst_Channels,
                               Dst_Rate     => Dst_Rate);

      Success := C_Result = 1;
   end Build_CVT;

   ---------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------
   procedure Convert (CVT     : in out Conversion;
                      Success :    out Boolean)
   is
      C_Result : Interfaces.C.int;
   begin
      C_Result := C_Convert (CVT => CVT'Address);

      Success := C_Result = 0;
   end Convert;

   ---------------------------------------------------------------------
   --  Free_WAV
   ---------------------------------------------------------------------
   procedure Free_WAV (Audio_Buf : in out Audio_Buffer) is
   begin
      C_Free_WAV (Audio_Buf => Audio_Buf);
      Audio_Buf := Audio_Buffer (System.Null_Address);
   end Free_WAV;

   ---------------------------------------------------------------------
   --  Load_WAV
   ---------------------------------------------------------------------
   procedure Load_WAV (File_Name : in     String;
                       Spec      :    out Audio_Spec;
                       Audio_Buf :    out Audio_Buffer;
                       Audio_Len :    out Interfaces.Unsigned_32;
                       Success   :    out Boolean)
   is
      File_Ops : constant RWOps.RWops :=
        RWOps.From_File (File_Name => File_Name,
                         Mode      => RWOps.Read_Binary);
   begin
      Success :=
        C_Load_WAV (Src      => File_Ops,
                    Free_Src => True,
                    Spec     => Spec'Address,
                    Buf      => Audio_Buf'Address,
                    Len      => Audio_Len'Address) /= null;
   end Load_WAV;

   ---------------------------------------------------------------------
   -- Open
   ---------------------------------------------------------------------
   procedure Open (Desired  : in out Audio_Spec;
                   Obtained : in out Audio_Spec;
                   Success  :    out Boolean)
   is
      Ret_Value : Interfaces.C.int;
   begin
      Ret_Value := C_Open (Desired  => Desired'Address,
                           Obtained => Obtained'Address);

      Success := Ret_Value = 0;
   end Open;

   ---------------------------------------------------------------------
   -- Open
   ---------------------------------------------------------------------
   procedure Open (Required : in out Audio_Spec;
                   Success  :    out Boolean)
   is
      Ret_Value : Interfaces.C.int;
   begin
      Ret_Value := C_Open (Desired  => Required'Address,
                           Obtained => System.Null_Address);

      Success := Ret_Value = 0;
   end Open;

end SDL.Audio;
