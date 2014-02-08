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
--  SDL.Video.Surfaces
--
--  Render surface abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with System;

package SDL.Video.Surfaces is
   type Surface is new Ada.Finalization.Limited_Controlled with private;

   Null_Surface : constant Surface;
private
   type Surface is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address := System.Null_Address;
      end record;

   function Get_Address (Self : in Surface) return System.Address with
     Export     => True,
     Convention => Ada;

   procedure Set_Address (Self : in out Surface; A : in System.Address) with
     Export     => True,
     Convention => Ada;

   Null_Surface : constant Surface := (Ada.Finalization.Limited_Controlled with
                                         Internal => System.Null_Address);

end SDL.Video.Surfaces;
