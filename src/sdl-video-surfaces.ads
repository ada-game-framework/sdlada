--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-surfaces.ads
--  Description     : Subprograms relating to surfaces.
--  Author          : Luke A. Guest
--  Created On      : Sat Oct 12 14:23:05 2013
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
