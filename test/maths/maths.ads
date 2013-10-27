--                              -*- Mode: Ada -*-
--  Filename        : maths.ads
--  Description     : Sample shared library using an Ada and a C function as an example.
--  Author          : Luke A. Guest
--  Created On      : Sun Oct 27 17:50:33 2013
package Maths is
   function Add (A, B : in Integer) return Integer with
     Export        => True,
     Convention    => Ada,
     External_Name => "Add";

   function Sub (A, B : in Integer) return Integer with
     Import        => True,
     Convention    => C,
     External_Name => "sub";
end Maths;
