--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
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
