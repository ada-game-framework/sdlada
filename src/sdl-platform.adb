--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
package body SDL.Platform is
   --  Bring in the platform specific version for each build of the library. It's also defined as inline so
   --  that the compiler can eliminate redundant code in the static expressions that use this function.
   function Get return Platforms is separate;
end SDL.Platform;
