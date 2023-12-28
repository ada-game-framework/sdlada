--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Platform
--
--  Determine which platform we are running on.
--------------------------------------------------------------------------------------------------------------------
package SDL.Platform is
   pragma Pure;

   type Platforms is (Windows, Mac_OS_X, Linux, BSD, iOS, Android);

   function Get return Platforms with
     Inline => True;
end SDL.Platform;
