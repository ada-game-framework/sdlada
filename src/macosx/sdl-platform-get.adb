--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  Mac OS X implementation.
--------------------------------------------------------------------------------------------------------------------
separate (SDL.Platform)
function Get return Platforms is
begin
   return Mac_OS_X;
end Get;
