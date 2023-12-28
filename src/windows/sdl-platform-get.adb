--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  Windows implementation.
--------------------------------------------------------------------------------------------------------------------
separate (SDL.Platform)
function Get return Platforms is
begin
   return Windows;
end Get;
