--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video
--
--  Common display and video driver functionality.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Video is
   pragma Preelaborate;

   package C renames Interfaces.C;

   Video_Error : exception;

   --  subtype Coordinate is C.int;

   type Blend_Modes is (None, Alpha_Blend, Additive, Colour_Modulate, Multiply) with
     Convention => C;

   for Blend_Modes use
     (None            => 16#0000_0000#,
      Alpha_Blend     => 16#0000_0001#,
      Additive        => 16#0000_0002#,
      Colour_Modulate => 16#0000_0004#,
      Multiply        => 16#0000_0008#);

   --  Screen saver information.
   procedure Enable_Screen_Saver with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_EnableScreenSaver";

   procedure Disable_Screen_Saver with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_DisableScreenSaver";

   function Is_Screen_Saver_Enabled return Boolean with
     Inline => True;

   --  Video drivers.
   function Initialise (Name : in String) return Boolean;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_VideoQuit";

   function Total_Drivers return Positive;

   function Driver_Name (Index : in Positive) return String;

   function Current_Driver_Name return String;
end SDL.Video;
