--                              -*- Mode: Ada -*-
--  Filename        : sdl-video.ads
--  Description     : Common video related stuff.
--  Author          : Luke A. Guest
--  Created On      : Tue Sep 24 13:45:56 2013
package SDL.Video is
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
   function Initialise (Name : in String) return Error_Code;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_VideoQuit";

   function Total_Drivers (Total : out Positive) return Boolean;

   function Driver_Name (Index : in Natural) return String;

   function Current_Driver_Name return String;

   --  Videe displays.
   function Total_Displays (Total : out Positive) return Boolean;
end SDL.Video;
