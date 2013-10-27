--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-renderers.ads
--  Description     : Renderer.
--  Author          : Luke A. Guest
--  Created On      : Sat Oct 12 21:39:48 2013
with Ada.Finalization;
with System;

package SDL.Video.Renderers is
   type Renderer is new Ada.Finalization.Limited_Controlled with private;
   --  TODO: Finish this.

--  SDL_RendererFlags
--  SDL_RendererFlip
--  SDL_RendererInfo

--  SDL_CreateRenderer
--  SDL_CreateSoftwareRenderer
--  SDL_CreateWindowAndRenderer
--  SDL_DestroyRenderer
--  SDL_DestroyTexture
--  SDL_GetNumRenderDrivers
--  SDL_GetRenderDrawBlendMode
--  SDL_GetRenderDrawColor
--  SDL_GetRenderDriverInfo
--  SDL_GetRenderer
--  SDL_GetRendererInfo
--  SDL_RenderClear
--  SDL_RenderCopy
--  SDL_RenderCopyEx
--  SDL_RenderDrawLine
--  SDL_RenderDrawLines
--  SDL_RenderDrawPoint
--  SDL_RenderDrawPoints
--  SDL_RenderDrawRect
--  SDL_RenderDrawRects
--  SDL_RenderFillRect
--  SDL_RenderFillRects
--  SDL_RenderGetClipRect
--  SDL_RenderGetViewport
--  SDL_RenderPresent
--  SDL_RenderReadPixels
--  SDL_RenderSetClipRect
--  SDL_RenderSetViewport
--  SDL_SetRenderDrawBlendMode
--  SDL_SetRenderDrawColor
--  SDL_SetRenderTarget
private
   type Renderer is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address;
      end record;

   function Get_Address (Self : in Renderer) return System.Address with
     Export     => True,
     Convention => Ada;
end SDL.Video.Renderers;
