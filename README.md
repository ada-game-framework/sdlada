# SDLAda

This is a new variable thickness Ada 2012 binding to SDL 2.x.y (http://www.libsdl.org). This means that things are
wrapped when they need to be into a thicker binding and those things that can just be imported, are.

There will still be a need to use conditional compilation for the various platforms as there will be
some platform specifics which cannot be abstracted out cleanly, but this will be reduced as much as possible.

The bindings are only generated for the specific parts of SDL which Ada does not already have support for, i.e.
threads are not bound as Ada has tasking support built in.

## Usage, Licence & Attribution

SDLAda is distributed under the same zlib licence as the SDL library is. You may use this library in any way
you like whether free or commercial. I would add that whilst you don't have to state that your game uses this
library, it would be nice if you did, it's all good advertising for Ada :)

You do not need to complete any copyright assignment forms, the licence will not change from zlib as I want people to be
able to use the library however they wish without restrictions.

## Building

There are a number of variables which can be set to control the compilation:

* SDL_PLATFORM = Can be set to one of the following values: linux, windows, macosx, ios or android
* SDL_MODE     = Can be one of: debug or release. Defaults to debug.

```
cd build/gnat
make SDL_PLATFORM=linux SDL_MODE=release
```

## Installation

```
make SDL_PLATFORM=linux SDL_MODE=release DESTDIR=$HOME/opt/sdlada install
```

## Copyright

Copyright (C) 2013-2020, Luke A. Guest

## Thanks to

To everyone who has provided patches, pull requests, typo fixes, etc.

## WARNING!!!

I have tried to model the types correctly, but remember SDL is a C library and some things may not make sense. Please
file a bug report if you use this library and find that a type does not cover enough values for cross-platform use.

## TODO

Anything listed here is a question of whether it's required.

* Do we need to hook into the Assert module? To raise an exception maybe?
* Create event tagged type hierarchy, use Poll to convert the C events into these types.
* Task safe event handling?
* Split Image, TTF into separate GPR files.
* Cross platform testing.
* Optimisations to make the library as thin as possible:
  - Enable -gnatN for inlining.
  - Try to get as much pre-elaborated as possible.
* Disable a shared library build, make it a static only build, this makes it easier to distribute. i.e. Link with this
  library and distribute SDL2 libs only.
* Hide the RWops stuff maybe? Should really be using Ada streams and files.

## [Current version](http://www.semver.org)

v2.5.1

## Versions tested against

Library   | Version
----------|--------
SDL       | 2.0.7
SDL_image | 2.0.2
SDL_ttf   | 2.0.14

## Bound functions

### SDL

Name                                  | Bound | Won't bind
--------------------------------------|-------|-----------
SDL_AddEventWatch                     |   n   |
SDL_AddHintCallback                   |   n   |
SDL_AddTimer                          |   n   |
SDL_AllocFormat                       |   y   |
SDL_AllocPalette                      |   y   |
SDL_AllocRW                           |   n   |
SDL_AndroidGetActivity                |   n   |
SDL_AndroidGetExternalStoragePath     |   n   |
SDL_AndroidGetExternalStorageState    |   n   |
SDL_AndroidGetInternalStoragePath     |   n   |
SDL_AndroidGetJNIEnv                  |   n   |
SDL_AtomicAdd                         |   n   |
SDL_AtomicCAS                         |   n   |
SDL_AtomicCASPtr                      |   n   |
SDL_AtomicDecRef                      |   n   |
SDL_AtomicGet                         |   n   |
SDL_AtomicGetPtr                      |   n   |
SDL_AtomicIncRef                      |   n   |
SDL_AtomicLock                        |   n   |
SDL_AtomicSet                         |   n   |
SDL_AtomicSetPtr                      |   n   |
SDL_AtomicTryLock                     |   n   |
SDL_AtomicUnlock                      |   n   |
SDL_AudioInit                         |   n   |
SDL_AudioQuit                         |   n   |
SDL_BlitScaled                        |   y   |
SDL_BlitSurface                       |   y   |
SDL_BuildAudioCVT                     |   n   |
SDL_COMPILEDVERSION                   |   n   |      *
SDL_CalculateGammaRamp                |   y   |
SDL_CaptureMouse                      |   y   |
SDL_ClearError                        |   y   |
SDL_ClearHints                        |   y   |
SDL_ClearQueuedAudio                  |   n   |
SDL_CloseAudio                        |   n   |
SDL_CloseAudioDevice                  |   n   |
SDL_CompilerBarrier                   |   n   |
SDL_CondBroadcast                     |   n   |
SDL_CondSignal                        |   n   |
SDL_CondWait                          |   n   |
SDL_CondWaitTimeout                   |   n   |
SDL_ConvertAudio                      |   n   |
SDL_ConvertPixels                     |   n   |
SDL_ConvertSurface                    |   n   |
SDL_ConvertSurfaceFormat              |   n   |
SDL_CreateColorCursor                 |   n   |
SDL_CreateCond                        |   n   |
SDL_CreateCursor                      |   n   |
SDL_CreateMutex                       |   n   |
SDL_CreateRGBSurface                  |   y   |
SDL_CreateRGBSurfaceFrom              |   n   |
SDL_CreateRenderer                    |   y   |
SDL_CreateSemaphore                   |   n   |
SDL_CreateSoftwareRenderer            |   y   |
SDL_CreateSystemCursor                |   y   |
SDL_CreateTexture                     |   y   |
SDL_CreateTextureFromSurface          |   y   |
SDL_CreateThread                      |   n   |
SDL_CreateWindow                      |   y   |
SDL_CreateWindowAndRenderer           |   n   |
SDL_CreateWindowFrom                  |   y   |
SDL_DXGIGetOutputInfo                 |   n   |
SDL_DelEventWatch                     |   n   |
SDL_DelHintCallback                   |   n   |
SDL_Delay                             |   n   |
SDL_DestroyCond                       |   n   |
SDL_DestroyMutex                      |   n   |
SDL_DestroyRenderer                   |   y   |
SDL_DestroySemaphore                  |   n   |
SDL_DestroyTexture                    |   y   |
SDL_DestroyWindow                     |   y   |
SDL_DetachThread                      |   n   |
SDL_Direct3D9GetAdapterIndex          |   n   |
SDL_DisableScreenSaver                |   y   |
SDL_EnableScreenSaver                 |   y   |
SDL_EnclosePoints                     |   y   |
SDL_EventState                        |   n   |
SDL_FillRect                          |   y   |
SDL_FillRects                         |   y   |
SDL_FilterEvents                      |   n   |
SDL_FlushEvent                        |   n   |
SDL_FlushEvents                       |   n   |
SDL_FreeCursor                        |   y   |
SDL_FreeFormat                        |   y   |
SDL_FreePalette                       |   y   |
SDL_FreeRW                            |   n   |      * (implicitly called by RWclose)
SDL_FreeSurface                       |   y   |
SDL_FreeWAV                           |   n   |
SDL_GL_BindTexture                    |   y   |
SDL_GL_CreateContext                  |   y   |
SDL_GL_DeleteContext                  |   y   |
SDL_GL_ExtensionSupported             |   y   |
SDL_GL_GetAttribute                   |   y   |
SDL_GL_GetCurrentContext              |   y   |
SDL_GL_GetCurrentWindow               |   n   |
SDL_GL_GetDrawableSize                |   y   |
SDL_GL_GetProcAddress                 |   y   |
SDL_GL_GetSwapInterval                |   y   |
SDL_GL_LoadLibrary                    |   y   |
SDL_GL_MakeCurrent                    |   y   |
SDL_GL_ResetAttributes                |   y   |
SDL_GL_SetAttribute                   |   y   |
SDL_GL_SetSwapInterval                |   y   |
SDL_GL_SwapWindow                     |   y   |
SDL_GL_UnbindTexture                  |   y   |
SDL_GL_UnloadLibrary                  |   y   |
SDL_GameControllerAddMapping          |   y   |
SDL_GameControllerAddMappingsFromFile |   y   |
SDL_GameControllerAddMappingsFromRW   |   n   |
SDL_GameControllerClose               |   y   |
SDL_GameControllerEventState          |   y   |
SDL_GameControllerGetAttached         |   y   |
SDL_GameControllerGetAxis             |   y   |
SDL_GameControllerGetAxisFromString   |   y   |
SDL_GameControllerGetBindForAxis      |   y   |
SDL_GameControllerGetBindForButton    |   y   |
SDL_GameControllerGetButton           |   y   |
SDL_GameControllerGetButtonFromString |   y   |
SDL_GameControllerGetJoystick         |   y   |
SDL_GameControllerGetStringForAxis    |   y   |
SDL_GameControllerGetStringForButton  |   y   |
SDL_GameControllerMapping             |   y   |
SDL_GameControllerMappingForGUID      |   y   |
SDL_GameControllerName                |   y   |
SDL_GameControllerNameForIndex        |   y   |
SDL_GameControllerOpen                |   y   |
SDL_GameControllerUpdate              |   y   |
SDL_GetAssertionHandler               |   n   |
SDL_GetAssertionReport                |   n   |
SDL_GetAudioDeviceName                |   n   |
SDL_GetAudioDeviceStatus              |   n   |
SDL_GetAudioDriver                    |   n   |
SDL_GetAudioStatus                    |   n   |
SDL_GetBasePath                       |   y   |
SDL_GetCPUCacheLineSize               |   y   |
SDL_GetCPUCount                       |   y   |
SDL_GetClipRect                       |   y   |
SDL_GetClipboardText                  |   y   |
SDL_GetClosestDisplayMode             |   y   |
SDL_GetColorKey                       |   y   |
SDL_GetCurrentAudioDriver             |   n   |
SDL_GetCurrentDisplayMode             |   y   |
SDL_GetCurrentVideoDriver             |   y   |
SDL_GetCursor                         |   y   |
SDL_GetDefaultAssertionHandler        |   n   |
SDL_GetDefaultCursor                  |   n   |
SDL_GetDesktopDisplayMode             |   y   |
SDL_GetDisplayBounds                  |   y   |
SDL_GetDisplayMode                    |   y   |
SDL_GetDisplayName                    |   n   |
SDL_GetError                          |   y   |
SDL_GetEventFilter                    |   n   |
SDL_GetGlobalMouseState               |   y   |
SDL_GetHint                           |   y   |
SDL_GetKeyFromName                    |   y   |
SDL_GetKeyFromScancode                |   y   |
SDL_GetKeyName                        |   y   |
SDL_GetKeyboardFocus                  |   y   |
SDL_GetKeyboardState                  |   n   |
SDL_GetModState                       |   y   |
SDL_GetMouseFocus                     |   n   |
SDL_GetMouseState                     |   y   |
SDL_GetNumAudioDevices                |   n   |
SDL_GetNumAudioDrivers                |   n   |
SDL_GetNumDisplayModes                |   y   |
SDL_GetNumRenderDrivers               |   y   |
SDL_GetNumTouchDevices                |   n   |
SDL_GetNumTouchFingers                |   n   |
SDL_GetNumVideoDisplays               |   y   |
SDL_GetNumVideoDrivers                |   y   |
SDL_GetPerformanceCounter             |   n   |
SDL_GetPerformanceFrequency           |   n   |
SDL_GetPixelFormatName                |   y   |
SDL_GetPlatform                       |   n   |      *
SDL_GetPowerInfo                      |   y   |
SDL_GetPrefPath                       |   y   |
SDL_GetQueuedAudioSize                |   n   |
SDL_GetRGB                            |   y   |
SDL_GetRGBA                           |   y   |
SDL_GetRelativeMouseMode              |   y   |
SDL_GetRelativeMouseState             |   y   |
SDL_GetRenderDrawBlendMode            |   y   |
SDL_GetRenderDrawColor                |   y   |
SDL_GetRenderDriverInfo               |   n   |
SDL_GetRenderTarget                   |   n   |
SDL_GetRenderer                       |   y   |
SDL_GetRendererInfo                   |   n   |
SDL_GetRendererOutputSize             |   n   |
SDL_GetRevision                       |   y   |
SDL_GetRevisionNumber                 |   y   |
SDL_GetScancodeFromKey                |   y   |
SDL_GetScancodeFromName               |   y   |
SDL_GetScancodeName                   |   y   |
SDL_GetSurfaceAlphaMod                |   y   |
SDL_GetSurfaceBlendMode               |   y   |
SDL_GetSurfaceColorMod                |   y   |
SDL_GetSystemRAM                      |   n   |
SDL_GetTextureAlphaMod                |   y   |
SDL_GetTextureBlendMode               |   y   |
SDL_GetTextureColorMod                |   y   |
SDL_GetThreadID                       |   n   |
SDL_GetThreadName                     |   n   |
SDL_GetTicks                          |   n   |
SDL_GetTouchDevice                    |   n   |
SDL_GetTouchFinger                    |   n   |
SDL_GetVersion                        |   y   |
SDL_GetVideoDriver                    |   y   |
SDL_GetWindowBrightness               |   y   |
SDL_GetWindowData                     |   y   |
SDL_GetWindowDisplayIndex             |   y   |
SDL_GetWindowDisplayMode              |   y   |
SDL_GetWindowFlags                    |   y   |
SDL_GetWindowFromID                   |   y   |
SDL_GetWindowGammaRamp                |   y   |
SDL_GetWindowGrab                     |   y   |
SDL_GetWindowID                       |   y   |
SDL_GetWindowMaximumSize              |   y   |
SDL_GetWindowMinimumSize              |   y   |
SDL_GetWindowPixelFormat              |   y   |
SDL_GetWindowPosition                 |   y   |
SDL_GetWindowSize                     |   y   |
SDL_GetWindowSurface                  |   y   |
SDL_GetWindowTitle                    |   y   |
SDL_GetWindowWMInfo                   |   y   |
SDL_HapticClose                       |   n   |
SDL_HapticDestroyEffect               |   n   |
SDL_HapticEffectSupported             |   n   |
SDL_HapticGetEffectStatus             |   n   |
SDL_HapticIndex                       |   n   |
SDL_HapticName                        |   n   |
SDL_HapticNewEffect                   |   n   |
SDL_HapticNumAxes                     |   n   |
SDL_HapticNumEffects                  |   n   |
SDL_HapticNumEffectsPlaying           |   n   |
SDL_HapticOpen                        |   n   |
SDL_HapticOpenFromJoystick            |   n   |
SDL_HapticOpenFromMouse               |   n   |
SDL_HapticOpened                      |   n   |
SDL_HapticPause                       |   n   |
SDL_HapticQuery                       |   n   |
SDL_HapticRumbleInit                  |   n   |
SDL_HapticRumblePlay                  |   n   |
SDL_HapticRumbleStop                  |   n   |
SDL_HapticRumbleSupported             |   n   |
SDL_HapticRunEffect                   |   n   |
SDL_HapticSetAutocenter               |   n   |
SDL_HapticSetGain                     |   n   |
SDL_HapticStopAll                     |   n   |
SDL_HapticStopEffect                  |   n   |
SDL_HapticUnpause                     |   n   |
SDL_HapticUpdateEffect                |   n   |
SDL_Has3DNow                          |   y   |
SDL_HasAVX                            |   y   |
SDL_HasAVX2                           |   y   |
SDL_HasAltiVec                        |   y   |
SDL_HasClipboardText                  |   y   |
SDL_HasEvent                          |   n   |
SDL_HasEvents                         |   n   |
SDL_HasIntersection                   |   y   |
SDL_HasMMX                            |   y   |
SDL_HasRDTSC                          |   y   |
SDL_HasSSE                            |   y   |
SDL_HasSSE2                           |   y   |
SDL_HasSSE3                           |   y   |
SDL_HasSSE41                          |   y   |
SDL_HasSSE42                          |   n   |
SDL_HasScreenKeyboardSupport          |   y   |
SDL_HideWindow                        |   y   |
SDL_Init                              |   y   |
SDL_InitSubSystem                     |   y   |
SDL_IntersectRect                     |   y   |
SDL_IntersectRectAndLine              |   y   |
SDL_IsGameController                  |   y   |
SDL_IsScreenKeyboardShown             |   y   |
SDL_IsScreenSaverEnabled              |   y   |
SDL_IsTextInputActive                 |   y   |
SDL_JoystickClose                     |   y   |
SDL_JoystickEventState                |   y   |
SDL_JoystickFromInstanceID            |   n   |
SDL_JoystickGetAttached               |   y   |
SDL_JoystickGetAxis                   |   y   |
SDL_JoystickGetBall                   |   y   |
SDL_JoystickGetButton                 |   y   |
SDL_JoystickGetDeviceGUID             |   y   |
SDL_JoystickGetGUID                   |   y   |
SDL_JoystickGetGUIDFromString         |   y   |
SDL_JoystickGetGUIDString             |   y   |
SDL_JoystickGetHat                    |   y   |
SDL_JoystickInstanceID                |   y   |
SDL_JoystickIsHaptic                  |   y   |
SDL_JoystickName                      |   y   |
SDL_JoystickNameForIndex              |   y   |
SDL_JoystickNumAxes                   |   y   |
SDL_JoystickNumBalls                  |   y   |
SDL_JoystickNumButtons                |   y   |
SDL_JoystickNumHats                   |   y   |
SDL_JoystickOpen                      |   y   |
SDL_JoystickUpdate                    |   y   |
SDL_LoadBMP                           |   n   |
SDL_LoadBMP_RW                        |   n   |
SDL_LoadDollarTemplates               |   n   |
SDL_LoadFunction                      |   y   |
SDL_LoadObject                        |   y   |
SDL_LoadWAV                           |   n   |
SDL_LoadWAV_RW                        |   n   |
SDL_LockAudio                         |   n   |
SDL_LockAudioDevice                   |   n   |
SDL_LockMutex                         |   n   |
SDL_LockSurface                       |   y   |
SDL_LockTexture                       |   y   |
SDL_Log                               |   y   |
SDL_LogCritical                       |   y   |
SDL_LogDebug                          |   y   |
SDL_LogError                          |   y   |
SDL_LogGetOutputFunction              |   n   |
SDL_LogGetPriority                    |   n   |
SDL_LogInfo                           |   y   |
SDL_LogMessage                        |   y   |
SDL_LogMessageV                       |   n   |
SDL_LogResetPriorities                |   y   |
SDL_LogSetAllPriority                 |   y   |
SDL_LogSetOutputFunction              |   n   |
SDL_LogSetPriority                    |   y   |
SDL_LogVerbose                        |   y   |
SDL_LogWarn                           |   y   |
SDL_LowerBlit                         |   y   |
SDL_LowerBlitScaled                   |   y   |
SDL_MUSTLOCK                          |   y   |
SDL_MapRGB                            |   y   |
SDL_MapRGBA                           |   y   |
SDL_MasksToPixelFormatEnum            |   y   |
SDL_MaximizeWindow                    |   y   |
SDL_MinimizeWindow                    |   y   |
SDL_MixAudio                          |   n   |
SDL_MixAudioFormat                    |   n   |
SDL_MostSignificantBitIndex32         |   n   |
SDL_MouseIsHaptic                     |   n   |
SDL_NumHaptics                        |   n   |
SDL_NumJoysticks                      |   y   |
SDL_OpenAudio                         |   n   |
SDL_OpenAudioDevice                   |   n   |
SDL_PauseAudio                        |   n   |
SDL_PauseAudioDevice                  |   n   |
SDL_PeepEvents                        |   n   |
SDL_PixelFormatEnumToMasks            |   y   |
SDL_PointInRect                       |   n   |
SDL_PollEvent                         |   y   |
SDL_PumpEvents                        |   n   |
SDL_PushEvent                         |   n   |
SDL_QueryTexture                      |   y   |
SDL_QueueAudio                        |   n   |
SDL_Quit                              |   y   |
SDL_QuitRequested                     |   n   |
SDL_QuitSubSystem                     |   y   |
SDL_REVISION                          |   n   |      *
SDL_RWFromConstMem                    |   n   |
SDL_RWFromFP                          |   n   |      * (unix only)
SDL_RWFromFile                        |   y   |
SDL_RWFromMem                         |   n   |
SDL_RWclose                           |   y   |
SDL_RWread                            |   y   |
SDL_RWseek                            |   y   |
SDL_RWsize                            |   y   |
SDL_RWtell                            |   y   |
SDL_RWwrite                           |   y   |
SDL_RaiseWindow                       |   y   |
SDL_ReadBE16                          |   y   |
SDL_ReadBE32                          |   y   |
SDL_ReadBE64                          |   y   |
SDL_ReadLE16                          |   y   |
SDL_ReadLE32                          |   y   |
SDL_ReadLE64                          |   y   |
SDL_ReadU8                            |   y   |
SDL_RecordGesture                     |   n   |
SDL_RectEmpty                         |   n   |
SDL_RectEquals                        |   n   |
SDL_RegisterEvents                    |   n   |
SDL_RemoveTimer                       |   n   |
SDL_RenderClear                       |   y   |
SDL_RenderCopy                        |   y   |
SDL_RenderCopyEx                      |   y   |
SDL_RenderDrawLine                    |   y   |
SDL_RenderDrawLines                   |   y   |
SDL_RenderDrawPoint                   |   y   |
SDL_RenderDrawPoints                  |   y   |
SDL_RenderDrawRect                    |   y   |
SDL_RenderDrawRects                   |   y   |
SDL_RenderFillRect                    |   y   |
SDL_RenderFillRects                   |   y   |
SDL_RenderGetClipRect                 |   y   |
SDL_RenderGetD3D9Device               |   n   |
SDL_RenderGetLogicalSize              |   y   |
SDL_RenderGetScale                    |   y   |
SDL_RenderGetViewport                 |   y   |
SDL_RenderIsClipEnabled               |   n   |
SDL_RenderPresent                     |   y   |
SDL_RenderReadPixels                  |   n   |
SDL_RenderSetClipRect                 |   y   |
SDL_RenderSetLogicalSize              |   y   |
SDL_RenderSetScale                    |   y   |
SDL_RenderSetViewport                 |   y   |
SDL_RenderTargetSupported             |   y   |
SDL_ResetAssertionReport              |   n   |
SDL_RestoreWindow                     |   y   |
SDL_SaveAllDollarTemplates            |   n   |
SDL_SaveBMP                           |   n   |
SDL_SaveBMP_RW                        |   n   |
SDL_SaveDollarTemplate                |   n   |
SDL_SemPost                           |   n   |
SDL_SemTryWait                        |   n   |
SDL_SemValue                          |   n   |
SDL_SemWait                           |   n   |
SDL_SemWaitTimeout                    |   n   |
SDL_SetAssertionHandler               |   n   |
SDL_SetClipRect                       |   y   |
SDL_SetClipboardText                  |   y   |
SDL_SetColorKey                       |   y   |
SDL_SetCursor                         |   y   |
SDL_SetError                          |   y   |
SDL_SetEventFilter                    |   n   |
SDL_SetHint                           |   y   |
SDL_SetHintWithPriority               |   y   |
SDL_SetMainReady                      |   n   |
SDL_SetModState                       |   y   |
SDL_SetPaletteColors                  |   n   |
SDL_SetPixelFormatPalette             |   n   |
SDL_SetRelativeMouseMode              |   y   |
SDL_SetRenderDrawBlendMode            |   y   |
SDL_SetRenderDrawColor                |   y   |
SDL_SetRenderTarget                   |   y   |
SDL_SetSurfaceAlphaMod                |   y   |
SDL_SetSurfaceBlendMode               |   y   |
SDL_SetSurfaceColorMod                |   y   |
SDL_SetSurfacePalette                 |   n   |
SDL_SetSurfaceRLE                     |   y   |
SDL_SetTextInputRect                  |   y   |
SDL_SetTextureAlphaMod                |   y   |
SDL_SetTextureBlendMode               |   y   |
SDL_SetTextureColorMod                |   y   |
SDL_SetThreadPriority                 |   n   |
SDL_SetWindowBordered                 |   n   |
SDL_SetWindowBrightness               |   y   |
SDL_SetWindowData                     |   y   |
SDL_SetWindowDisplayMode              |   y   |
SDL_SetWindowFullscreen               |   y   |
SDL_SetWindowGammaRamp                |   y   |
SDL_SetWindowGrab                     |   y   |
SDL_SetWindowHitTest                  |   n   |
SDL_SetWindowIcon                     |   y   |
SDL_SetWindowMaximumSize              |   y   |
SDL_SetWindowMinimumSize              |   y   |
SDL_SetWindowPosition                 |   y   |
SDL_SetWindowSize                     |   y   |
SDL_SetWindowTitle                    |   y   |
SDL_ShowCursor                        |   n   |
SDL_ShowMessageBox                    |   n   |
SDL_ShowSimpleMessageBox              |   n   |
SDL_ShowWindow                        |   y   |
SDL_StartTextInput                    |   y   |
SDL_StopTextInput                     |   y   |
SDL_Swap16                            |   n   |
SDL_Swap32                            |   n   |
SDL_Swap64                            |   n   |
SDL_SwapBE16                          |   n   |
SDL_SwapBE32                          |   n   |
SDL_SwapBE64                          |   n   |
SDL_SwapFloat                         |   n   |
SDL_SwapFloatBE                       |   n   |
SDL_SwapFloatLE                       |   n   |
SDL_SwapLE16                          |   n   |
SDL_SwapLE32                          |   n   |
SDL_SwapLE64                          |   n   |
SDL_TICKS_PASSED                      |   n   |
SDL_TLSCreate                         |   n   |
SDL_TLSGet                            |   n   |
SDL_TLSSet                            |   n   |
SDL_ThreadID                          |   n   |
SDL_TriggerBreakpoint                 |   n   |
SDL_TryLockMutex                      |   n   |
SDL_UnionRect                         |   n   |
SDL_UnloadObject                      |   y   |
SDL_UnlockAudio                       |   n   |
SDL_UnlockAudioDevice                 |   n   |
SDL_UnlockMutex                       |   n   |
SDL_UnlockSurface                     |   y   |
SDL_UnlockTexture                     |   y   |
SDL_UpdateTexture                     |   n   |
SDL_UpdateWindowSurface               |   n   |
SDL_UpdateWindowSurfaceRects          |   y   |
SDL_UpdateYUVTexture                  |   n   |
SDL_VERSION                           |   n   |      *
SDL_VERSIONNUM                        |   n   |      *
SDL_VERSION_ATLEAST                   |   n   |      *
SDL_VideoInit                         |   y   |
SDL_VideoQuit                         |   y   |
SDL_WaitEvent                         |   n   |
SDL_WaitEventTimeout                  |   n   |
SDL_WaitThread                        |   n   |
SDL_WarpMouseGlobal                   |   y   |
SDL_WarpMouseInWindow                 |   y   |
SDL_WasInit                           |   y   |
SDL_WinRTGetFSPathUNICODE             |   n   |
SDL_WinRTGetFSPathUTF8                |   n   |
SDL_WinRTRunApp                       |   n   |
SDL_WriteBE16                         |   n   |
SDL_WriteBE32                         |   n   |
SDL_WriteBE64                         |   n   |
SDL_WriteLE16                         |   n   |
SDL_WriteLE32                         |   n   |
SDL_WriteLE64                         |   n   |
SDL_acos                              |   n   |
SDL_assert                            |   n   |
SDL_assert_paranoid                   |   n   |
SDL_assert_release                    |   n   |

### SDL_image

Name                                  | Bound | Won't bind
--------------------------------------|-------|-----------
IMG_Linked_Version                    |   y   |
IMG_Init                              |   y   |
IMG_Quit                              |   y   |
IMG_Load                              |   y   |
IMG_Load_RW                           |   n   |
IMG_LoadTyped_RW                      |   y   |
IMG_LoadTexture                       |   n   |
IMG_LoadTexture_RW                    |   n   |
IMG_LoadTextureTyped_RW               |   n   |
IMG_LoadCUR_RW                        |   n   |
IMG_LoadICO_RW                        |   n   |
IMG_LoadBMP_RW                        |   n   |
IMG_LoadPNM_RW                        |   n   |
IMG_LoadXPM_RW                        |   n   |
IMG_LoadXCF_RW                        |   n   |
IMG_LoadPCX_RW                        |   n   |
IMG_LoadGIF_RW                        |   n   |
IMG_LoadJPG_RW                        |   n   |
IMG_LoadTIF_RW                        |   n   |
IMG_LoadPNG_RW                        |   n   |
IMG_LoadTGA_RW                        |   n   |
IMG_LoadLBM_RW                        |   n   |
IMG_LoadXV_RW                         |   n   |
IMG_ReadXPMFromArray                  |   n   |
IMG_isCUR                             |   n   |
IMG_isICO                             |   n   |
IMG_isBMP                             |   n   |
IMG_isPNM                             |   n   |
IMG_isXPM                             |   n   |
IMG_isXCF                             |   n   |
IMG_isPCX                             |   n   |
IMG_isGIF                             |   n   |
IMG_isJPG                             |   n   |
IMG_isTIF                             |   n   |
IMG_isPNG                             |   n   |
IMG_isLBM                             |   n   |
IMG_isXV                              |   n   |
IMG_SavePNG                           |   y   |
IMG_SavePNG_RW                        |   n   |
IMG_SetError                          |   n   |      *
IMG_GetError                          |   n   |      *

### SDL_ttf

Name                                  | Bound | Won't bind
--------------------------------------|-------|-----------
TTF_Init                              |   y   |
TTF_WasInit                           |   n   |      *
TTF_Quit                              |   y   |
TTF_GetError                          |   n   |      *
TTF_SetError                          |   n   |      *
TTF_OpenFont                          |   n   |      *
TTF_OpenFontRW                        |   n   |      *
TTF_OpenFontIndex                     |   n   |      *
TTF_OpenFontIndexRW                   |   y   |
TTF_CloseFont                         |   y   |
TTF_ByteSwappedUNICODE                |   n   |
TTF_GetFontStyle                      |   y   |
TTF_SetFontStyle                      |   y   |
TTF_GetFontOutline                    |   y   |
TTF_SetFontOutline                    |   y   |
TTF_GetFontHinting                    |   y   |
TTF_SetFontHinting                    |   y   |
TTF_GetFontKerning                    |   y   |
TTF_SetFontKerning                    |   y   |
TTF_FontHeight                        |   y   |
TTF_FontAscent                        |   y   |
TTF_FontDescent                       |   y   |
TTF_FontLineSkip                      |   y   |
TTF_FontFaces                         |   y   |
TTF_FontFaceIsFixedWidth              |   y   |
TTF_FontFaceFamilyName                |   y   |
TTF_FontFaceStyleName                 |   y   |
TTF_GlyphIsProvided                   |   n   |
TTF_GlyphMetrics                      |   n   |
TTF_SizeText                          |   y   |
TTF_SizeUTF8                          |   y   |
TTF_SizeUNICODE                       |   n   |
TTF_RenderText_Solid                  |   y   |
TTF_RenderUTF8_Solid                  |   y   |
TTF_RenderUNICODE_Solid               |   n   |
TTF_RenderGlyph_Solid                 |   n   |
TTF_RenderText_Shaded                 |   y   |
TTF_RenderUTF8_Shaded                 |   y   |
TTF_RenderUNICODE_Shaded              |   n   |
TTF_RenderGlyph_Shaded                |   n   |
TTF_RenderText_Blended                |   y   |
TTF_RenderUTF8_Blended                |   y   |
TTF_RenderUNICODE_Blended             |   n   |
TTF_RenderGlyph_Blended               |   n   |
TTF_Linked_Version                    |   y   |
SDL_TTF_MAJOR_VERSION                 |   y   |
SDL_TTF_MINOR_VERSION                 |   y   |
SDL_TTF_PATCHLEVEL                    |   y   |
