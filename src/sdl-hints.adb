--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with Interfaces.C.Strings;
with SDL.Error;

package body SDL.Hints is
   package C renames Interfaces.C;

   use type C.Strings.chars_ptr;

   Accelerometer_As_Joystick_Name                : aliased constant String := "SDL_ACCELEROMETER_AS_JOYSTICK";
   Allow_Alt_Tab_While_Grabbed_Name              : aliased constant String := "SDL_ALLOW_ALT_TAB_WHILE_GRABBED";
   Allow_Topmost_Name                            : aliased constant String := "SDL_ALLOW_TOPMOST";
   Android_Apk_Expansion_Main_File_Version_Name  : aliased constant String :=
     "SDL_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION";
   Android_Apk_Expansion_Patch_File_Version_Name : aliased constant String :=
     "SDL_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION";
   Android_Block_On_Pause_Name                   : aliased constant String := "SDL_ANDROID_BLOCK_ON_PAUSE";
   Android_Block_On_Pause_Pauseaudio_Name        : aliased constant String := "SDL_ANDROID_BLOCK_ON_PAUSE_PAUSEAUDIO";
   Android_Trap_Back_Button_Name                 : aliased constant String := "SDL_ANDROID_TRAP_BACK_BUTTON";
   App_Name_Name                                 : aliased constant String := "SDL_APP_NAME";
   Apple_TV_Controller_UI_Events_Name            : aliased constant String := "SDL_APPLE_TV_CONTROLLER_UI_EVENTS";
   Apple_TV_Remote_Allow_Rotation_Name           : aliased constant String := "SDL_APPLE_TV_REMOTE_ALLOW_ROTATION";
   Audio_Category_Name                           : aliased constant String := "SDL_AUDIO_CATEGORY";
   Audio_Device_App_Name                         : aliased constant String := "SDL_AUDIO_DEVICE_APP_NAME";
   Audio_Device_Stream_Name                      : aliased constant String := "SDL_AUDIO_DEVICE_STREAM_NAME";
   Audio_Device_Stream_Role_Name                 : aliased constant String := "SDL_AUDIO_DEVICE_STREAM_ROLE";
   Audio_Resampling_Mode_Name                    : aliased constant String := "SDL_AUDIO_RESAMPLING_MODE";
   Auto_Update_Joysticks_Name                    : aliased constant String := "SDL_AUTO_UPDATE_JOYSTICKS";
   Auto_Update_Sensors_Name                      : aliased constant String := "SDL_AUTO_UPDATE_SENSORS";
   BMP_Save_Legacy_Format_Name                   : aliased constant String := "SDL_BMP_SAVE_LEGACY_FORMAT";
   Display_Usable_Bounds_Name                    : aliased constant String := "SDL_DISPLAY_USABLE_BOUNDS";
   Emscripten_Asyncify_Name                      : aliased constant String := "SDL_EMSCRIPTEN_ASYNCIFY";
   Emscripten_Keyboard_Element_Name              : aliased constant String := "SDL_EMSCRIPTEN_KEYBOARD_ELEMENT";
   Enable_Screen_Keyboard_Name                   : aliased constant String := "SDL_ENABLE_SCREEN_KEYBOARD";
   Enable_Steam_Controllers_Name                 : aliased constant String := "SDL_ENABLE_STEAM_CONTROLLERS";
   Event_Logging_Name                            : aliased constant String := "SDL_EVENT_LOGGING";
   Hint_Force_Raise_Window_Name                  : aliased constant String := "SDL_HINT_FORCE_RAISEWINDOW";
   Frame_Buffer_Acceleration_Name                : aliased constant String := "SDL_FRAMEBUFFER_ACCELERATION";
   Game_Controller_Config_Name                   : aliased constant String := "SDL_GAMECONTROLLERCONFIG";
   Game_Controller_Config_File_Name              : aliased constant String := "SDL_GAMECONTROLLERCONFIG_FILE";
   Game_Controller_Ignore_Devices_Name           : aliased constant String := "SDL_GAMECONTROLLER_IGNORE_DEVICES";
   Game_Controller_Ignore_Devices_Except_Name    : aliased constant String :=
     "SDL_GAMECONTROLLER_IGNORE_DEVICES_EXCEPT";
   Game_Controller_Use_Button_Labels_Name        : aliased constant String := "SDL_GAMECONTROLLER_USE_BUTTON_LABELS";
   Grab_Keyboard_Name                            : aliased constant String := "SDL_GRAB_KEYBOARD";
   HID_API_Ignore_Devices_Name                   : aliased constant String := "SDL_HIDAPI_IGNORE_DEVICES";
   IOS_Idle_Timer_Disabled_Name                  : aliased constant String := "SDL_IOS_IDLE_TIMER_DISABLED";
   IME_Internal_Editing_Name                     : aliased constant String := "SDL_IME_INTERNAL_EDITING";
   IME_Show_Ui_Name                              : aliased constant String := "SDL_IME_SHOW_UI";
   IME_Support_Extended_Text_Name                : aliased constant String := "SDL_IME_SUPPORT_EXTENDED_TEXT";
   IOS_Hide_Home_Indicator_Name                  : aliased constant String := "SDL_IOS_HIDE_HOME_INDICATOR";
   Joystick_Allow_Background_Events_Name         : aliased constant String := "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS";
   Joystick_Arcade_Stick_Devices_Name            : aliased constant String := "SDL_JOYSTICK_ARCADESTICK_DEVICES";
   Joystick_Arcade_Stick_Devices_Excluded_Name   : aliased constant String :=
     "SDL_JOYSTICK_ARCADESTICK_DEVICES_EXCLUDED";
   Joystick_Blacklist_Devices_Name               : aliased constant String := "SDL_JOYSTICK_BLACKLIST_DEVICES";
   Joystick_Blacklist_Devices_Excluded_Name      : aliased constant String := "SDL_JOYSTICK_BLACKLIST_DEVICES_EXCLUDED";
   Joystick_Flight_Stick_Devices_Name            : aliased constant String := "SDL_JOYSTICK_FLIGHTSTICK_DEVICES";
   Joystick_Flight_Stick_Devices_Excluded_Name   : aliased constant String :=
     "SDL_JOYSTICK_FLIGHTSTICK_DEVICES_EXCLUDED";
   Joystick_Gamecube_Devices_Name                : aliased constant String := "SDL_JOYSTICK_GAMECUBE_DEVICES";
   Joystick_Gamecube_Devices_Excluded_Name       : aliased constant String := "SDL_JOYSTICK_GAMECUBE_DEVICES_EXCLUDED";
   Joystick_HID_API_Name                         : aliased constant String := "SDL_JOYSTICK_HIDAPI";
   Joystick_HID_API_Gamecube_Name                : aliased constant String := "SDL_JOYSTICK_HIDAPI_GAMECUBE";
   Joystick_Gamecube_Rumble_Brake_Name           : aliased constant String := "SDL_JOYSTICK_GAMECUBE_RUMBLE_BRAKE";
   Joystick_HID_API_Joy_Cons_Name                : aliased constant String := "SDL_JOYSTICK_HIDAPI_JOY_CONS";
   Joystick_HID_API_Combine_Joy_Cons_Name        : aliased constant String := "SDL_JOYSTICK_HIDAPI_COMBINE_JOY_CONS";
   Joystick_HID_API_Vertical_Joy_Cons_Name       : aliased constant String := "SDL_JOYSTICK_HIDAPI_VERTICAL_JOY_CONS";
   Joystick_HID_API_Luna_Name                    : aliased constant String := "SDL_JOYSTICK_HIDAPI_LUNA";
   Joystick_HID_API_Nintendo_Classic_Name        : aliased constant String := "SDL_JOYSTICK_HIDAPI_NINTENDO_CLASSIC";
   Joystick_HID_API_Shield_Name                  : aliased constant String := "SDL_JOYSTICK_HIDAPI_SHIELD";
   Joystick_HID_API_PS3_Name                     : aliased constant String := "SDL_JOYSTICK_HIDAPI_PS3";
   Joystick_HID_API_PS4_Name                     : aliased constant String := "SDL_JOYSTICK_HIDAPI_PS4";
   Joystick_HID_API_PS4_Rumble_Name              : aliased constant String := "SDL_JOYSTICK_HIDAPI_PS4_RUMBLE";
   Joystick_HID_API_PS5_Name                     : aliased constant String := "SDL_JOYSTICK_HIDAPI_PS5";
   Joystick_HID_API_PS5_Player_LED_Name          : aliased constant String := "SDL_JOYSTICK_HIDAPI_PS5_PLAYER_LED";
   Joystick_HID_API_PS5_Rumble_Name              : aliased constant String := "SDL_JOYSTICK_HIDAPI_PS5_RUMBLE";
   Joystick_HID_API_Stadia_Name                  : aliased constant String := "SDL_JOYSTICK_HIDAPI_STADIA";
   Joystick_HID_API_Steam_Name                   : aliased constant String := "SDL_JOYSTICK_HIDAPI_STEAM";
   Joystick_HID_API_Steamdeck_Name               : aliased constant String := "SDL_JOYSTICK_HIDAPI_STEAMDECK";
   Joystick_HID_API_Switch_Name                  : aliased constant String := "SDL_JOYSTICK_HIDAPI_SWITCH";
   Joystick_HID_API_Switch_Home_LED_Name         : aliased constant String := "SDL_JOYSTICK_HIDAPI_SWITCH_HOME_LED";
   Joystick_HID_API_Joy_Con_Home_LED_Name        : aliased constant String := "SDL_JOYSTICK_HIDAPI_JOYCON_HOME_LED";
   Joystick_HID_API_Switch_Player_LED_Name       : aliased constant String := "SDL_JOYSTICK_HIDAPI_SWITCH_PLAYER_LED";
   Joystick_HID_API_WII_Name                     : aliased constant String := "SDL_JOYSTICK_HIDAPI_WII";
   Joystick_HID_API_WII_Player_LED_Name          : aliased constant String := "SDL_JOYSTICK_HIDAPI_WII_PLAYER_LED";
   Joystick_HID_API_Xbox_Name                    : aliased constant String := "SDL_JOYSTICK_HIDAPI_XBOX";
   Joystick_HID_API_Xbox_360_Name                : aliased constant String := "SDL_JOYSTICK_HIDAPI_XBOX_360";
   Joystick_HID_API_Xbox_360_Player_LED_Name     : aliased constant String := "SDL_JOYSTICK_HIDAPI_XBOX_360_PLAYER_LED";
   Joystick_HID_API_Xbox_360_Wireless_Name       : aliased constant String := "SDL_JOYSTICK_HIDAPI_XBOX_360_WIRELESS";
   Joystick_HID_API_Xbox_One_Name                : aliased constant String := "SDL_JOYSTICK_HIDAPI_XBOX_ONE";
   Joystick_HID_API_Xbox_One_Home_LED_Name       : aliased constant String := "SDL_JOYSTICK_HIDAPI_XBOX_ONE_HOME_LED";
   Joystick_IOKit_Name                           : aliased constant String := "SDL_JOYSTICK_IOKIT";
   Joystick_MFI_Name                             : aliased constant String := "SDL_JOYSTICK_MFI";
   Joystick_Raw_Input_Name                       : aliased constant String := "SDL_JOYSTICK_RAWINPUT";
   Joystick_Raw_Input_Correlate_XInput_Name      : aliased constant String := "SDL_JOYSTICK_RAWINPUT_CORRELATE_XINPUT";
   Joystick_ROG_Chakram_Name                     : aliased constant String := "SDL_JOYSTICK_ROG_CHAKRAM";
   Joystick_Thread_Name                          : aliased constant String := "SDL_JOYSTICK_THREAD";
   Joystick_Throttle_Devices_Name                : aliased constant String := "SDL_JOYSTICK_THROTTLE_DEVICES";
   Joystick_Throttle_Devices_Excluded_Name       : aliased constant String := "SDL_JOYSTICK_THROTTLE_DEVICES_EXCLUDED";
   Joystick_WGI_Name                             : aliased constant String := "SDL_JOYSTICK_WGI";
   Joystick_Wheel_Devices_Name                   : aliased constant String := "SDL_JOYSTICK_WHEEL_DEVICES";
   Joystick_Wheel_Devices_Excluded_Name          : aliased constant String := "SDL_JOYSTICK_WHEEL_DEVICES_EXCLUDED";
   Joystick_Zero_Centered_Devices_Name           : aliased constant String := "SDL_JOYSTICK_ZERO_CENTERED_DEVICES";
   KMS_DRM_Require_DRM_Master_Name               : aliased constant String := "SDL_KMSDRM_REQUIRE_DRM_MASTER";
   Joystick_Device_Name                          : aliased constant String := "SDL_JOYSTICK_DEVICE";
   Linux_Digital_Hats_Name                       : aliased constant String := "SDL_LINUX_DIGITAL_HATS";
   Linux_Hat_Deadzones_Name                      : aliased constant String := "SDL_LINUX_HAT_DEADZONES";
   Linux_Joystick_Classic_Name                   : aliased constant String := "SDL_LINUX_JOYSTICK_CLASSIC";
   Linux_Joystick_Deadzones_Name                 : aliased constant String := "SDL_LINUX_JOYSTICK_DEADZONES";
   Logging_Name                                  : aliased constant String := "SDL_LOGGING";
   Mac_Background_App_Name                       : aliased constant String := "SDL_MAC_BACKGROUND_APP";
   Mac_Ctrl_Click_Emulate_Right_Click_Name       : aliased constant String := "SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK";
   Mac_OpenGL_Async_Dispatch_Name                : aliased constant String := "SDL_MAC_OPENGL_ASYNC_DISPATCH";
   Mouse_Double_Click_Radius_Name                : aliased constant String := "SDL_MOUSE_DOUBLE_CLICK_RADIUS";
   Mouse_Double_Click_Time_Name                  : aliased constant String := "SDL_MOUSE_DOUBLE_CLICK_TIME";
   Mouse_Focus_Clickthrough_Name                 : aliased constant String := "SDL_MOUSE_FOCUS_CLICKTHROUGH";
   Mouse_Normal_Speed_Scale_Name                 : aliased constant String := "SDL_MOUSE_NORMAL_SPEED_SCALE";
   Mouse_Relative_Mode_Center_Name               : aliased constant String := "SDL_MOUSE_RELATIVE_MODE_CENTER";
   Mouse_Relative_Mode_Warp_Name                 : aliased constant String := "SDL_MOUSE_RELATIVE_MODE_WARP";
   Mouse_Relative_Scaling_Name                   : aliased constant String := "SDL_MOUSE_RELATIVE_SCALING";
   Mouse_Relative_Speed_Scale_Name               : aliased constant String := "SDL_MOUSE_RELATIVE_SPEED_SCALE";
   Mouse_Relative_System_Scale_Name              : aliased constant String := "SDL_MOUSE_RELATIVE_SYSTEM_SCALE";
   Mouse_Relative_Warp_Motion_Name               : aliased constant String := "SDL_MOUSE_RELATIVE_WARP_MOTION";
   Mouse_Relative_Cursor_Visible_Name            : aliased constant String := "SDL_MOUSE_RELATIVE_CURSOR_VISIBLE";
   Mouse_Touch_Events_Name                       : aliased constant String := "SDL_MOUSE_TOUCH_EVENTS";
   Mouse_Auto_Capture_Name                       : aliased constant String := "SDL_MOUSE_AUTO_CAPTURE";
   No_Signal_Handlers_Name                       : aliased constant String := "SDL_NO_SIGNAL_HANDLERS";
   OpenGL_ES_Driver_Name                         : aliased constant String := "SDL_OPENGL_ES_DRIVER";
   IOS_Orientations_Name_Name                    : aliased constant String := "SDL_IOS_ORIENTATIONS";
   Hint_Orientations_Name                        : aliased constant String := "SDL_IOS_ORIENTATIONS";
   Poll_Sentinel_Name                            : aliased constant String := "SDL_POLL_SENTINEL";
   Preferred_Locales_Name                        : aliased constant String := "SDL_PREFERRED_LOCALES";
   Qtwayland_Content_Orientation_Name            : aliased constant String := "SDL_QTWAYLAND_CONTENT_ORIENTATION";
   Qtwayland_Window_Flags_Name                   : aliased constant String := "SDL_QTWAYLAND_WINDOW_FLAGS";
   Render_Batching_Name                          : aliased constant String := "SDL_RENDER_BATCHING";
   Render_Line_Method_Name                       : aliased constant String := "SDL_RENDER_LINE_METHOD";
   Render_Direct3D11_Debug_Name                  : aliased constant String := "SDL_RENDER_DIRECT3D11_DEBUG";
   Render_Direct3D_Threadsafe_Name               : aliased constant String := "SDL_RENDER_DIRECT3D_THREADSAFE";
   Render_Driver_Name                            : aliased constant String := "SDL_RENDER_DRIVER";
   Render_Logical_Size_Mode_Name                 : aliased constant String := "SDL_RENDER_LOGICAL_SIZE_MODE";
   Render_OpenGL_Shaders_Name                    : aliased constant String := "SDL_RENDER_OPENGL_SHADERS";
   Render_Scale_Quality_Name                     : aliased constant String := "SDL_RENDER_SCALE_QUALITY";
   Render_VSync_Name                             : aliased constant String := "SDL_RENDER_VSYNC";
   Render_Metal_Prefer_Low_Power_Device_Name     : aliased constant String :=
     "SDL_RENDER_METAL_PREFER_LOW_POWER_DEVICE";
   ROG_Gamepad_Mice_Name                         : aliased constant String := "SDL_ROG_GAMEPAD_MICE";
   ROG_Gamepad_Mice_Excluded_Name                : aliased constant String := "SDL_ROG_GAMEPAD_MICE_EXCLUDED";
   PS2_Dynamic_VSync_Name                        : aliased constant String := "SDL_PS2_DYNAMIC_VSYNC";
   Return_Key_Hides_IME_Name                     : aliased constant String := "SDL_RETURN_KEY_HIDES_IME";
   RPi_Video_Layer_Name                          : aliased constant String := "SDL_RPI_VIDEO_LAYER";
   Screensaver_Inhibit_Activity_Name_Name        : aliased constant String := "SDL_SCREENSAVER_INHIBIT_ACTIVITY_NAME";
   Thread_Force_Realtime_Time_Critical_Name      : aliased constant String := "SDL_THREAD_FORCE_REALTIME_TIME_CRITICAL";
   Thread_Priority_Policy_Name                   : aliased constant String := "SDL_THREAD_PRIORITY_POLICY";
   Thread_Stack_Size_Name                        : aliased constant String := "SDL_THREAD_STACK_SIZE";
   Timer_Resolution_Name                         : aliased constant String := "SDL_TIMER_RESOLUTION";
   Touch_Mouse_Events_Name                       : aliased constant String := "SDL_TOUCH_MOUSE_EVENTS";
   Vita_Touch_Mouse_Device_Name                  : aliased constant String := "SDL_HINT_VITA_TOUCH_MOUSE_DEVICE";
   TV_Remote_As_Joystick_Name                    : aliased constant String := "SDL_TV_REMOTE_AS_JOYSTICK";
   Video_Allow_Screensaver_Name                  : aliased constant String := "SDL_VIDEO_ALLOW_SCREENSAVER";
   Video_Double_Buffer_Name                      : aliased constant String := "SDL_VIDEO_DOUBLE_BUFFER";
   Video_EGL_Allow_Transparency_Name             : aliased constant String := "SDL_VIDEO_EGL_ALLOW_TRANSPARENCY";
   Video_External_Context_Name                   : aliased constant String := "SDL_VIDEO_EXTERNAL_CONTEXT";
   Video_High_DPI_Disabled_Name                  : aliased constant String := "SDL_VIDEO_HIGHDPI_DISABLED";
   Video_Mac_Fullscreen_Spaces_Name              : aliased constant String := "SDL_VIDEO_MAC_FULLSCREEN_SPACES";
   Video_Minimize_On_Focus_Loss_Name             : aliased constant String := "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS";
   Video_Wayland_Allow_Libdecor_Name             : aliased constant String := "SDL_VIDEO_WAYLAND_ALLOW_LIBDECOR";
   Video_Wayland_Prefer_Libdecor_Name            : aliased constant String := "SDL_VIDEO_WAYLAND_PREFER_LIBDECOR";
   Video_Wayland_Mode_Emulation_Name             : aliased constant String := "SDL_VIDEO_WAYLAND_MODE_EMULATION";
   Video_Wayland_Emulate_Mouse_Warp_Name         : aliased constant String := "SDL_VIDEO_WAYLAND_EMULATE_MOUSE_WARP";
   Video_Window_Share_Pixel_Format_Name          : aliased constant String := "SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT";
   Video_Foreign_Window_OpenGL_Name              : aliased constant String := "SDL_VIDEO_FOREIGN_WINDOW_OPENGL";
   Video_Foreign_Window_Vulkan_Name              : aliased constant String := "SDL_VIDEO_FOREIGN_WINDOW_VULKAN";
   Video_Win_D3D_Compiler_Name                   : aliased constant String := "SDL_VIDEO_WIN_D3DCOMPILER";
   Video_X11_Force_EGL_Name                      : aliased constant String := "SDL_VIDEO_X11_FORCE_EGL";
   Video_X11_Net_WM_Bypass_Compositor_Name       : aliased constant String := "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR";
   Video_X11_Net_WM_Ping_Name                    : aliased constant String := "SDL_VIDEO_X11_NET_WM_PING";
   Video_X11_Window_Visualid_Name                : aliased constant String := "SDL_VIDEO_X11_WINDOW_VISUALID";
   Video_X11_Xinerama_Name                       : aliased constant String := "SDL_VIDEO_X11_XINERAMA";
   Video_X11_XRandR_Name                         : aliased constant String := "SDL_VIDEO_X11_XRANDR";
   Video_X11_XVidMode_Name                       : aliased constant String := "SDL_VIDEO_X11_XVIDMODE";
   Wave_Fact_Chunk_Name                          : aliased constant String := "SDL_WAVE_FACT_CHUNK";
   Wave_RIFF_Chunk_Size_Name                     : aliased constant String := "SDL_WAVE_RIFF_CHUNK_SIZE";
   Wave_Truncation_Name                          : aliased constant String := "SDL_WAVE_TRUNCATION";
   Windows_Disable_Thread_Naming_Name            : aliased constant String := "SDL_WINDOWS_DISABLE_THREAD_NAMING";
   Windows_Enable_Menu_Mnemonics_Name            : aliased constant String := "SDL_WINDOWS_ENABLE_MENU_MNEMONICS";
   Windows_Enable_Messageloop_Name               : aliased constant String := "SDL_WINDOWS_ENABLE_MESSAGELOOP";
   Windows_Force_Mutex_Critical_Sections_Name    : aliased constant String :=
     "SDL_WINDOWS_FORCE_MUTEX_CRITICAL_SECTIONS";
   Windows_Force_Semaphore_Kernel_Name           : aliased constant String := "SDL_WINDOWS_FORCE_SEMAPHORE_KERNEL";
   Windows_Intresource_Icon_Name                 : aliased constant String := "SDL_WINDOWS_INTRESOURCE_ICON";
   Windows_Intresource_Icon_Small_Name           : aliased constant String := "SDL_WINDOWS_INTRESOURCE_ICON_SMALL";
   Windows_No_Close_On_Alt_F4_Name               : aliased constant String := "SDL_WINDOWS_NO_CLOSE_ON_ALT_F4";
   Windows_Use_D3D9_Ex_Name                      : aliased constant String := "SDL_WINDOWS_USE_D3D9EX";
   Windows_DPI_Awareness_Name                    : aliased constant String := "SDL_WINDOWS_DPI_AWARENESS";
   Windows_DPI_Scaling_Name                      : aliased constant String := "SDL_WINDOWS_DPI_SCALING";
   Window_No_Activation_When_Shown_Name          : aliased constant String := "SDL_WINDOW_NO_ACTIVATION_WHEN_SHOWN";
   Winrt_Handle_Back_Button_Name                 : aliased constant String := "SDL_WINRT_HANDLE_BACK_BUTTON";
   Winrt_Privacy_Policy_Label_Name               : aliased constant String := "SDL_WINRT_PRIVACY_POLICY_LABEL";
   Winrt_Privacy_Policy_URL_Name                 : aliased constant String := "SDL_WINRT_PRIVACY_POLICY_URL";
   X11_Force_Override_Redirect_Name              : aliased constant String := "SDL_X11_FORCE_OVERRIDE_REDIRECT";
   XInput_Enabled_Name                           : aliased constant String := "SDL_XINPUT_ENABLED";
   Directinput_Enabled_Name                      : aliased constant String := "SDL_DIRECTINPUT_ENABLED";
   XInput_Use_Old_Joystick_Mapping_Name          : aliased constant String := "SDL_XINPUT_USE_OLD_JOYSTICK_MAPPING";
   Audio_Include_Monitors_Name                   : aliased constant String := "SDL_AUDIO_INCLUDE_MONITORS";
   X11_Window_Type_Name                          : aliased constant String := "SDL_X11_WINDOW_TYPE";
   Quit_On_Last_Window_Close_Name                : aliased constant String := "SDL_QUIT_ON_LAST_WINDOW_CLOSE";
   Video_Driver_Name                             : aliased constant String := "SDL_VIDEODRIVER";
   Audio_Driver_Name                             : aliased constant String := "SDL_AUDIODRIVER";
   KMS_DRM_Device_Index_Name                     : aliased constant String := "SDL_KMSDRM_DEVICE_INDEX";
   Trackpad_Is_Touch_Only_Name                   : aliased constant String := "SDL_TRACKPAD_IS_TOUCH_ONLY";
   Shutdown_DBUS_On_Quit_Name                    : aliased constant String := "SDL_SHUTDOWN_DBUS_ON_QUIT";

   type Hint_Names is access constant String;

   Hint_Name_Map : constant array (Hint'Range) of Hint_Names :=
     (Accelerometer_As_Joystick_Name'Access,
      Allow_Alt_Tab_While_Grabbed_Name'Access,
      Allow_Topmost_Name'Access,
      Android_Apk_Expansion_Main_File_Version_Name'Access,
      Android_Apk_Expansion_Patch_File_Version_Name'Access,
      Android_Block_On_Pause_Name'Access,
      Android_Block_On_Pause_Pauseaudio_Name'Access,
      Android_Trap_Back_Button_Name'Access,
      App_Name_Name'Access,
      Apple_TV_Controller_UI_Events_Name'Access,
      Apple_TV_Remote_Allow_Rotation_Name'Access,
      Audio_Category_Name'Access,
      Audio_Device_App_Name'Access,
      Audio_Device_Stream_Name'Access,
      Audio_Device_Stream_Role_Name'Access,
      Audio_Resampling_Mode_Name'Access,
      Auto_Update_Joysticks_Name'Access,
      Auto_Update_Sensors_Name'Access,
      BMP_Save_Legacy_Format_Name'Access,
      Display_Usable_Bounds_Name'Access,
      Emscripten_Asyncify_Name'Access,
      Emscripten_Keyboard_Element_Name'Access,
      Enable_Screen_Keyboard_Name'Access,
      Enable_Steam_Controllers_Name'Access,
      Event_Logging_Name'Access,
      Hint_Force_Raise_Window_Name'Access,
      Frame_Buffer_Acceleration_Name'Access,
      Game_Controller_Config_Name'Access,
      Game_Controller_Config_File_Name'Access,
      Game_Controller_Ignore_Devices_Name'Access,
      Game_Controller_Ignore_Devices_Except_Name'Access,
      Game_Controller_Use_Button_Labels_Name'Access,
      Grab_Keyboard_Name'Access,
      HID_API_Ignore_Devices_Name'Access,
      IOS_Idle_Timer_Disabled_Name'Access,
      IME_Internal_Editing_Name'Access,
      IME_Show_Ui_Name'Access,
      IME_Support_Extended_Text_Name'Access,
      IOS_Hide_Home_Indicator_Name'Access,
      Joystick_Allow_Background_Events_Name'Access,
      Joystick_Arcade_Stick_Devices_Name'Access,
      Joystick_Arcade_Stick_Devices_Excluded_Name'Access,
      Joystick_Blacklist_Devices_Name'Access,
      Joystick_Blacklist_Devices_Excluded_Name'Access,
      Joystick_Flight_Stick_Devices_Name'Access,
      Joystick_Flight_Stick_Devices_Excluded_Name'Access,
      Joystick_Gamecube_Devices_Name'Access,
      Joystick_Gamecube_Devices_Excluded_Name'Access,
      Joystick_HID_API_Name'Access,
      Joystick_HID_API_Gamecube_Name'Access,
      Joystick_Gamecube_Rumble_Brake_Name'Access,
      Joystick_HID_API_Joy_Cons_Name'Access,
      Joystick_HID_API_Combine_Joy_Cons_Name'Access,
      Joystick_HID_API_Vertical_Joy_Cons_Name'Access,
      Joystick_HID_API_Luna_Name'Access,
      Joystick_HID_API_Nintendo_Classic_Name'Access,
      Joystick_HID_API_Shield_Name'Access,
      Joystick_HID_API_PS3_Name'Access,
      Joystick_HID_API_PS4_Name'Access,
      Joystick_HID_API_PS4_Rumble_Name'Access,
      Joystick_HID_API_PS5_Name'Access,
      Joystick_HID_API_PS5_Player_LED_Name'Access,
      Joystick_HID_API_PS5_Rumble_Name'Access,
      Joystick_HID_API_Stadia_Name'Access,
      Joystick_HID_API_Steam_Name'Access,
      Joystick_HID_API_Steamdeck_Name'Access,
      Joystick_HID_API_Switch_Name'Access,
      Joystick_HID_API_Switch_Home_LED_Name'Access,
      Joystick_HID_API_Joy_Con_Home_LED_Name'Access,
      Joystick_HID_API_Switch_Player_LED_Name'Access,
      Joystick_HID_API_WII_Name'Access,
      Joystick_HID_API_WII_Player_LED_Name'Access,
      Joystick_HID_API_Xbox_Name'Access,
      Joystick_HID_API_Xbox_360_Name'Access,
      Joystick_HID_API_Xbox_360_Player_LED_Name'Access,
      Joystick_HID_API_Xbox_360_Wireless_Name'Access,
      Joystick_HID_API_Xbox_One_Name'Access,
      Joystick_HID_API_Xbox_One_Home_LED_Name'Access,
      Joystick_IOKit_Name'Access,
      Joystick_MFI_Name'Access,
      Joystick_Raw_Input_Name'Access,
      Joystick_Raw_Input_Correlate_XInput_Name'Access,
      Joystick_ROG_Chakram_Name'Access,
      Joystick_Thread_Name'Access,
      Joystick_Throttle_Devices_Name'Access,
      Joystick_Throttle_Devices_Excluded_Name'Access,
      Joystick_WGI_Name'Access,
      Joystick_Wheel_Devices_Name'Access,
      Joystick_Wheel_Devices_Excluded_Name'Access,
      Joystick_Zero_Centered_Devices_Name'Access,
      KMS_DRM_Require_DRM_Master_Name'Access,
      Joystick_Device_Name'Access,
      Linux_Digital_Hats_Name'Access,
      Linux_Hat_Deadzones_Name'Access,
      Linux_Joystick_Classic_Name'Access,
      Linux_Joystick_Deadzones_Name'Access,
      Logging_Name'Access,
      Mac_Background_App_Name'Access,
      Mac_Ctrl_Click_Emulate_Right_Click_Name'Access,
      Mac_OpenGL_Async_Dispatch_Name'Access,
      Mouse_Double_Click_Radius_Name'Access,
      Mouse_Double_Click_Time_Name'Access,
      Mouse_Focus_Clickthrough_Name'Access,
      Mouse_Normal_Speed_Scale_Name'Access,
      Mouse_Relative_Mode_Center_Name'Access,
      Mouse_Relative_Mode_Warp_Name'Access,
      Mouse_Relative_Scaling_Name'Access,
      Mouse_Relative_Speed_Scale_Name'Access,
      Mouse_Relative_System_Scale_Name'Access,
      Mouse_Relative_Warp_Motion_Name'Access,
      Mouse_Relative_Cursor_Visible_Name'Access,
      Mouse_Touch_Events_Name'Access,
      Mouse_Auto_Capture_Name'Access,
      No_Signal_Handlers_Name'Access,
      OpenGL_ES_Driver_Name'Access,
      IOS_Orientations_Name_Name'Access,
      Hint_Orientations_Name'Access,
      Poll_Sentinel_Name'Access,
      Preferred_Locales_Name'Access,
      Qtwayland_Content_Orientation_Name'Access,
      Qtwayland_Window_Flags_Name'Access,
      Render_Batching_Name'Access,
      Render_Line_Method_Name'Access,
      Render_Direct3D11_Debug_Name'Access,
      Render_Direct3D_Threadsafe_Name'Access,
      Render_Driver_Name'Access,
      Render_Logical_Size_Mode_Name'Access,
      Render_OpenGL_Shaders_Name'Access,
      Render_Scale_Quality_Name'Access,
      Render_VSync_Name'Access,
      Render_Metal_Prefer_Low_Power_Device_Name'Access,
      ROG_Gamepad_Mice_Name'Access,
      ROG_Gamepad_Mice_Excluded_Name'Access,
      PS2_Dynamic_VSync_Name'Access,
      Return_Key_Hides_IME_Name'Access,
      RPi_Video_Layer_Name'Access,
      Screensaver_Inhibit_Activity_Name_Name'Access,
      Thread_Force_Realtime_Time_Critical_Name'Access,
      Thread_Priority_Policy_Name'Access,
      Thread_Stack_Size_Name'Access,
      Timer_Resolution_Name'Access,
      Touch_Mouse_Events_Name'Access,
      Vita_Touch_Mouse_Device_Name'Access,
      TV_Remote_As_Joystick_Name'Access,
      Video_Allow_Screensaver_Name'Access,
      Video_Double_Buffer_Name'Access,
      Video_EGL_Allow_Transparency_Name'Access,
      Video_External_Context_Name'Access,
      Video_High_DPI_Disabled_Name'Access,
      Video_Mac_Fullscreen_Spaces_Name'Access,
      Video_Minimize_On_Focus_Loss_Name'Access,
      Video_Wayland_Allow_Libdecor_Name'Access,
      Video_Wayland_Prefer_Libdecor_Name'Access,
      Video_Wayland_Mode_Emulation_Name'Access,
      Video_Wayland_Emulate_Mouse_Warp_Name'Access,
      Video_Window_Share_Pixel_Format_Name'Access,
      Video_Foreign_Window_OpenGL_Name'Access,
      Video_Foreign_Window_Vulkan_Name'Access,
      Video_Win_D3D_Compiler_Name'Access,
      Video_X11_Force_EGL_Name'Access,
      Video_X11_Net_WM_Bypass_Compositor_Name'Access,
      Video_X11_Net_WM_Ping_Name'Access,
      Video_X11_Window_Visualid_Name'Access,
      Video_X11_Xinerama_Name'Access,
      Video_X11_XRandR_Name'Access,
      Video_X11_XVidMode_Name'Access,
      Wave_Fact_Chunk_Name'Access,
      Wave_RIFF_Chunk_Size_Name'Access,
      Wave_Truncation_Name'Access,
      Windows_Disable_Thread_Naming_Name'Access,
      Windows_Enable_Menu_Mnemonics_Name'Access,
      Windows_Enable_Messageloop_Name'Access,
      Windows_Force_Mutex_Critical_Sections_Name'Access,
      Windows_Force_Semaphore_Kernel_Name'Access,
      Windows_Intresource_Icon_Name'Access,
      Windows_Intresource_Icon_Small_Name'Access,
      Windows_No_Close_On_Alt_F4_Name'Access,
      Windows_Use_D3D9_Ex_Name'Access,
      Windows_DPI_Awareness_Name'Access,
      Windows_DPI_Scaling_Name'Access,
      Window_No_Activation_When_Shown_Name'Access,
      Winrt_Handle_Back_Button_Name'Access,
      Winrt_Privacy_Policy_Label_Name'Access,
      Winrt_Privacy_Policy_URL_Name'Access,
      X11_Force_Override_Redirect_Name'Access,
      XInput_Enabled_Name'Access,
      Directinput_Enabled_Name'Access,
      XInput_Use_Old_Joystick_Mapping_Name'Access,
      Audio_Include_Monitors_Name'Access,
      X11_Window_Type_Name'Access,
      Quit_On_Last_Window_Close_Name'Access,
      Video_Driver_Name'Access,
      Audio_Driver_Name'Access,
      KMS_DRM_Device_Index_Name'Access,
      Trackpad_Is_Touch_Only_Name'Access,
      Shutdown_DBUS_On_Quit_Name'Access);

   function Value (H : in Hint) return String with
     Inline => True;

   function Value (H : in Hint) return String is
   begin
      return Hint_Name_Map (H).all;
   end Value;

   function Get (Name : in Hint) return String is
      function SDL_Get_Hint (C_Str : in C.char_array) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetHint";

      C_Str : constant C.Strings.chars_ptr := SDL_Get_Hint (C.To_C (Value (Name)));
   begin
      if C_Str = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (C_Str);
   end Get;

   procedure Set (Name : in Hint; Value : in String) is
      function SDL_Set_Hint (Name, Value : in C.char_array) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetHint";
   begin
      if SDL_Set_Hint (C.To_C (Hints.Value (Name)), C.To_C (Value)) = SDL_False then
         raise Hint_Error with SDL.Error.Get;
      end if;
   end Set;

   procedure Set (Name : in Hint; Value : in String; Priority : in Priorities) is
      function SDL_Set_Hint_With_Priority (Name, Value : in C.char_array; P : in Priorities) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetHintWithPriority";
   begin
      if SDL_Set_Hint_With_Priority (C.To_C (Hints.Value (Name)), C.To_C (Value), Priority) = SDL_False then
         raise Hint_Error with SDL.Error.Get;
      end if;
   end Set;
end SDL.Hints;
