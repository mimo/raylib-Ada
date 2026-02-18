------------------------------------------------------------------------------------
--  Window and Graphics Device Functions
------------------------------------------------------------------------------------

package Raylib.Window is
    --  [[  Window-related functions ]]  --
    procedure Init (Width, Height : Positive; Title : String);

    --// Close window and unload OpenGL context
    procedure Close
    with Import, Convention => C, External_Name => "CloseWindow";

    --// Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
    function Should_Close return Boolean;

    --// Check if window has been initialized successfully
    function Is_Window_Ready return Boolean;

    --// Check if window is currently fullscreen
    function Is_Window_Fullscreen return Boolean;

    --// Check if window is currently hidden
    function Is_Window_Hidden return Boolean;

    --// Check if window is currently minimized
    function Is_Window_Minimized return Boolean;

    --// Check if window is currently maximized
    function Is_Window_Maximized return Boolean;

    --// Check if window is currently focused
    function Is_Window_Focused return Boolean;

    --// Check if window has been resized last frame
    function Is_Window_Resized return Boolean;

    --// Check if one specific window flag is enabled
    function Is_Window_State (Flag : unsigned) return Boolean;

    --// Set window configuration state using flags
    procedure Set_Window_State (Flags : unsigned)
    with Import, Convention => C, External_Name => "SetWindowState";

    --// Clear window configuration state flags
    procedure Clear_Window_State (Flags : unsigned)
    with Import, Convention => C, External_Name => "ClearWindowState";

    --// Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
    procedure Toggle_Fullscreen
    with Import, Convention => C, External_Name => "ToggleFullscreen";

    --// Toggle window state: borderless windowed (only PLATFORM_DESKTOP)
    procedure Toggle_Borderless_Windowed
    with Import, Convention => C, External_Name => "ToggleBorderlessWindowed";

    --// Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
    procedure Maximize_Window
    with Import, Convention => C, External_Name => "MaximizeWindow";

    --// Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
    procedure Minimize_Window
    with Import, Convention => C, External_Name => "MinimizeWindow";

    --// Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
    procedure Restore_Window
    with Import, Convention => C, External_Name => "RestoreWindow";

    --// Set icon for window (single image, RGBA 32bit)
    procedure Set_Window_Icon (Icon_Image : Image)
    with Import, Convention => C, External_Name => "SetWindowIcon";

    --// Set icon for window (multiple images, RGBA 32bit)
    procedure Set_Window_Icons (Images : access Image; Count : int)
    with Import, Convention => C, External_Name => "SetWindowIcons";

    --// Set title for window (only PLATFORM_DESKTOP and PLATFORM_WEB)
    procedure Set_Window_Title (Title : String);

    --// Set window position on screen (only PLATFORM_DESKTOP)
    procedure Set_Window_Position (X, Y : int)
    with Import, Convention => C, External_Name => "SetWindowPosition";

    --// Set monitor for the current window
    procedure Set_Window_Monitor (Monitor : int)
    with Import, Convention => C, External_Name => "SetWindowMonitor";

    --// Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
    procedure Set_Window_Min_Size (Width, Height : int)
    with Import, Convention => C, External_Name => "SetWindowMinSize";

    --// Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
    procedure Set_Window_Max_Size (Width, Height : int)
    with Import, Convention => C, External_Name => "SetWindowMaxSize";

    --// Set window dimensions
    procedure Set_Window_Size (Width, Height : int)
    with Import, Convention => C, External_Name => "SetWindowSize";

    --// Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)
    procedure Set_Window_Opacity (Opacity : Float)
    with Import, Convention => C, External_Name => "SetWindowOpacity";

    --// Set window focused (only PLATFORM_DESKTOP)
    procedure Set_Window_Focused
    with Import, Convention => C, External_Name => "SetWindowFocused";

    --// Get native window handle
    function Get_Window_Handle return System.Address
    with Import, Convention => C, External_Name => "GetWindowHandle";

    -- Get current screen width
    function Get_Screen_Width return int
    with Import, Convention => C, External_Name => "GetScreenWidth";

    -- Get current screen height
    function Get_Screen_Height return int
    with Import, Convention => C, External_Name => "GetScreenHeight";

    --// Get current render width (it considers HiDPI)
    function Get_Render_Width return int
    with Import, Convention => C, External_Name => "GetRenderWidth";

    --// Get current render height (it considers HiDPI)
    function Get_Render_Height return int
    with Import, Convention => C, External_Name => "GetRenderHeight";

    --// Set clipboard text content
    procedure Set_Clipboard_Text (Text : String);

    --// Get clipboard text content
    function Get_Clipboard_Text return String;

    --// Get clipboard image content
    function Get_Clipboard_Image return Image
    with Import, Convention => C, External_Name => "GetClipboardImage";

    --// Enable waiting for events on EndDrawing(), no automatic event polling
    procedure Enable_Event_Waiting
    with Import, Convention => C, External_Name => "EnableEventWaiting";

    --// Disable waiting for events on EndDrawing(), automatic events polling
    procedure Disable_Event_Waiting
    with Import, Convention => C, External_Name => "DisableEventWaiting";

    --// Custom frame control functions
    procedure Swap_Screen_Buffer
    with Import, Convention => C, External_Name => "SwapScreenBuffer";
    procedure Poll_Input_Events
    with Import, Convention => C, External_Name => "PollInputEvents";
    procedure Wait_Time (Seconds : Interfaces.C.double)
    with Import, Convention => C, External_Name => "WaitTime";

    --  [[ Cursor-related functions ]] --
    procedure Show_Cursor
    with Import, Convention => C, External_Name => "ShowCursor";
    procedure Hide_Cursor
    with Import, Convention => C, External_Name => "HideCursor";

    function Is_Cursor_Hidden return Boolean;

    procedure Enable_Cursor
    with Import, Convention => C, External_Name => "EnableCursor";

    procedure Disable_Cursor
    with Import, Convention => C, External_Name => "DisableCursor";

    --// Check if cursor is on the screen
    function Is_Cursor_On_Screen return Boolean;

    --  [[  Drawing-related functions ]]  --
    procedure Clear_Background (Bg_Color : Color)
    with Import, Convention => C, External_Name => "ClearBackground";

    --// Setup canvas (framebuffer) to start drawing
    procedure Begin_Drawing
    with Import, Convention => C, External_Name => "BeginDrawing";

    --// End canvas drawing and swap buffers (double buffering)
    procedure End_Drawing
    with Import, Convention => C, External_Name => "EndDrawing";

    --// Begin 2D mode with custom camera (2D)
    procedure Begin_Mode_2D (Camera : Camera2D)
    with Import, Convention => C, External_Name => "BeginMode2D";

    --// Ends 2D mode with custom camera
    procedure End_Mode_2D
    with Import, Convention => C, External_Name => "EndMode2D";

    --// Begin 3D mode with custom camera (3D)
    procedure Begin_Mode_3D (Camera : Camera3D)
    with Import, Convention => C, External_Name => "BeginMode3D";

    --// Ends 3D mode and returns to default 2D orthographic mode
    procedure End_Mode_3D
    with Import, Convention => C, External_Name => "EndMode3D";

    --// Begin drawing to render texture
    procedure Begin_Texture_Mode (Target : RenderTexture2D)
    with Import, Convention => C, External_Name => "BeginTextureMode";

    --// Ends drawing to render texture
    procedure End_Texture_Mode
    with Import, Convention => C, External_Name => "EndTextureMode";

    --  [[  Screen-space-related functions  ]]  --

    function Get_Screen_To_World_Ray
       (position : Vector2; C : Camera3D) return Ray
    with Import, Convention => C, External_Name => "GetScreenToWorldRay";

    --// Get the screen space position for a 3D world space position
    function Get_World_To_Screen
       (Position : Vector3; Camera : Camera3D) return Vector2
    with Import, Convention => C, External_Name => "GetWorldToScreen";

    --// Get size position for a 3D world space position
    function Get_World_To_Screen_Ex
       (Position : Vector3; Camera : Camera3D; Width, Height : int)
        return Vector2
    with Import, Convention => C, External_Name => "GetWorldToScreenEx";

    --// Get the screen space position for a 2D camera world space position
    function Get_World_To_Screen_2D
       (Position : Vector2; Camera : Camera2D) return Vector2
    with Import, Convention => C, External_Name => "GetWorldToScreen2D";

    --// Get the world space position for a 2D camera screen space position
    function Get_Screen_To_World_2D
       (Position : Vector2; Camera : Camera2D) return Vector2
    with Import, Convention => C, External_Name => "GetScreenToWorld2D";

    --  [[  Timing-related functions  ]]  --

    procedure Set_Target_FPS (FPS : int)
    with Import, Convention => C, External_Name => "SetTargetFPS";

    --// Get time in seconds for last frame drawn (delta time)
    function Get_Frame_Time return Float
    with Import, Convention => C, External_Name => "GetFrameTime";

    --// Get elapsed time in seconds since InitWindow()
    function Get_Time return double
    with Import, Convention => C, External_Name => "GetTime";

    --// Get current FPS
    function Get_FPS return int
    with Import, Convention => C, External_Name => "GetFPS";

    --// Set the seed for the random number generator
    procedure Set_Random_Seed (Seed : unsigned)
    with Import, Convention => C, External_Name => "SetRandomSeed";

    --// Get a random value between min and max (both included)
    function Get_Random_Value (Min, Max : int) return int
    with Import, Convention => C, External_Name => "GetRandomValue";

    --  RLAPI int *LoadRandomSequence(unsigned int count, int min, int max); // Load random values sequence, no values repeated
    --  RLAPI void UnloadRandomSequence(int *sequence);                   // Unload random values sequence

    --// Takes a screenshot of current screen (filename extension defines format)
    procedure Take_Screenshot (Filename : String);

    --// Setup init configuration flags (view FLAGS)
    procedure Set_Config_Flags (Flags : unsigned)
    with Import, Convention => C, External_Name => "SetConfigFlags";

    --// Open URL with default system browser (if available)
    procedure Open_URL (URL : String);

end Raylib.Window;
