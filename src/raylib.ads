with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;
---
--  for reference use :
--  https://www.adaic.org/resources/add_content/standards/05rm/html/RM-B-3.html
--  https://raw.githubusercontent.com/raysan5/raylib/4.5.0/src/raylib.h

package raylib is

   subtype int           is Interfaces.C.int;
   subtype c_float       is Interfaces.C.C_float;
   subtype unsigned      is Interfaces.C.unsigned;
   subtype unsigned_char is Interfaces.C.unsigned_char;
   subtype bool          is Interfaces.C.Extensions.bool;
   subtype chars_ptr     is Interfaces.C.Strings.chars_ptr;

   RAYLIB_VERSION_MAJOR : constant := 5;
   RAYLIB_VERSION_MINOR : constant := 0;
   RAYLIB_VERSION_PATCH : constant := 0;
   RAYLIB_VERSION : aliased constant String := "5.0";

   PI : constant := 3.14159265358979323846;

   type Vector2 is record
      x, y : Float;
   end record
      with Convention => C_Pass_By_Copy;

   type Vector3 is record
      x, y, z : Float;
   end record
      with Convention => C_Pass_By_Copy;

   type Vector4 is record
      x, y, z, w : Float;
   end record
      with Convention => C_Pass_By_Copy;

   type Quaternion is new Vector4;

   type Matrix is record
      m0, m4, m8,  m12 : Float;
      m1, m5, m9,  m13 : Float;
      m2, m6, m10, m14 : Float;
      m3, m7, m11, m15 : Float;
   end record
      with Convention => C_Pass_By_Copy;

   type Color is record
      r, g, b, a : unsigned_char;
   end record
      with Convention => C_Pass_By_Copy;

   type Rectangle is record
     x, y, width, height : Float;
   end record
      with Convention => C_Pass_By_Copy;

   type NPatch_Info is record
      sourceRec : Rectangle;
      left, top, right, bottom : int;
      layout : int;
   end record
      with Convention => C_Pass_By_Copy;

    FLAG_VSYNC_HINT               : constant := 16#00000040#;  -- Set to try enabling V-Sync on GPU
    FLAG_FULLSCREEN_MODE          : constant := 16#00000002#;  -- Set to run program in fullscreen
    FLAG_WINDOW_RESIZABLE         : constant := 16#00000004#;  -- Set to allow resizable window
    FLAG_WINDOW_UNDECORATED       : constant := 16#00000008#;  -- Set to disable window decoration (frame and buttons)
    FLAG_WINDOW_HIDDEN            : constant := 16#00000080#;  -- Set to hide window
    FLAG_WINDOW_MINIMIZED         : constant := 16#00000200#;  -- Set to minimize window (iconify)
    FLAG_WINDOW_MAXIMIZED         : constant := 16#00000400#;  -- Set to maximize window (expanded to monitor)
    FLAG_WINDOW_UNFOCUSED         : constant := 16#00000800#;  -- Set to window non focused
    FLAG_WINDOW_TOPMOST           : constant := 16#00001000#;  -- Set to window always on top
    FLAG_WINDOW_ALWAYS_RUN        : constant := 16#00000100#;  -- Set to allow windows running while minimized
    FLAG_WINDOW_TRANSPARENT       : constant := 16#00000010#;  -- Set to allow transparent framebuffer
    FLAG_WINDOW_HIGHDPI           : constant := 16#00002000#;  -- Set to support HighDPI
    FLAG_WINDOW_MOUSE_PASSTHROUGH : constant := 16#00004000#;  -- Set to support mouse passthrough#, only supported when FLAG_WINDOW_UNDECORATED
    FLAG_BORDERLESS_WINDOWED_MODE : constant := 16#00008000#;  -- Set to run program in borderless windowed mode
    FLAG_MSAA_4X_HINT             : constant := 16#00000020#;  -- Set to try enabling MSAA 4X
    FLAG_INTERLACED_HINT          : constant := 16#00010000#;  -- Set to try enabling interlaced video format (for V3D)

   --  Keyboard Function Keys
   type Keys is (
      KEY_NULL,
      KEY_SPACE,

      --  Alphanumeric keys
      KEY_APOSTROPHE,
      KEY_COMMA,
      KEY_MINUS,
      KEY_PERIOD,
      KEY_SLASH,
      KEY_ZERO,
      KEY_ONE,
      KEY_TWO,
      KEY_THREE,
      KEY_FOUR,
      KEY_FIVE,
      KEY_SIX,
      KEY_SEVEN,
      KEY_EIGHT,
      KEY_NINE,
      KEY_SEMICOLON,
      KEY_EQUAL,
      KEY_A,
      KEY_B,
      KEY_C,
      KEY_D,
      KEY_E,
      KEY_F,
      KEY_G,
      KEY_H,
      KEY_I,
      KEY_J,
      KEY_K,
      KEY_L,
      KEY_M,
      KEY_N,
      KEY_O,
      KEY_P,
      KEY_Q,
      KEY_R,
      KEY_S,
      KEY_T,
      KEY_U,
      KEY_V,
      KEY_W,
      KEY_X,
      KEY_Y,
      KEY_Z,

      KEY_LEFT_BRACKET,
      KEY_BACKSLASH,
      KEY_RIGHT_BRACKET,
      KEY_GRAVE,

      --  Function keys
      KEY_ESCAPE,
      KEY_ENTER,
      KEY_TAB,
      KEY_BACKSPACE,
      KEY_INSERT,
      KEY_DELETE,
      KEY_RIGHT,
      KEY_LEFT,
      KEY_DOWN,
      KEY_UP,
      KEY_PAGE_UP,
      KEY_PAGE_DOWN,
      KEY_HOME,
      KEY_END,
      KEY_CAPS_LOCK,
      KEY_SCROLL_LOCK,
      KEY_NUM_LOCK,
      KEY_PRINT_SCREEN,
      KEY_PAUSE,
      KEY_F1,
      KEY_F2,
      KEY_F3,
      KEY_F4,
      KEY_F5,
      KEY_F6,
      KEY_F7,
      KEY_F8,
      KEY_F9,
      KEY_F10,
      KEY_F11,
      KEY_F12,

      --  Keypad keys
      KEY_KP_0,
      KEY_KP_1,
      KEY_KP_2,
      KEY_KP_3,
      KEY_KP_4,
      KEY_KP_5,
      KEY_KP_6,
      KEY_KP_7,
      KEY_KP_8,
      KEY_KP_9,
      KEY_KP_DECIMAL,
      KEY_KP_DIVIDE,
      KEY_KP_MULTIPLY,
      KEY_KP_SUBTRACT,
      KEY_KP_ADD,
      KEY_KP_ENTER,
      KEY_KP_EQUAL,

      KEY_LEFT_SHIFT,
      KEY_LEFT_CONTROL,
      KEY_LEFT_ALT,
      KEY_LEFT_SUPER,
      KEY_RIGHT_SHIFT,
      KEY_RIGHT_CONTROL,
      KEY_RIGHT_ALT,
      KEY_RIGHT_SUPER,
      KEY_KB_MENU)
   with Convention => C;

   for Keys use (
      KEY_NULL   => 0,
      KEY_SPACE  => 32,

      KEY_APOSTROPHE => 39,
      KEY_COMMA      => 44,
      KEY_MINUS      => 45,
      KEY_PERIOD     => 46,
      KEY_SLASH      => 47,
      KEY_ZERO       => 48,
      KEY_ONE        => 49,
      KEY_TWO        => 50,
      KEY_THREE      => 51,
      KEY_FOUR       => 52,
      KEY_FIVE       => 53,
      KEY_SIX        => 54,
      KEY_SEVEN      => 55,
      KEY_EIGHT      => 56,
      KEY_NINE       => 57,
      KEY_SEMICOLON  => 59,
      KEY_EQUAL      => 61,
      KEY_A => 65,
      KEY_B => 66,
      KEY_C => 67,
      KEY_D => 68,
      KEY_E => 69,
      KEY_F => 70,
      KEY_G => 71,
      KEY_H => 72,
      KEY_I => 73,
      KEY_J => 74,
      KEY_K => 75,
      KEY_L => 76,
      KEY_M => 77,
      KEY_N => 78,
      KEY_O => 79,
      KEY_P => 80,
      KEY_Q => 81,
      KEY_R => 82,
      KEY_S => 83,
      KEY_T => 84,
      KEY_U => 85,
      KEY_V => 86,
      KEY_W => 87,
      KEY_X => 88,
      KEY_Y => 89,
      KEY_Z => 90,

      KEY_LEFT_BRACKET  => 91,
      KEY_BACKSLASH     => 92,
      KEY_RIGHT_BRACKET => 93,
      KEY_GRAVE         => 96,

      KEY_ESCAPE => 256,
      KEY_ENTER  => 257,
      KEY_TAB    => 258,
      KEY_BACKSPACE => 259,
      KEY_INSERT => 260,
      KEY_DELETE => 261,
      KEY_RIGHT  => 262,
      KEY_LEFT   => 263,
      KEY_DOWN   => 264,
      KEY_UP     => 265,
      KEY_PAGE_UP       => 266,
      KEY_PAGE_DOWN     => 267,
      KEY_HOME          => 268,
      KEY_END           => 269,
      KEY_CAPS_LOCK     => 280,
      KEY_SCROLL_LOCK   => 281,
      KEY_NUM_LOCK      => 282,
      KEY_PRINT_SCREEN  => 283,
      KEY_PAUSE         => 284,
      KEY_F1            => 290,
      KEY_F2            => 291,
      KEY_F3            => 292,
      KEY_F4            => 293,
      KEY_F5            => 294,
      KEY_F6            => 295,
      KEY_F7            => 296,
      KEY_F8            => 297,
      KEY_F9            => 298,
      KEY_F10           => 299,
      KEY_F11           => 300,
      KEY_F12           => 301,

      KEY_KP_0        => 320,
      KEY_KP_1        => 321,
      KEY_KP_2        => 322,
      KEY_KP_3        => 323,
      KEY_KP_4        => 324,
      KEY_KP_5        => 325,
      KEY_KP_6        => 326,
      KEY_KP_7        => 327,
      KEY_KP_8        => 328,
      KEY_KP_9        => 329,
      KEY_KP_DECIMAL  => 330,
      KEY_KP_DIVIDE   => 331,
      KEY_KP_MULTIPLY => 332,
      KEY_KP_SUBTRACT => 333,
      KEY_KP_ADD      => 334,
      KEY_KP_ENTER    => 335,
      KEY_KP_EQUAL    => 336,

      KEY_LEFT_SHIFT    => 340,
      KEY_LEFT_CONTROL  => 341,
      KEY_LEFT_ALT      => 342,
      KEY_LEFT_SUPER    => 343,
      KEY_RIGHT_SHIFT   => 344,
      KEY_RIGHT_CONTROL => 345,
      KEY_RIGHT_ALT     => 346,
      KEY_RIGHT_SUPER   => 347,
      KEY_KB_MENU       => 348);

   type Mouse_Cursor is (
     MOUSE_CURSOR_DEFAULT,
     MOUSE_CURSOR_ARROW,
     MOUSE_CURSOR_IBEAM,
     MOUSE_CURSOR_POINTING_HAND,
     MOUSE_CURSOR_RESIZE_EW,
     MOUSE_CURSOR_RESIZE_NS,
     MOUSE_CURSOR_RESIZE_NWSE,
     MOUSE_CURSOR_RESIZE_NESW,
     MOUSE_CURSOR_RESIZE_ALL,
     MOUSE_CURSOR_NOT_ALLOWED
     )
   with Convention => C;
 
   type Mouse_Button is (
       MOUSE_BUTTON_LEFT,
       MOUSE_BUTTON_RIGHT,
       MOUSE_BUTTON_MIDDLE,
       MOUSE_BUTTON_SIDE,
       MOUSE_BUTTON_EXTRA,
       MOUSE_BUTTON_FORWARD,
       MOUSE_BUTTON_BACK)
   with Convention => C;

   type Gamepad_Button is (
      GAMEPAD_BUTTON_UNKNOWN,
      GAMEPAD_BUTTON_LEFT_FACE_UP,
      GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
      GAMEPAD_BUTTON_LEFT_FACE_DOWN,
      GAMEPAD_BUTTON_LEFT_FACE_LEFT,
      GAMEPAD_BUTTON_RIGHT_FACE_UP,
      GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,
      GAMEPAD_BUTTON_RIGHT_FACE_DOWN,
      GAMEPAD_BUTTON_RIGHT_FACE_LEFT,
      GAMEPAD_BUTTON_LEFT_TRIGGER_1,
      GAMEPAD_BUTTON_LEFT_TRIGGER_2,
      GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
      GAMEPAD_BUTTON_RIGHT_TRIGGER_2,
      GAMEPAD_BUTTON_MIDDLE_LEFT,
      GAMEPAD_BUTTON_MIDDLE,
      GAMEPAD_BUTTON_MIDDLE_RIGHT,
      GAMEPAD_BUTTON_LEFT_THUMB,
      GAMEPAD_BUTTON_RIGHT_THUMB)
   with Convention => C;

   type Gamepad_Axis is
     (GAMEPAD_AXIS_LEFT_X,
      GAMEPAD_AXIS_LEFT_Y,
      GAMEPAD_AXIS_RIGHT_X,
      GAMEPAD_AXIS_RIGHT_Y,
      GAMEPAD_AXIS_LEFT_TRIGGER,
      GAMEPAD_AXIS_RIGHT_TRIGGER)
   with Convention => C;

   type Pixel_Format is (
      PIXELFORMAT_UNCOMPRESSED_GRAYSCALE,         -- 8 bit per pixel (no alpha)
      PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA,        -- 8*2 bpp (2 channels)
      PIXELFORMAT_UNCOMPRESSED_R5G6B5,            -- 16 bpp
      PIXELFORMAT_UNCOMPRESSED_R8G8B8,            -- 24 bpp
      PIXELFORMAT_UNCOMPRESSED_R5G5B5A1,          -- 16 bpp (1 bit alpha)
      PIXELFORMAT_UNCOMPRESSED_R4G4B4A4,          -- 16 bpp (4 bit alpha)
      PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,          -- 32 bpp
      PIXELFORMAT_UNCOMPRESSED_R32,               -- 32 bpp (1 channel - float)
      PIXELFORMAT_UNCOMPRESSED_R32G32B32,         -- 32*3 bpp (3 channels - float)
      PIXELFORMAT_UNCOMPRESSED_R32G32B32A32,      -- 32*4 bpp (4 channels - float)
      PIXELFORMAT_UNCOMPRESSED_R16,               -- 16 bpp (1 channel - half float)
      PIXELFORMAT_UNCOMPRESSED_R16G16B16,         -- 16*3 bpp (3 channels - half float)
      PIXELFORMAT_UNCOMPRESSED_R16G16B16A16,      -- 16*4 bpp (4 channels - half float)
      PIXELFORMAT_COMPRESSED_DXT1_RGB,            -- 4 bpp (no alpha)
      PIXELFORMAT_COMPRESSED_DXT1_RGBA,           -- 4 bpp (1 bit alpha)
      PIXELFORMAT_COMPRESSED_DXT3_RGBA,           -- 8 bpp
      PIXELFORMAT_COMPRESSED_DXT5_RGBA,           -- 8 bpp
      PIXELFORMAT_COMPRESSED_ETC1_RGB,            -- 4 bpp
      PIXELFORMAT_COMPRESSED_ETC2_RGB,            -- 4 bpp
      PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA,       -- 8 bpp
      PIXELFORMAT_COMPRESSED_PVRT_RGB,            -- 4 bpp
      PIXELFORMAT_COMPRESSED_PVRT_RGBA,           -- 4 bpp
      PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA,       -- 8 bpp
      PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA)       -- 2 bpp
   with Convention => C;

   for Pixel_Format'Size use Interfaces.C.int'Size;
   for Pixel_Format use (
      PIXELFORMAT_UNCOMPRESSED_GRAYSCALE    => 1,     -- 8 bit per pixel (no alpha)
      PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA   => 2,     -- 8*2 bpp (2 channels)
      PIXELFORMAT_UNCOMPRESSED_R5G6B5       => 3,     -- 16 bpp
      PIXELFORMAT_UNCOMPRESSED_R8G8B8       => 4,     -- 24 bpp
      PIXELFORMAT_UNCOMPRESSED_R5G5B5A1     => 5,     -- 16 bpp (1 bit alpha)
      PIXELFORMAT_UNCOMPRESSED_R4G4B4A4     => 6,     -- 16 bpp (4 bit alpha)
      PIXELFORMAT_UNCOMPRESSED_R8G8B8A8     => 7,     -- 32 bpp
      PIXELFORMAT_UNCOMPRESSED_R32          => 8,     -- 32 bpp (1 channel - float)
      PIXELFORMAT_UNCOMPRESSED_R32G32B32    => 9,     -- 32*3 bpp (3 channels - float)
      PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 => 10,    -- 32*4 bpp (4 channels - float)
      PIXELFORMAT_UNCOMPRESSED_R16          => 11,    -- 16 bpp (1 channel - half float)
      PIXELFORMAT_UNCOMPRESSED_R16G16B16    => 12,    -- 16*3 bpp (3 channels - half float)
      PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 => 13,    -- 16*4 bpp (4 channels - half float)      
      PIXELFORMAT_COMPRESSED_DXT1_RGB       => 14,    -- 4 bpp (no alpha)
      PIXELFORMAT_COMPRESSED_DXT1_RGBA      => 15,    -- 4 bpp (1 bit alpha)
      PIXELFORMAT_COMPRESSED_DXT3_RGBA      => 16,    -- 8 bpp
      PIXELFORMAT_COMPRESSED_DXT5_RGBA      => 17,    -- 8 bpp
      PIXELFORMAT_COMPRESSED_ETC1_RGB       => 18,    -- 4 bpp
      PIXELFORMAT_COMPRESSED_ETC2_RGB       => 19,    -- 4 bpp
      PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA  => 20,    -- 8 bpp
      PIXELFORMAT_COMPRESSED_PVRT_RGB       => 21,    -- 4 bpp
      PIXELFORMAT_COMPRESSED_PVRT_RGBA      => 22,    -- 4 bpp
      PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA  => 23,    -- 8 bpp
      PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA  => 24);   -- 2 bpp

   type NPatch_Layout is (
      NPATCH_NINE_PATCH,
      NPATCH_THREE_PATCH_VERTICAL,
      NPATCH_THREE_PATCH_HORIZONTAL)
   with Convention => C;
   
   type Image is record
     data : System.Address;
     width, height, mipmaps : int;
     format : int;
     --  format : Pixel_Format;
   end record
      with Convention => C_Pass_By_Copy;

   type Texture2D is record
     id : unsigned;
     width, height : int;
     mimaps : int;
     format : int;
     --format : Pixel_Format;
   end record
      with Convention => C_Pass_By_Copy;

   subtype Texture is Texture2D;
   subtype TextureCubemap is Texture2D;

   type RenderTexture2D is record
      id : unsigned;
      texture, depth : Texture2D;
   end record
      with Convention => C_Pass_By_Copy;
   subtype RenderTexture is RenderTexture2D;

   --  Font character info
   type GlyphInfo is record
       value : int;              -- Character value (Unicode)
       offsetX : int;            -- Character offset X when drawing
       offsetY : int;            -- Character offset Y when drawing
       advanceX : int;           -- Character advance position X
       img : Image;              -- Character image data, named image in raylib.h but conflict with the type
   end record
      with Convention => C_Pass_By_Copy;

   type Font  is record
      baseSize : int;         -- Base size (default chars height)
      glyphCount : int;       -- Number of glyph characters
      glyphPadding : int;     -- Padding around the glyph characters
      texture  : Texture2D;   -- Texture atlas containing the glyphs
      recs : access Rectangle;  -- Rectangles in texture for the glyphs
      glyphs : access GlyphInfo; --  Glyphs info data
   end record
      with Convention => C_Pass_By_Copy;

   --  Camera system modes
   type CameraMode is (
      CAMERA_CUSTOM,
      CAMERA_FREE,
      CAMERA_ORBITAL,
      CAMERA_FIRST_PERSON,
      CAMERA_THIRD_PERSON)
   with Convention => C;

   --  Camera projection modes
   type CameraType is (
      CAMERA_PERSPECTIVE,
      CAMERA_ORTHOGRAPHIC)
   with Convention => C;

   --  Camera type, defines a camera position/orientation in 3d space
   type Camera3D is record
      position : Vector3;  -- Camera position
      target   : Vector3;  -- Camera target it looks-at
      up   : Vector3;      -- Camera up vector (rotation over its axis)
      --  Camera field-of-view apperture in Y (degrees) in perspective,
      --  used as near plane width in orthographic
      fovy : Float;
      projection : CameraType;  -- Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
   end record
      with Convention => C_Pass_By_Copy;
   subtype Camera is Camera3D;

   type P_Camera is access all Camera3D;

   type Camera2D is record
      offset : Vector2;    -- Camera offset (displacement from target)
      target : Vector2;    -- Camera target (rotation and zoom origin)
      rotation : Float;    -- Camera rotation in degrees
      zoom : Float;        -- Camera zoom (scaling), should be 1.0f by default
   end record;

   type AudioStream is record
      buffer : System.Address;
      processor : System.Address;
      sampleRate, sampleSize, channels : unsigned;
   end record
      with Convention => C_Pass_By_Copy;

   type Sound is record
      stream : AudioStream;
      frameCount : unsigned;
   end record
      with Convention => C_Pass_By_Copy;

   type Log is (LOG_ALL, LOG_TRACE, LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_FATAL, LOG_NONE);
   for Log'Size use int'Size;

   LIGHTGRAY : constant Color := (200, 200, 200, 255);
   GRAY      : constant Color := (130, 130, 130, 255);
   DARKGRAY  : constant Color := (80,  80,  80,  255);
   YELLOW    : constant Color := (253, 249, 0,   255);
   GOLD      : constant Color := (255, 203, 0,   255);
   ORANGE    : constant Color := (255, 161, 0,   255);
   PINK      : constant Color := (255, 109, 194, 255);
   RED       : constant Color := (230, 41,  55,  255);
   MAROON    : constant Color := (190, 33,  55,  255);
   GREEN     : constant Color := (0,   228, 48,  255);
   LIME      : constant Color := (0, 158, 47, 255);
   DARKGREEN : constant Color := (0, 117, 44, 255);
   SKYBLUE   : constant Color := (102, 191, 255, 255);
   BLUE      : constant Color := (0, 121, 241, 255);
   DARKBLUE  : constant Color := (0, 82, 172, 255);
   PURPLE    : constant Color := (200, 122, 255, 255);
   VIOLET    : constant Color := (135, 60, 190, 255);
   DARKPURPLE : constant Color := (112, 31, 126, 255);
   BEIGE     : constant Color := (211, 176, 131, 255);
   BROWN     : constant Color := (127, 106, 79, 255);
   DARKBROWN : constant Color := (76, 63, 47, 255);

   WHITE     : constant Color := (255, 255, 255, 255);
   BLACK     : constant Color := (0, 0, 0, 255);
   BLANK     : constant Color := (0, 0, 0, 0);
   MAGENTA   : constant Color := (255, 0, 255, 255);
   RAYWHITE  : constant Color := (245, 245, 245, 255);

   ------------------------------------------------------------------------------------
   --  Window and Graphics Device Functions
   ------------------------------------------------------------------------------------
   package window is
      --  [[  Window-related functions ]]  --
      procedure init (width, height : Positive; title : String);

      --// Close window and unload OpenGL context
      procedure close
         with
            Import => True,
            Convention => C,
            External_Name => "CloseWindow";

      --// Check if application should close (KEY_ESCAPE pressed or windows close icon clicked)
      function  should_close return Boolean;

      --// Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
      procedure toggle_fullscreen
         with
            Import => True,
            Convention => C,
            External_Name => "ToggleFullscreen";
                    
      --// Toggle window state: borderless windowed (only PLATFORM_DESKTOP)
      procedure toggle_borderless_windowed
         with
            Import => True,
            Convention => C,
            External_Name => "ToggleBorderlessWindowed";
                                
      --// Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
      procedure maximize_window
         with
            Import => True,
            Convention => C,
            External_Name => "MaximizeWindow";
                                
      --// Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
      procedure minimize_window
         with
            Import => True,
            Convention => C,
            External_Name => "MinimizeWindow";
                                
      --// Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
      procedure restore_window
         with
            Import => True,
            Convention => C,
            External_Name => "RestoreWindow";
      
      --// Set icon for window (single image, RGBA 32bit, only PLATFORM_DESKTOP)
      procedure set_window_icon (icon_image : Image)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowIcon";                         
                  
      --// Set title for window (only PLATFORM_DESKTOP and PLATFORM_WEB)
      procedure set_window_title (title : String)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowTitle";
                     
      --// Set window position on screen (only PLATFORM_DESKTOP)
      procedure set_window_position (x, y : int)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowPosition";
                     
      --// Set monitor for the current window
      procedure set_window_monitor (monitor : int)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowMonitor";
           
      --// Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
      procedure set_window_min_size (width, height : int)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowMinSize";
             
      --// Set window maximum dimensions (for FLAG_WINDOW_RESIZABLE)
      procedure set_window_max_size (width, height : int)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowMaxSize";
                       
      --// Set window dimensions
      procedure set_window_size (width, height : int)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowSize";
                             
      --// Set window opacity [0.0f..1.0f] (only PLATFORM_DESKTOP)
      procedure set_window_opacity (opacity : Float)
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowOpacity";
                                
      --// Set window focused (only PLATFORM_DESKTOP)
      procedure set_window_focused
         with
            Import => True,
            Convention => C,
            External_Name => "SetWindowFocused";
                             
      --// Get native window handle
      function get_window_handle return System.Address
         with
            Import => True,
            Convention => C,
            External_Name => "GetWindowHandle";

      -- Get current screen width
      function get_screen_width return int
      with import => True, Convention => C, External_Name => "GetScreenWidth";

      -- Get current screen height
      function get_screen_height return int
      with import => True, Convention => C, External_Name => "GetScreenHeight";
                                 
      --// Get current render width (it considers HiDPI)
      function get_render_width return int
         with
            Import => True,
            Convention => C,
            External_Name => "GetRenderWidth";
                                     
      --// Get current render height (it considers HiDPI)
      function get_render_height return int
         with
            Import => True,
            Convention => C,
            External_Name => "GetRenderHeight";
                  
      --// Set clipboard text content
      procedure set_clipboard_text (text : String)
         with
            Import => True,
            Convention => C,
            External_Name => "SetClipboardText";

      --RLAPI const char *GetClipboardText(void);                         
      --// Get clipboard text content
      function get_clipboard_text return chars_ptr
         with
            Import => True,
            Convention => C,
            External_Name => "GetClipboardText";
                               
      --// Enable waiting for events on EndDrawing(), no automatic event polling
      procedure enable_event_waiting
         with
            Import => True,
            Convention => C,
            External_Name => "EnableEventWaiting";
                      
      --// Disable waiting for events on EndDrawing(), automatic events polling
      procedure disable_event_waiting
         with
            Import => True,
            Convention => C,
            External_Name => "DisableEventWaiting";

      --  [[ Cursor-related functions ]] --
      procedure show_cursor
         with
            Import => True,
            Convention => C,
            External_Name => "ShowCursor";
      procedure hide_cursor
         with
            Import => True,
            Convention => C,
            External_Name => "HideCursor";
      
      function is_cursor_hidden
         return Boolean;

      procedure enable_cursor
         with
            Import => True,
            Convention => C,
            External_Name => "EnableCursor";

      procedure disable_cursor
         with
            Import => True,
            Convention => C,
            External_Name => "DisableCursor";

      --RLAPI bool IsCursorOnScreen(void);                                
      --// Check if cursor is on the screen
      function is_cursor_on_screen return Bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsCursorOnScreen";

      --  [[  Drawing-related functions ]]  --
      procedure clear_background (bg_color : Color)
         with
            Import,
            Convention => C,
            External_Name => "ClearBackground";

      --// Setup canvas (framebuffer) to start drawing
      procedure begin_drawing
         with
            Import,
            Convention => C,
            External_Name => "BeginDrawing";

      --// End canvas drawing and swap buffers (double buffering)
      procedure end_drawing
         with
            Import,
            Convention => C,
            External_Name => "EndDrawing";
                                                            
      --// Begin 2D mode with custom camera (2D)
      procedure begin_mode2D (camera : Camera2D)
         with
            Import,
            Convention => C,
            External_Name => "BeginMode2D";
                                    
      --// Ends 2D mode with custom camera
      procedure end_mode2D
         with
            Import,
            Convention => C,
            External_Name => "EndMode2D";
                           
      --// Begin 3D mode with custom camera (3D)
      procedure begin_mode3D (camera : Camera3D)
         with
            Import,
            Convention => C,
            External_Name => "BeginMode3D";
                                    
      --// Ends 3D mode and returns to default 2D orthographic mode
      procedure end_mode3D
         with
            Import,
            Convention => C,
            External_Name => "EndMode3D";

      procedure set_target_FPS (fps : int)
         with
            Import,
            Convention => C,
            External_Name => "SetTargetFPS";
                                 
      --// Get time in seconds for last frame drawn (delta time)
      function get_frame_time return Float
         with
            Import,
            Convention => C,
            External_Name => "GetFrameTime";
                                       
      --// Get elapsed time in seconds since InitWindow()
      function get_time return Float
         with
            Import,
            Convention => C,
            External_Name => "GetTime";
      
      --// Get current FPS
      function get_FPS return int
         with
            Import,
            Convention => C,
            External_Name => "GetFPS";

      --  // Custom frame control functions
      --  // NOTE: Those functions are intended for advance users that want full control over the frame processing
      --  // By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
      --  // To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
      --  RLAPI void SwapScreenBuffer(void);                                // Swap back buffer with front buffer (screen drawing)
      --  RLAPI void PollInputEvents(void);                                 // Register all input events
      --  RLAPI void WaitTime(double seconds);                              // Wait for some time (halt program execution)
                    
      --// Set the seed for the random number generator
      procedure set_random_seed (seed : unsigned)
         with
            Import,
            Convention => C,
            External_Name => "SetRandomSeed";
      
      --// Get a random value between min and max (both included)
      function get_random_value (min, max : int) return int
      with
         Import => True,
         Convention => C,
         External_Name => "GetRandomValue";
      
      --  RLAPI int *LoadRandomSequence(unsigned int count, int min, int max); // Load random values sequence, no values repeated
      --  RLAPI void UnloadRandomSequence(int *sequence);                   // Unload random values sequence

      --  // Misc. functions
      --  RLAPI void TakeScreenshot(const char *fileName);                  
      --// Takes a screenshot of current screen (filename extension defines format)
      procedure take_screenshot (filename : String)
         with
            Import => True,
            Convention => C,
            External_Name => "TakeScreenshot";
      
      --  RLAPI void SetConfigFlags(unsigned int flags);                    
      --// Setup init configuration flags (view FLAGS)
      procedure set_config_flags (flags : unsigned)
      with
         Import => True,
         Convention => C,
         External_Name => "SetConfigFlags";

      --  RLAPI void OpenURL(const char *url);                              
      --// Open URL with default system browser (if available)
      procedure open_URL (url : String)
      with
         Import => True,
         Convention => C,
         External_Name => "OpenURL";
   end window;

   package utils is
      --  // NOTE: Following functions implemented in module [utils]
      --  //------------------------------------------------------------------
      --  RLAPI void TraceLog(int logLevel, const char *text, ...);
      --// Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
      procedure trace_log (ltype : Log; text : String);

      --  RLAPI void SetTraceLogLevel(int logLevel);                        
      --// Set the current threshold (minimum) log level
      procedure set_trace_log_level (ltype : Log) 
      with
         Import => True,
         Convention => C,
         External_Name => "SetTraceLogLevel";

      --  RLAPI void *MemAlloc(unsigned int size);                          // Internal memory allocator
      --  RLAPI void *MemRealloc(void *ptr, unsigned int size);             // Internal memory reallocator
      --  RLAPI void MemFree(void *ptr);                                    // Internal memory free

      --  // Set custom callbacks
      --  // WARNING: Callbacks setup is intended for advance users
      --  RLAPI void SetTraceLogCallback(TraceLogCallback callback);         // Set custom trace log
      --  RLAPI void SetLoadFileDataCallback(LoadFileDataCallback callback); // Set custom file binary data loader
      --  RLAPI void SetSaveFileDataCallback(SaveFileDataCallback callback); // Set custom file binary data saver
      --  RLAPI void SetLoadFileTextCallback(LoadFileTextCallback callback); // Set custom file text data loader
      --  RLAPI void SetSaveFileTextCallback(SaveFileTextCallback callback); // Set custom file text data saver

      --  // Files management functions
      --  RLAPI unsigned char *LoadFileData(const char *fileName, int *dataSize); // Load file data as byte array (read)
      --  RLAPI void UnloadFileData(unsigned char *data);                   // Unload file data allocated by LoadFileData()
      --  RLAPI bool SaveFileData(const char *fileName, void *data, int dataSize); // Save data to file from byte array (write), returns true on success
      --  RLAPI bool ExportDataAsCode(const unsigned char *data, int dataSize, const char *fileName); // Export data to code (.h), returns true on success
      --  RLAPI char *LoadFileText(const char *fileName);                   // Load text data from file (read), returns a '\0' terminated string
      --  RLAPI void UnloadFileText(char *text);                            // Unload file text data allocated by LoadFileText()
      --  RLAPI bool SaveFileText(const char *fileName, char *text);        // Save text data to file (write), string must be '\0' terminated, returns true on success
      --  //------------------------------------------------------------------

      --  // File system functions
      --  RLAPI bool FileExists(const char *fileName);                      
      --// Check if file exists
      --  RLAPI bool DirectoryExists(const char *dirPath);                  
      --// Check if a directory path exists
      --  RLAPI bool IsFileExtension(const char *fileName, const char *ext); // Check file extension (including point: .png, .wav)
      --  RLAPI int GetFileLength(const char *fileName);                    --// Get file length in bytes (NOTE: GetFileSize() conflicts with windows.h)
      --  RLAPI const char *GetFileExtension(const char *fileName);         // Get pointer to extension for a filename string (includes dot: '.png')
      --  RLAPI const char *GetFileName(const char *filePath);              // Get pointer to filename for a path string
      --  RLAPI const char *GetFileNameWithoutExt(const char *filePath);    // Get filename string without extension (uses static string)
      --  RLAPI const char *GetDirectoryPath(const char *filePath);         // Get full path for a given fileName with path (uses static string)
      --  RLAPI const char *GetPrevDirectoryPath(const char *dirPath);      // Get previous directory path for a given path (uses static string)
      --  RLAPI const char *GetWorkingDirectory(void);                      
      --// Get current working directory (uses static string)
      --  RLAPI const char *GetApplicationDirectory(void);                  
      --// Get the directory of the running application (uses static string)
      --  RLAPI bool ChangeDirectory(const char *dir);                      
      --// Change working directory, return true on success
      --  RLAPI bool IsPathFile(const char *path);                          
      --// Check if a given path is a file or a directory
      --  RLAPI FilePathList LoadDirectoryFiles(const char *dirPath);       // Load directory filepaths
      --  RLAPI FilePathList LoadDirectoryFilesEx(const char *basePath, const char *filter, bool scanSubdirs); // Load directory filepaths with extension filtering and recursive directory scan
      --  RLAPI void UnloadDirectoryFiles(FilePathList files);              // Unload filepaths
      --  RLAPI bool IsFileDropped(void);                                   // Check if a file has been dropped into window
      --  RLAPI FilePathList LoadDroppedFiles(void);                        // Load dropped filepaths
      --  RLAPI void UnloadDroppedFiles(FilePathList files);                // Unload dropped filepaths
      --  RLAPI long GetFileModTime(const char *fileName);                  // Get file modification time (last write time)

      --  // Compression/Encoding functionality
      --  RLAPI unsigned char *CompressData(const unsigned char *data, int dataSize, int *compDataSize);        // Compress data (DEFLATE algorithm), memory must be MemFree()
      --  RLAPI unsigned char *DecompressData(const unsigned char *compData, int compDataSize, int *dataSize);  // Decompress data (DEFLATE algorithm), memory must be MemFree()
      --  RLAPI char *EncodeDataBase64(const unsigned char *data, int dataSize, int *outputSize);               // Encode data to Base64 string, memory must be MemFree()
      --  RLAPI unsigned char *DecodeDataBase64(const unsigned char *data, int *outputSize);                    // Decode Base64 string data, memory must be MemFree()

      --  // Automation events functionality
      --  RLAPI AutomationEventList LoadAutomationEventList(const char *fileName);                // Load automation events list from file, NULL for empty list, capacity = MAX_AUTOMATION_EVENTS
      --  RLAPI void UnloadAutomationEventList(AutomationEventList *list);                        // Unload automation events list from file
      --  RLAPI bool ExportAutomationEventList(AutomationEventList list, const char *fileName);   // Export automation events list as text file
      --  RLAPI void SetAutomationEventList(AutomationEventList *list);                           // Set automation event list to record to
      --  RLAPI void SetAutomationEventBaseFrame(int frame);                                      // Set automation event internal base frame to start recording
      --  RLAPI void StartAutomationEventRecording(void);                                         // Start recording automation events (AutomationEventList must be set)
      --  RLAPI void StopAutomationEventRecording(void);                                          // Stop recording automation events
      --  RLAPI void PlayAutomationEvent(AutomationEvent event);   
   end utils;

   ------------------------------------------------------------------------------------
   --  Input Handling Functions (Module: core)
   ------------------------------------------------------------------------------------
   package input is
      --  [[ Input-related functions: keyboard ]]  --

      --// Check if a key has been pressed once
      function is_key_pressed (key : Keys) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsKeyPressed";
                  
      --// Check if a key has been pressed again (Only PLATFORM_DESKTOP)
      function is_key_pressed_repeat (key : Keys) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsKeyPressedRepeat";
      
      --// Check if a key is being pressed
      function is_key_down (key : Keys) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsKeyDown";
      
      --// Check if a key has been released once
      function is_key_released (key : Keys) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsKeyReleased";
                              
      --// Check if a key is NOT being pressed
      function is_key_up (key : Keys) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsKeyUp";
     
      --// Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
      function get_key_pressed return Keys
         with 
            Import => True,
            Convention => C,
            External_Name => "GetKeyPressed";
      
      --// Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
      function get_char_pressed return int
         with 
            Import => True,
            Convention => C,
            External_Name => "GetCharPressed";
      
      --// Set a custom key to exit program (default is ESC)
      procedure set_exit_key (key : Keys)
         with
            Import => True,
            Convention => C,
            External_Name => "SetExitKey";

      --  [[ Input-related functions: gamepads  ]]  --
      function is_gamepad_available (gamepad : int) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsGamepadAvailable";

      function get_gamepad_name (gamepad : int) return String;

      --  Detect if a gamepad button has been pressed once
      function is_gamepad_button_pressed (
         gamepad : int;
         button : int)
         return bool
      with
         Import => True,
         Convention => C,
         External_Name => "IsGamepadButtonPressed";

      --// Check if a gamepad button is being pressed
      function is_gamepad_button_down (
         gamepad : int;
         button : Gamepad_Button)
         return bool
      with
         Import => True,
         Convention => C,
         External_Name => "IsGamepadButtonDown";

      --// Check if a gamepad button has been released once
      function is_gamepad_button_released (gamepad : int; button : Gamepad_Button)
         return bool
         with Import => True, Convention => C, External_Name => "IsGamepadButtonReleased";

      --// Check if a gamepad button is NOT being pressed
      function is_gamepad_button_up (gamepad : int; button : Gamepad_Button)
         return bool
         with Import => True, Convention => C, External_Name => "IsGamepadButtonUp";

      --  Get the last gamepad button pressed
      function get_gamepad_button_pressed return Gamepad_Button
      with Import => True, Convention => C, External_Name => "GetGamepadButtonPressed";

      --  Return gamepad axis count for a gamepad
      function get_gamepad_axis_count (gamepad : int) return int
      with Import => True, Convention => C, External_Name => "GetGamepadAxisCount";

      --  Return axis movement value for a gamepad axis
      function get_gamepad_axis_movement (
         gamepad : int;
         axis : Gamepad_Axis)
         return Float
      with
         Import => True,
         Convention => C,
         External_Name => "GetGamepadAxisMovement";
      --RLAPI int SetGamepadMappings(const char *mappings);
      --// Set internal gamepad mappings (SDL_GameControllerDB)

      --  [[ Input-related functions: mouse  ]] --

      --// Check if a mouse button has been pressed once
      function is_mouse_button_pressed (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonPressed";
      
      --// Check if a mouse button is being pressed
      function is_mouse_button_down (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonDown";

      --// Check if a mouse button has been released once      
      function is_mouse_button_released (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonReleased";
                     
      --// Check if a mouse button is NOT being pressed
      function is_mouse_button_up (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonUp";
      
      function get_mouse_position return Vector2
         with
            Import => True,
            Convention => C,
            External_Name => "GetMousePosition";

      function get_mouse_delta return Vector2
         with
            Import => True,
            Convention => C,
            External_Name => "GetMouseDelta";
      
      function get_mouse_wheel_move return float
         with
            Import => True,
            Convention => C,
            External_Name => "GetMouseWheelMove";

      procedure set_mouse_cursor(cursor: Mouse_Cursor)
         with 
            Import => True,
            Convention => C,
            External_Name => "SetMouseCursor";
   end input;

   package rcamera is
      --// Update camera position for selected mode
      procedure update (camera : access Camera3D; mode : CameraMode)
         with
            Import => True,
            Convention => C,
            External_Name => "UpdateCamera";
      
      --// Update camera movement/rotation
      procedure update_pro (camera : access Camera3D; movement, rotation : Vector3; zoom : Float)
         with
            Import => True,
            Convention => C,
            External_Name => "UpdateCameraPro";

      -- From rcamera.h
      --// Camera movement
      procedure Camera_Move_Forward (camera : access Camera3D; distance : Float; moveInWorldPlane : bool)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraMoveForward";

      procedure Camera_Move_Up (camera : access Camera3D; distance : Float)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraMoveUp";

      procedure Camera_Move_Right (camera : access Camera3D; distance : Float; moveInWorldPlane : bool)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraMoveRight";

      procedure Camera_Move_To_Target (camera : access Camera3D; steps : Float)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraMoveToTarget";

      --// Camera rotation
      procedure Camera_Yaw (camera : access Camera3D; angle : Float; rotateAroundTarget : bool)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraYaw";
      
      procedure Camera_Pitch (camera : access Camera3D; angle : Float; lockView, rotateAroundTarget, rotateUp : bool)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraPitch";
      
      procedure Camera_Roll (camera : access Camera3D; angle : Float)
         with
            Import => True,
            Convention => C,
            External_Name => "CameraRoll";

   end rcamera;

   package shapes is
      --// Draw a line
      procedure draw_line (
            start_posX, start_posY, end_posX, end_posy : int;
            c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLine";

      --// Draw a line (using gl lines)
      procedure draw_line_v (start_pos, end_pos : Vector2; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLineV";

      --// Draw a line (using triangles/quads)
      procedure draw_line_ex (
         start_pos, end_pos : Vector2;
         thick : C_Float;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLineEx";
                                 
      --// Draw lines sequence (using gl lines)
      procedure draw_line_strip (
         points : access Vector2;
         point_count : int;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLineStrip";
      
      --// Draw line segment cubic-bezier in-out interpolation
      procedure draw_line_bezier (
         start_pos, end_pos : Vector2;
         thick : Float;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLineBezier";

      --  Draw a color-filled circle
      procedure draw_circle (centerX, centerY : int; radius : Float; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawCircle";

      --// Draw a color-filled rectangle
      procedure draw_rectangle (posX, posY, width, height : int; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangle";

      --// Draw a color-filled rectangle (Vector version)
      procedure draw_rectangle_v (position, size : Vector2; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleV";

      --// Draw a color-filled rectangle
      procedure draw_rectangle_rec (bounds : Rectangle; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleRec";

      --// Draw a color-filled rectangle with pro parameters
      procedure draw_rectangle_pro (rec : Rectangle; origin : Vector2; rotation : Float; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectanglePro";
      
      --// Draw a vertical-gradient-filled rectangle
      procedure draw_rectangle_gradient_v (posX, posY, width, height : int; color1, color2 : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleGradientV";
   
      --// Draw a horizontal-gradient-filled rectangle
      procedure draw_rectangle_gradient_h (posX, posY, width, height : int; color1, color2 : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleGradientH";

      --// Draw a gradient-filled rectangle with custom vertex colors
      procedure draw_rectangle_gradient_ex (rec : Rectangle; col1, col2, col3, col4 : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleGradientEx";
      
      --// Draw rectangle outline
      procedure draw_rectangle_lines (posX, posY, width, height : int; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleLines";                                               

      --// Draw rectangle outline with extended parameters
      procedure draw_rectangle_lines_ex (
            rec : Rectangle;
            line_thick : int;
            c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleLinesEx";

      --// Draw a color-filled triangle (vertex in counter-clockwise order!)
      procedure draw_triangle (v1, v2, v3 : Vector2; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangle";
                    
      --// Draw triangle outline (vertex in counter-clockwise order!)
      procedure draw_triangle_lines (v1, v2, v3 : Vector2; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangleLines";
                               
      --// Draw a triangle fan defined by points (first vertex is the center)
      procedure draw_triangle_fan (points : access Vector2; point_count : int; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangleFan";
                                   
      --// Draw a triangle strip defined by points
      procedure draw_triangle_strip (points : access Vector2; point_count : int; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTriangleStrip";

      --  [[ Basic shapes collision detection functions ]] --

      --// Check collision between two rectangles                                      
      function check_collision_recs (rec1, rec2 : Rectangle)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionRecs";
      
      --// Check collision between two circles
      function check_collision_circles (
            center1 : Vector2;
            radius1 : Float;
            center2 : Vector2;
            radius2 : Float)
            return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionCircles";

      --// Check collision between circle and rectangle
      function check_collision_circle_rec (center : Vector2; radius : Float; rec : Rectangle)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionCircleRec";

      --// Check if point is inside rectangle
      function check_collision_point_rec (point : Vector2; rec : Rectangle)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionPointRec";
                  
      --// Check if point is inside circle
      function check_collision_point_circle (point, center : Vector2; radius : Float)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionPointCircle";

      --// Check if point is inside a triangle
      function check_collision_point_triangle (point, p1, p2, p3 : Vector2)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionPointTriangle";
                  
      --// Check if point is within a polygon described by array of vertices
      function check_collision_point_poly (point : Vector2; points : access Vector2; point_count : int)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionPointPoly";
 
      --// Check the collision between two lines defined by two points each, returns collision point by reference
      function check_collision_lines (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2; collision_point : access Vector2)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionLines";
               
      --// Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
      function check_collision_point_line (point, p1, p2 : Vector2; threshold : int)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionPointLine";
      
      --// Get collision rectangle for two rectangles collision  
      function Get_collision_rec (rec1, rec2 : Rectangle)
         return Rectangle
         with
            Import => True,
            Convention => C,
            External_Name => "GetCollisionRec";
   end shapes;

   package colors is
      function color_to_int (c : Color) return int
         with Import => True,
              Convention => C,
              External_Name => "ColorToInt";
      function get_color (hexvalue : unsigned) return Color
         with
            Import => True,
            Convention => C,
            External_Name => "GetColor";
      function fade (c : Color; alpha : Float) return Color
         with
            Import => True,
            Convention => C,
            External_Name => "Fade";
   end colors;

   ------------------------------------------------------------------------------------
   --  Texture Loading and Drawing Functions
   ------------------------------------------------------------------------------------
   package textures is
      --  [[ Image loading functions ]] --
      -- TODO: Add more functions from raylib.h

      --  [[ Image generation functions ]] --
      -- TODO: Add more functions from raylib.h

      --  [[ Image manipulation functions ]] --
      -- TODO: Add more functions from raylib.h

      --  [[ Image drawing functions ]] --
      -- NOTE: Image software-rendering functions (CPU)
      -- TODO: Add more functions from raylib.h

      --  [[ Texture loading functions ]] --

      --  Load texture from file into GPU memory (VRAM)
      function load (filename : String) return Texture2D;

      --// Load texture from image data
      function load_from_image (source_image : Image) return Texture2D
         with
            Import => True,
            Convention => C,
            External_Name => "LoadTextureFromImage";
      
      --  Unload texture from GPU memory (VRAM)
      procedure unload (texture : Texture2D)
         with
            Import => True,
            Convention => C,
            External_Name => "UnloadTexture";

      --  [[ Texture2D drawing functions ]]  --
      --// Draw a Texture2D
      procedure draw_texture (
            texture : Texture2D;
            posX, posY : int;
            tint : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTexture";
                                                                
      --// Draw a Texture2D with position defined as Vector2
      procedure draw_texture_v (
            texture : Texture2D;
            position : Vector2;
            tint : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTextureV"; 

      --// Draw a Texture2D with extended parameters
      procedure draw_texture_ex (
            texture : Texture2D;
            position : Vector2;
            rotation, scale : Float;
            tint : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTextureEx";

      procedure draw_texture_rec (
         texture : Texture2D;
         sourceRec : Rectangle;
         position : Vector2;
         tint : Color)
      with
         Import => True,
         Convention => C,
         External_Name => "DrawTextureRec";

      procedure draw_texture_pro (
         texture : Texture2D;
         sourceRec : Rectangle;
         destRec : Rectangle;
         origin : Vector2;
         rotation : Float;
         tint : Color)
      with
         Import => True,
         Convention => C,
         External_Name => "DrawTexturePro";
   end textures;

  ------------------------------------------------------------------------------------
  -- Font Loading and Text Drawing
  ------------------------------------------------------------------------------------
   package text is
      type char_list is array (Integer range <>) of int;

      --  Font loading/unloading functions
      function get_font_default return Font;
      pragma Import (C, get_font_default, "GetFontDefault");
      
      -- Load font from file into GPU memory (VRAM)
      function load_font (file : String) return Font;

      -- Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set
      function load_font_ex (file : String; size : int; chars : access int; glyphCount : int) return Font;
      function load_font_ex (file : String; size : int; chars : char_list; glyphCount : int) return Font;

      procedure unload_font (f : Font) with Import, Convention => C, External_Name => "UnloadFont";

      --  Text drawing functions

      --// Draw current FPS 
      procedure draw_FPS (x, y : int);
      pragma Import (C, draw_FPS, "DrawFPS");

      --// Draw text (using default font)
      procedure draw (text : String; posX, posY, fontSize : int; c : Color);

      --//  Draw text using font and additional parameters
      procedure draw_ex (
         F : Font;
         text : String;
         position : Vector2;
         fontSize, spacing : Float;
         tint : Color);

      --// Draw text using Font and pro parameters (rotation)
      procedure draw_pro (
            F : Font;
            text : String;
            position, origin : Vector2;
            rotation, fontSize, spacing : Float;
            tint : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawTextPro";

      --  Text font info functions
                                            
      --// Set vertical line spacing when drawing with line-breaks
      procedure set_text_line_spacing (spacing : int)
         with
            Import => True,
            Convention => C,
            External_Name => "SetTextLineSpacing";
      
      --  Measure string width for default font
      function measure (text : String; fontSize : int) return int;

      --  Measure string size for Font
      function measure_ex (f : Font; text : String; fontSize, spacing : Float)
         return Vector2;
      --  Get index position for a unicode character on font
      --  RLAPI int GetGlyphIndex(Font font, int character);
      --  Returns next codepoint in a UTF8 encoded string
      --  RLAPI int GetNextCodepoint(const char *text, int *count);
      --  NOTE: 0x3f(`?`) is returned on failure,
      --  `count` will hold the total number of bytes processed
   end text;

   ------------------------------------------------------------------------------------
   --  Basic 3d Shapes
   ------------------------------------------------------------------------------------
   package shapes3D is
      procedure draw_plane (center : Vector3; size : Vector2; tint : Color)
         with
            Import,
            Convention => C,
            External_Name => "DrawPlane";

      procedure draw_cube_v (position, size : Vector3; tint : Color)
         with
            Import,
            Convention => C,
            External_Name => "DrawCubeV";

      procedure draw_cube_wires (
         position : Vector3;
         width, height, length : Float;
         tint : Color)
      with
         Import => True,
         Convention => C,
         External_Name => "DrawCubeWires";
   end shapes3D;

   ------------------------------------------------------------------------------------
   -- Audio Loading and Playing Functions (Module: audio)
   ------------------------------------------------------------------------------------
   package Audio is
      --typedef void (*AudioCallback)(void *bufferData, unsigned int frames);

      -- Audio device management functions
      procedure init_audio_device;  -- Initialize audio device and context
      procedure close_audio_device; -- Close the audio device and context
      function  is_audio_device_ready return bool; -- Check if audio device has been initialized successfully
      procedure set_master_volume (volume : C_Float); -- Set master volume (listener)
      function  get_master_volume return C_Float; -- Get master volume
      ---
      pragma Import (C, init_audio_device,     "InitAudioDevice");
      pragma Import (C, close_audio_device,    "CloseAudioDevice");
      pragma Import (C, is_audio_device_ready, "IsAudioDeviceReady");
      pragma Import (C, set_master_volume,     "SetMasterVolume");
      pragma Import (C, get_master_volume,     "GetMasterVolume");

      -- Wave/Sound loading/unloading functions
      function load_sound    (file : Interfaces.C.strings.chars_ptr) return Sound;
      procedure unload_sound (s : Sound);
      function  is_sound_ready (s : Sound) return bool; --// Checks if a sound is ready
                            
      pragma Import (C, load_sound,     "LoadSound"); --// Play a sound
      pragma Import (C, unload_sound,   "UnloadSound");
      pragma Import (C, is_sound_ready, "IsSoundReady");

      -- Wave/Sound management functions
      procedure play_sound (s : Sound);

      pragma Import (C, play_sound, "PlaySound");
   end Audio;

end raylib;
