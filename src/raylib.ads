with Interfaces.C;
with Interfaces.C.Strings;
with System;
---
--  for reference use :
--  https://www.adaic.org/resources/add_content/standards/05rm/html/RM-B-3.html
--  https://raw.githubusercontent.com/raysan5/raylib/refs/tags/5.5/src/raylib.h

package Raylib is

    -- Internal C types for C interoperability (used in records with C convention)
    -- Users should generally not need to use these types directly
    subtype int is Interfaces.C.int;
    subtype c_float is Interfaces.C.C_float;
    subtype double is Interfaces.C.double;
    subtype unsigned is Interfaces.C.unsigned;
    subtype unsigned_char is Interfaces.C.unsigned_char;

    RAYLIB_VERSION_MAJOR : constant := 5;
    RAYLIB_VERSION_MINOR : constant := 5;
    RAYLIB_VERSION_PATCH : constant := 0;
    RAYLIB_VERSION       : aliased constant String := "5.5";

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
        m0, m4, m8, m12  : Float;
        m1, m5, m9, m13  : Float;
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
        sourceRec                : Rectangle;
        left, top, right, bottom : int;
        layout                   : int;
    end record
    with Convention => C_Pass_By_Copy;

    FLAG_VSYNC_HINT               : constant :=
       16#00000040#;  -- Set to try enabling V-Sync on GPU
    FLAG_FULLSCREEN_MODE          : constant :=
       16#00000002#;  -- Set to run program in fullscreen
    FLAG_WINDOW_RESIZABLE         : constant :=
       16#00000004#;  -- Set to allow resizable window
    FLAG_WINDOW_UNDECORATED       : constant :=
       16#00000008#;  -- Set to disable window decoration (frame and buttons)
    FLAG_WINDOW_HIDDEN            : constant :=
       16#00000080#;  -- Set to hide window
    FLAG_WINDOW_MINIMIZED         : constant :=
       16#00000200#;  -- Set to minimize window (iconify)
    FLAG_WINDOW_MAXIMIZED         : constant :=
       16#00000400#;  -- Set to maximize window (expanded to monitor)
    FLAG_WINDOW_UNFOCUSED         : constant :=
       16#00000800#;  -- Set to window non focused
    FLAG_WINDOW_TOPMOST           : constant :=
       16#00001000#;  -- Set to window always on top
    FLAG_WINDOW_ALWAYS_RUN        : constant :=
       16#00000100#;  -- Set to allow windows running while minimized
    FLAG_WINDOW_TRANSPARENT       : constant :=
       16#00000010#;  -- Set to allow transparent framebuffer
    FLAG_WINDOW_HIGHDPI           : constant :=
       16#00002000#;  -- Set to support HighDPI
    FLAG_WINDOW_MOUSE_PASSTHROUGH : constant :=
       16#00004000#;  -- Set to support mouse passthrough#, only supported when FLAG_WINDOW_UNDECORATED
    FLAG_BORDERLESS_WINDOWED_MODE : constant :=
       16#00008000#;  -- Set to run program in borderless windowed mode
    FLAG_MSAA_4X_HINT             : constant :=
       16#00000020#;  -- Set to try enabling MSAA 4X
    FLAG_INTERLACED_HINT          : constant :=
       16#00010000#;  -- Set to try enabling interlaced video format (for V3D)

    --  Keyboard Function Keys
    type Keys is
       (KEY_NULL,
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

    for Keys use
       (KEY_NULL          => 0,
        KEY_SPACE         => 32,

        KEY_APOSTROPHE    => 39,
        KEY_COMMA         => 44,
        KEY_MINUS         => 45,
        KEY_PERIOD        => 46,
        KEY_SLASH         => 47,
        KEY_ZERO          => 48,
        KEY_ONE           => 49,
        KEY_TWO           => 50,
        KEY_THREE         => 51,
        KEY_FOUR          => 52,
        KEY_FIVE          => 53,
        KEY_SIX           => 54,
        KEY_SEVEN         => 55,
        KEY_EIGHT         => 56,
        KEY_NINE          => 57,
        KEY_SEMICOLON     => 59,
        KEY_EQUAL         => 61,
        KEY_A             => 65,
        KEY_B             => 66,
        KEY_C             => 67,
        KEY_D             => 68,
        KEY_E             => 69,
        KEY_F             => 70,
        KEY_G             => 71,
        KEY_H             => 72,
        KEY_I             => 73,
        KEY_J             => 74,
        KEY_K             => 75,
        KEY_L             => 76,
        KEY_M             => 77,
        KEY_N             => 78,
        KEY_O             => 79,
        KEY_P             => 80,
        KEY_Q             => 81,
        KEY_R             => 82,
        KEY_S             => 83,
        KEY_T             => 84,
        KEY_U             => 85,
        KEY_V             => 86,
        KEY_W             => 87,
        KEY_X             => 88,
        KEY_Y             => 89,
        KEY_Z             => 90,

        KEY_LEFT_BRACKET  => 91,
        KEY_BACKSLASH     => 92,
        KEY_RIGHT_BRACKET => 93,
        KEY_GRAVE         => 96,

        KEY_ESCAPE        => 256,
        KEY_ENTER         => 257,
        KEY_TAB           => 258,
        KEY_BACKSPACE     => 259,
        KEY_INSERT        => 260,
        KEY_DELETE        => 261,
        KEY_RIGHT         => 262,
        KEY_LEFT          => 263,
        KEY_DOWN          => 264,
        KEY_UP            => 265,
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

        KEY_KP_0          => 320,
        KEY_KP_1          => 321,
        KEY_KP_2          => 322,
        KEY_KP_3          => 323,
        KEY_KP_4          => 324,
        KEY_KP_5          => 325,
        KEY_KP_6          => 326,
        KEY_KP_7          => 327,
        KEY_KP_8          => 328,
        KEY_KP_9          => 329,
        KEY_KP_DECIMAL    => 330,
        KEY_KP_DIVIDE     => 331,
        KEY_KP_MULTIPLY   => 332,
        KEY_KP_SUBTRACT   => 333,
        KEY_KP_ADD        => 334,
        KEY_KP_ENTER      => 335,
        KEY_KP_EQUAL      => 336,

        KEY_LEFT_SHIFT    => 340,
        KEY_LEFT_CONTROL  => 341,
        KEY_LEFT_ALT      => 342,
        KEY_LEFT_SUPER    => 343,
        KEY_RIGHT_SHIFT   => 344,
        KEY_RIGHT_CONTROL => 345,
        KEY_RIGHT_ALT     => 346,
        KEY_RIGHT_SUPER   => 347,
        KEY_KB_MENU       => 348);

    type Mouse_Cursor is
       (MOUSE_CURSOR_DEFAULT,
        MOUSE_CURSOR_ARROW,
        MOUSE_CURSOR_IBEAM,
        MOUSE_CURSOR_POINTING_HAND,
        MOUSE_CURSOR_RESIZE_EW,
        MOUSE_CURSOR_RESIZE_NS,
        MOUSE_CURSOR_RESIZE_NWSE,
        MOUSE_CURSOR_RESIZE_NESW,
        MOUSE_CURSOR_RESIZE_ALL,
        MOUSE_CURSOR_NOT_ALLOWED)
    with Convention => C;

    type Mouse_Button is
       (MOUSE_BUTTON_LEFT,
        MOUSE_BUTTON_RIGHT,
        MOUSE_BUTTON_MIDDLE,
        MOUSE_BUTTON_SIDE,
        MOUSE_BUTTON_EXTRA,
        MOUSE_BUTTON_FORWARD,
        MOUSE_BUTTON_BACK)
    with Convention => C;

    type Gamepad_Button is
       (GAMEPAD_BUTTON_UNKNOWN,
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

    type Pixel_Format is
       (PIXELFORMAT_UNCOMPRESSED_GRAYSCALE,         -- 8 bit per pixel (no alpha)
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
    for Pixel_Format use
       (PIXELFORMAT_UNCOMPRESSED_GRAYSCALE    => 1,
        -- 8 bit per pixel (no alpha)
        PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA   => 2,
        -- 8*2 bpp (2 channels)
        PIXELFORMAT_UNCOMPRESSED_R5G6B5       => 3,
        -- 16 bpp
        PIXELFORMAT_UNCOMPRESSED_R8G8B8       => 4,
        -- 24 bpp
        PIXELFORMAT_UNCOMPRESSED_R5G5B5A1     => 5,
        -- 16 bpp (1 bit alpha)
        PIXELFORMAT_UNCOMPRESSED_R4G4B4A4     => 6,
        -- 16 bpp (4 bit alpha)
        PIXELFORMAT_UNCOMPRESSED_R8G8B8A8     => 7,
        -- 32 bpp
        PIXELFORMAT_UNCOMPRESSED_R32          => 8,
        -- 32 bpp (1 channel - float)
        PIXELFORMAT_UNCOMPRESSED_R32G32B32    => 9,
        -- 32*3 bpp (3 channels - float)
        PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 => 10,
        -- 32*4 bpp (4 channels - float)
        PIXELFORMAT_UNCOMPRESSED_R16          => 11,
        -- 16 bpp (1 channel - half float)
        PIXELFORMAT_UNCOMPRESSED_R16G16B16    => 12,
        -- 16*3 bpp (3 channels - half float)
        PIXELFORMAT_UNCOMPRESSED_R16G16B16A16 => 13,
        -- 16*4 bpp (4 channels - half float)
        PIXELFORMAT_COMPRESSED_DXT1_RGB       => 14,
        -- 4 bpp (no alpha)
        PIXELFORMAT_COMPRESSED_DXT1_RGBA      => 15,
        -- 4 bpp (1 bit alpha)
        PIXELFORMAT_COMPRESSED_DXT3_RGBA      => 16,
        -- 8 bpp
        PIXELFORMAT_COMPRESSED_DXT5_RGBA      => 17,
        -- 8 bpp
        PIXELFORMAT_COMPRESSED_ETC1_RGB       => 18,
        -- 4 bpp
        PIXELFORMAT_COMPRESSED_ETC2_RGB       => 19,
        -- 4 bpp
        PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA  => 20,
        -- 8 bpp
        PIXELFORMAT_COMPRESSED_PVRT_RGB       => 21,
        -- 4 bpp
        PIXELFORMAT_COMPRESSED_PVRT_RGBA      => 22,
        -- 4 bpp
        PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA  => 23,
        -- 8 bpp
        PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA  => 24);   -- 2 bpp

    type NPatch_Layout is
       (NPATCH_NINE_PATCH,
        NPATCH_THREE_PATCH_VERTICAL,
        NPATCH_THREE_PATCH_HORIZONTAL)
    with Convention => C;

    type Image is record
        data                   : System.Address;
        width, height, mipmaps : int;
        format                 : int;
        --  format : Pixel_Format;
    end record
    with Convention => C_Pass_By_Copy;

    type Texture2D is record
        id            : unsigned;
        width, height : int;
        mimaps        : int;
        format        : int;
        --format : Pixel_Format;
    end record
    with Convention => C_Pass_By_Copy;

    subtype Texture is Texture2D;
    subtype TextureCubemap is Texture2D;

    type RenderTexture2D is record
        id             : unsigned;
        texture, depth : Texture2D;
    end record
    with Convention => C_Pass_By_Copy;
    subtype RenderTexture is RenderTexture2D;

    --  Font character info
    type GlyphInfo is record
        value    : int;              -- Character value (Unicode)
        offsetX  : int;            -- Character offset X when drawing
        offsetY  : int;            -- Character offset Y when drawing
        advanceX : int;           -- Character advance position X
        img      :
           Image;              -- Character image data, named image in raylib.h but conflict with the type
    end record
    with Convention => C_Pass_By_Copy;

    type Font is record
        baseSize     : int;         -- Base size (default chars height)
        glyphCount   : int;       -- Number of glyph characters
        glyphPadding : int;     -- Padding around the glyph characters
        texture      : Texture2D;   -- Texture atlas containing the glyphs
        recs         :
           access Rectangle;  -- Rectangles in texture for the glyphs
        glyphs       : access GlyphInfo; --  Glyphs info data
    end record
    with Convention => C_Pass_By_Copy;

    --  Camera system modes
    type CameraMode is
       (CAMERA_CUSTOM,
        CAMERA_FREE,
        CAMERA_ORBITAL,
        CAMERA_FIRST_PERSON,
        CAMERA_THIRD_PERSON)
    with Convention => C;

    --  Camera projection modes
    type CameraType is (CAMERA_PERSPECTIVE, CAMERA_ORTHOGRAPHIC)
    with Convention => C;

    --  Camera type, defines a camera position/orientation in 3d space
    type Camera3D is record
        position   : Vector3;  -- Camera position
        target     : Vector3;  -- Camera target it looks-at
        up         : Vector3;      -- Camera up vector (rotation over its axis)
        --  Camera field-of-view apperture in Y (degrees) in perspective,
        --  used as near plane width in orthographic
        fovy       : Float;
        projection :
           CameraType;  -- Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
    end record
    with Convention => C_Pass_By_Copy;
    subtype Camera is Camera3D;

    type P_Camera is access all Camera3D;

    type Camera2D is record
        offset   : Vector2;    -- Camera offset (displacement from target)
        target   : Vector2;    -- Camera target (rotation and zoom origin)
        rotation : Float;    -- Camera rotation in degrees
        zoom     :
           Float;        -- Camera zoom (scaling), should be 1.0f by default
    end record;

    --  Ray, ray for raycasting
    type Ray is record
        position  : Vector3;
        direction : Vector3;
    end record
    with Convention => C_Pass_By_Copy;

    --  RayCollision, ray hit information
    type RayCollision is record
        hit      : Boolean;
        distance : Float;
        point    : Vector3;
        normal   : Vector3;
    end record
    with Convention => C_Pass_By_Copy;

    type BoundingBox is record
        min : Vector3;            -- Minimum vertex box-corner
        max : Vector3;            -- Maximum vertex box-corner
    end record
    with Convention => C_Pass_By_Copy;

    --// Wave, audio wave data
    type Wave is record
        -- Total number of frames (considering channels)
        frameCount : unsigned;
        --  Frequency (samples per second)
        sampleRate : unsigned;
        --  Bit depth (bits per sample): 8, 16, 32 (24 not supported)
        sampleSize : unsigned;
        --  Number of channels (1-mono, 2-stereo, ...)
        channels   : unsigned;
        -- Buffer data pointer
        data       : System.Address;
    end record
    with Convention => C_Pass_By_Copy;

    type AudioStream is record
        buffer                           : System.Address;
        processor                        : System.Address;
        sampleRate, sampleSize, channels : unsigned;
    end record
    with Convention => C_Pass_By_Copy;

    type Sound is record
        stream     : AudioStream;
        frameCount : unsigned;
    end record
    with Convention => C_Pass_By_Copy;

    type Music is record
        -- Audio stream
        stream     : AudioStream;
        -- Total number of frames (considering channels)
        frameCount : unsigned;
        -- Music looping enable
        looping    : Boolean;
        -- Type of music context (audio filetype)
        ctxType    : int;
        -- Audio context data, depends on type
        ctxData    : System.Address;
    end record
    with Convention => C_Pass_By_Copy;

    type Log is
       (LOG_ALL,
        LOG_TRACE,
        LOG_DEBUG,
        LOG_INFO,
        LOG_WARNING,
        LOG_ERROR,
        LOG_FATAL,
        LOG_NONE);
    for Log'Size use int'Size;

    LIGHTGRAY  : constant Color := (200, 200, 200, 255);
    GRAY       : constant Color := (130, 130, 130, 255);
    DARKGRAY   : constant Color := (80, 80, 80, 255);
    YELLOW     : constant Color := (253, 249, 0, 255);
    GOLD       : constant Color := (255, 203, 0, 255);
    ORANGE     : constant Color := (255, 161, 0, 255);
    PINK       : constant Color := (255, 109, 194, 255);
    RED        : constant Color := (230, 41, 55, 255);
    MAROON     : constant Color := (190, 33, 55, 255);
    GREEN      : constant Color := (0, 228, 48, 255);
    LIME       : constant Color := (0, 158, 47, 255);
    DARKGREEN  : constant Color := (0, 117, 44, 255);
    SKYBLUE    : constant Color := (102, 191, 255, 255);
    BLUE       : constant Color := (0, 121, 241, 255);
    DARKBLUE   : constant Color := (0, 82, 172, 255);
    PURPLE     : constant Color := (200, 122, 255, 255);
    VIOLET     : constant Color := (135, 60, 190, 255);
    DARKPURPLE : constant Color := (112, 31, 126, 255);
    BEIGE      : constant Color := (211, 176, 131, 255);
    BROWN      : constant Color := (127, 106, 79, 255);
    DARKBROWN  : constant Color := (76, 63, 47, 255);

    WHITE    : constant Color := (255, 255, 255, 255);
    BLACK    : constant Color := (0, 0, 0, 255);
    BLANK    : constant Color := (0, 0, 0, 0);
    MAGENTA  : constant Color := (255, 0, 255, 255);
    RAYWHITE : constant Color := (245, 245, 245, 255);

    package Utils is
        --  // NOTE: Following functions implemented in module [utils]
        --  //------------------------------------------------------------------

        --// Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
        procedure Trace_Log (ltype : Log; text : String);

        --// Set the current threshold (minimum) log level
        procedure Set_Trace_Log_Level (ltype : Log)
        with
           Import        => True,
           Convention    => C,
           External_Name => "SetTraceLogLevel";
    end Utils;

    ------------------------------------------------------------------------------------
    --  Input Handling Functions (Module: core)
    ------------------------------------------------------------------------------------
    package Input is
        --  [[ Input-related functions: keyboard ]]  --

        --// Check if a key has been pressed once
        function Is_Key_Pressed (key : Keys) return Boolean;

        --// Check if a key has been pressed again (Only PLATFORM_DESKTOP)
        function Is_Key_Pressed_Repeat (key : Keys) return Boolean;

        --// Check if a key is being pressed
        function Is_Key_Down (key : Keys) return Boolean;

        --// Check if a key has been released once
        function Is_Key_Released (key : Keys) return Boolean;

        --// Check if a key is NOT being pressed
        function Is_Key_Up (key : Keys) return Boolean;

        --// Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
        function Get_Key_Pressed return Keys
        with Import, Convention => C, External_Name => "GetKeyPressed";

        --// Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
        function Get_Char_Pressed return int
        with Import, Convention => C, External_Name => "GetCharPressed";

        --// Set a custom key to exit program (default is ESC)
        procedure Set_Exit_Key (key : Keys)
        with Import, Convention => C, External_Name => "SetExitKey";

        --  [[ Input-related functions: gamepads  ]]  --
        function Is_Gamepad_Available (gamepad : int) return Boolean;

        function Get_Gamepad_Name (gamepad : int) return String;

        --  Detect if a gamepad button has been pressed once
        function Is_Gamepad_Button_Pressed
           (gamepad : int; button : int) return Boolean;

        --// Check if a gamepad button is being pressed
        function Is_Gamepad_Button_Down
           (gamepad : int; button : Gamepad_Button) return Boolean;

        --// Check if a gamepad button has been released once
        function Is_Gamepad_Button_Released
           (gamepad : int; button : Gamepad_Button) return Boolean;

        --// Check if a gamepad button is NOT being pressed
        function Is_Gamepad_Button_Up
           (gamepad : int; button : Gamepad_Button) return Boolean;

        --  Get the last gamepad button pressed
        function Get_Gamepad_Button_Pressed return Gamepad_Button
        with
           Import,
           Convention    => C,
           External_Name => "GetGamepadButtonPressed";

        --  Return gamepad axis count for a gamepad
        function Get_Gamepad_Axis_Count (gamepad : int) return int
        with Import, Convention => C, External_Name => "GetGamepadAxisCount";

        --  Return axis movement value for a gamepad axis
        function Get_Gamepad_Axis_Movement
           (gamepad : int; axis : Gamepad_Axis) return Float
        with
           Import,
           Convention    => C,
           External_Name => "GetGamepadAxisMovement";
        --RLAPI int SetGamepadMappings(const char *mappings);
        --// Set internal gamepad mappings (SDL_GameControllerDB)

        --  [[ Input-related functions: mouse  ]] --

        --// Check if a mouse button has been pressed once
        function Is_Mouse_Button_Pressed
           (button : Mouse_Button) return Boolean;

        --// Check if a mouse button is being pressed
        function Is_Mouse_Button_Down (button : Mouse_Button) return Boolean;

        --// Check if a mouse button has been released once
        function Is_Mouse_Button_Released
           (button : Mouse_Button) return Boolean;

        --// Check if a mouse button is NOT being pressed
        function Is_Mouse_Button_Up (button : Mouse_Button) return Boolean;

        function Get_Mouse_Position return Vector2
        with Import, Convention => C, External_Name => "GetMousePosition";

        function Get_Mouse_Delta return Vector2
        with Import, Convention => C, External_Name => "GetMouseDelta";

        function Get_Mouse_Wheel_Move return Float
        with Import, Convention => C, External_Name => "GetMouseWheelMove";

        procedure Set_Mouse_Cursor (cursor : Mouse_Cursor)
        with Import, Convention => C, External_Name => "SetMouseCursor";
    end Input;

    package RCamera is
        --// Update camera position for selected mode
        procedure Update (Camera : access Camera3D; mode : CameraMode)
        with Import, Convention => C, External_Name => "UpdateCamera";

        --// Update camera movement/rotation
        procedure Update_Pro
           (camera             : access Camera3D;
            movement, rotation : Vector3;
            zoom               : Float)
        with
           Import        => True,
           Convention    => C,
           External_Name => "UpdateCameraPro";

        -- From rcamera.h
        --// Camera movement
        procedure Camera_Move_Forward
           (camera           : access Camera3D;
            distance         : Float;
            moveInWorldPlane : Boolean);

        procedure Camera_Move_Up (camera : access Camera3D; distance : Float)
        with Import => True, Convention => C, External_Name => "CameraMoveUp";

        procedure Camera_Move_Right
           (camera           : access Camera3D;
            distance         : Float;
            moveInWorldPlane : Boolean);

        procedure Camera_Move_To_Target
           (camera : access Camera3D; steps : Float)
        with
           Import        => True,
           Convention    => C,
           External_Name => "CameraMoveToTarget";

        --// Camera rotation
        procedure Camera_Yaw
           (camera             : access Camera3D;
            angle              : Float;
            rotateAroundTarget : Boolean);

        procedure Camera_Pitch
           (camera                                 : access Camera3D;
            angle                                  : Float;
            lockView, rotateAroundTarget, rotateUp : Boolean);

        procedure Camera_Roll (camera : access Camera3D; angle : Float)
        with Import => True, Convention => C, External_Name => "CameraRoll";

    end RCamera;

    package Shapes is
        --// Draw a line
        procedure Draw_Line
           (start_posX, start_posY, end_posX, end_posy : int; c : Color)
        with Import => True, Convention => C, External_Name => "DrawLine";

        --// Draw a line (using gl lines)
        procedure Draw_Line_V (start_pos, end_pos : Vector2; c : Color)
        with Import => True, Convention => C, External_Name => "DrawLineV";

        --// Draw a line (using triangles/quads)
        procedure Draw_Line_Ex
           (start_pos, end_pos : Vector2; thick : C_Float; c : Color)
        with Import => True, Convention => C, External_Name => "DrawLineEx";

        --// Draw lines sequence (using gl lines)
        procedure Draw_Line_Strip
           (points : access Vector2; point_count : int; c : Color)
        with Import => True, Convention => C, External_Name => "DrawLineStrip";

        --// Draw line segment cubic-bezier in-out interpolation
        procedure Draw_Line_Bezier
           (start_pos, end_pos : Vector2; thick : Float; c : Color)
        with Import, Convention => C, External_Name => "DrawLineBezier";

        --  Draw a color-filled circle
        procedure Draw_Circle
           (centerX, centerY : int; radius : Float; c : Color)
        with Import => True, Convention => C, External_Name => "DrawCircle";

        --// Draw a color-filled rectangle
        procedure Draw_Rectangle (posX, posY, width, height : int; c : Color)
        with Import, Convention => C, External_Name => "DrawRectangle";

        --// Draw a color-filled rectangle (Vector version)
        procedure Draw_Rectangle_V (position, size : Vector2; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleV";

        --// Draw a color-filled rectangle
        procedure Draw_Rectangle_Rec (bounds : Rectangle; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleRec";

        --// Draw a color-filled rectangle with pro parameters
        procedure Draw_Rectangle_Pro
           (rec : Rectangle; origin : Vector2; rotation : Float; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectanglePro";

        procedure Draw_Rectangle_Rounded
           (rec : Rectangle; roundness : Float; segments : int; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleRounded";

        --// Draw a vertical-gradient-filled rectangle
        procedure Draw_Rectangle_Gradient_V
           (posX, posY, width, height : int; color1, color2 : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleGradientV";

        --// Draw a horizontal-gradient-filled rectangle
        procedure Draw_Rectangle_Gradient_H
           (posX, posY, width, height : int; color1, color2 : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleGradientH";

        --// Draw a gradient-filled rectangle with custom vertex colors
        procedure Draw_Rectangle_Gradient_Ex
           (rec : Rectangle; col1, col2, col3, col4 : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleGradientEx";

        --// Draw rectangle outline
        procedure Draw_Rectangle_Lines
           (posX, posY, width, height : int; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleLines";

        --// Draw rectangle outline with extended parameters
        procedure Draw_Rectangle_Lines_Ex
           (rec : Rectangle; line_thick : Float; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawRectangleLinesEx";

        --// Draw a color-filled triangle (vertex in counter-clockwise order!)
        procedure Draw_Triangle (v1, v2, v3 : Vector2; c : Color)
        with Import => True, Convention => C, External_Name => "DrawTriangle";

        --// Draw triangle outline (vertex in counter-clockwise order!)
        procedure Draw_Triangle_Lines (v1, v2, v3 : Vector2; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawTriangleLines";

        --// Draw a triangle fan defined by points (first vertex is the center)
        procedure Draw_Triangle_Fan
           (points : access Vector2; point_count : int; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawTriangleFan";

        --// Draw a triangle strip defined by points
        procedure Draw_Triangle_Strip
           (points : access Vector2; point_count : int; c : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawTriangleStrip";

        --  [[ Basic shapes collision detection functions ]] --

        --// Check collision between two rectangles
        function Check_Collision_Recs (rec1, rec2 : Rectangle) return Boolean;

        --// Check collision between two circles
        function Check_Collision_Circles
           (center1 : Vector2;
            radius1 : Float;
            center2 : Vector2;
            radius2 : Float) return Boolean;

        --// Check collision between circle and rectangle
        function Check_Collision_Circle_Rec
           (center : Vector2; radius : Float; rec : Rectangle) return Boolean;

        --// Check if point is inside rectangle
        function Check_Collision_Point_Rec
           (point : Vector2; rec : Rectangle) return Boolean;

        --// Check if point is inside circle
        function Check_Collision_Point_Circle
           (point, center : Vector2; radius : Float) return Boolean;

        --// Check if point is inside a triangle
        function Check_Collision_Point_Triangle
           (point, p1, p2, p3 : Vector2) return Boolean;

        --// Check if point is within a polygon described by array of vertices
        function Check_Collision_Point_Poly
           (point : Vector2; points : access Vector2; point_count : int)
            return Boolean;

        --// Check the collision between two lines defined by two points each, returns collision point by reference
        function Check_Collision_Lines
           (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
            collision_point                            : access Vector2)
            return Boolean;

        --// Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
        function Check_Collision_Point_Line
           (point, p1, p2 : Vector2; threshold : int) return Boolean;

        --// Get collision rectangle for two rectangles collision
        function Get_Collision_Rec (rec1, rec2 : Rectangle) return Rectangle
        with
           Import        => True,
           Convention    => C,
           External_Name => "GetCollisionRec";
    end Shapes;

    package Colors is
        function Color_To_Int (c : Color) return int
        with Import => True, Convention => C, External_Name => "ColorToInt";
        function Get_Color (hexvalue : unsigned) return Color
        with Import => True, Convention => C, External_Name => "GetColor";
        function Fade (c : Color; alpha : Float) return Color
        with Import => True, Convention => C, External_Name => "Fade";
    end Colors;

    ------------------------------------------------------------------------------------
    --  Texture Loading and Drawing Functions
    ------------------------------------------------------------------------------------
    package Textures is
        --  [[ Image loading functions ]] --
        function Load_Image (filename : String) return Image;
        procedure Unload_Image (img : Image)
        with Import => True, Convention => C, External_Name => "UnloadImage";

        --  [[ Image generation functions ]] --
        -- TODO: Add more functions from raylib.h

        --  [[ Image manipulation functions ]] --
        -- TODO: Add more functions from raylib.h

        --  [[ Image drawing functions ]] --
        -- NOTE: Image software-rendering functions (CPU)
        -- TODO: Add more functions from raylib.h

        --  [[ Texture loading functions ]] --

        --  Load texture from file into GPU memory (VRAM)
        function Load (filename : String) return Texture2D;

        --// Load texture from image data
        function Load_From_Image (source_image : Image) return Texture2D
        with
           Import        => True,
           Convention    => C,
           External_Name => "LoadTextureFromImage";

        --  Unload texture from GPU memory (VRAM)
        procedure Unload (texture : Texture2D)
        with Import => True, Convention => C, External_Name => "UnloadTexture";

        --  [[ Texture2D drawing functions ]]  --
        --// Draw a Texture2D
        procedure Draw_Texture
           (texture : Texture2D; posX, posY : int; tint : Color)
        with Import => True, Convention => C, External_Name => "DrawTexture";

        --// Draw a Texture2D with position defined as Vector2
        procedure Draw_Texture_V
           (texture : Texture2D; position : Vector2; tint : Color)
        with Import => True, Convention => C, External_Name => "DrawTextureV";

        --// Draw a Texture2D with extended parameters
        procedure Draw_Texture_Ex
           (texture         : Texture2D;
            position        : Vector2;
            rotation, scale : Float;
            tint            : Color)
        with Import => True, Convention => C, External_Name => "DrawTextureEx";

        procedure Draw_Texture_Rec
           (texture   : Texture2D;
            sourceRec : Rectangle;
            position  : Vector2;
            tint      : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawTextureRec";

        procedure Draw_Texture_Pro
           (texture   : Texture2D;
            sourceRec : Rectangle;
            destRec   : Rectangle;
            origin    : Vector2;
            rotation  : Float;
            tint      : Color)
        with
           Import        => True,
           Convention    => C,
           External_Name => "DrawTexturePro";
    end Textures;

    ------------------------------------------------------------------------------------
    -- Font Loading and Text Drawing
    ------------------------------------------------------------------------------------
    package Text is
        --  Font loading/unloading functions
        function Get_Font_Default return Font;
        pragma Import (C, Get_Font_Default, "GetFontDefault");

        -- Load font from file into GPU memory (VRAM)
        function Load_Font (file : String) return Font;

        -- Load font from file with extended parameters, use NULL for fontChars and 0 for glyphCount to load the default character set
        function Load_Font_Ex
           (file : String; size : int; chars : access int; glyphCount : int)
            return Font;

        procedure Unload_Font (f : Font)
        with Import, Convention => C, External_Name => "UnloadFont";

        --  Text drawing functions

        --// Draw current FPS
        procedure Draw_FPS (x, y : int)
        with Import, Convention => C, External_Name => "DrawFPS";

        --// Draw text (using default font)
        procedure Draw (text : String; posX, posY, fontSize : int; c : Color);

        --//  Draw text using font and additional parameters
        procedure Draw_Ex
           (F                 : Font;
            text              : String;
            position          : Vector2;
            fontSize, spacing : Float;
            tint              : Color);

        --// Draw text using Font and pro parameters (rotation)
        procedure Draw_Pro
           (F                           : Font;
            text                        : String;
            position, origin            : Vector2;
            rotation, fontSize, spacing : Float;
            tint                        : Color)
        with Import, Convention => C, External_Name => "DrawTextPro";

        --  Text font info functions

        --// Set vertical line spacing when drawing with line-breaks
        procedure Set_Text_Line_Spacing (spacing : int)
        with Import, Convention => C, External_Name => "SetTextLineSpacing";

        --  Measure string width for default font
        function Measure (text : String; fontSize : int) return int;

        --  Measure string size for Font
        function Measure_Ex
           (f : Font; text : String; fontSize, spacing : Float) return Vector2;
        --  Get index position for a unicode character on font
        --  RLAPI int GetGlyphIndex(Font font, int character);
        --  Returns next codepoint in a UTF8 encoded string
        --  RLAPI int GetNextCodepoint(const char *text, int *count);
        --  NOTE: 0x3f(`?`) is returned on failure,
        --  `count` will hold the total number of bytes processed
    end Text;

    ------------------------------------------------------------------------------------
    --  Basic 3d Shapes
    ------------------------------------------------------------------------------------
    package Shapes_3D is
        -- Draw a line in 3D world space
        procedure Draw_Line_3D (Start_Pos, End_Pos : Vector3; C : Color)
        with Import, Convention => C, External_Name => "DrawLine3D";

        procedure Draw_Plane (Center : Vector3; Size : Vector2; Tint : Color)
        with Import, Convention => C, External_Name => "DrawPlane";

        procedure Draw_Cube
           (Position : Vector3; Width, Height, Length : Float; Tint : Color)
        with Import, Convention => C, External_Name => "DrawCube";

        procedure Draw_Cube_V (Position, Size : Vector3; Tint : Color)
        with Import, Convention => C, External_Name => "DrawCubeV";

        procedure Draw_Cube_Wires
           (Position : Vector3; Width, Height, Length : Float; Tint : Color)
        with Import, Convention => C, External_Name => "DrawCubeWires";

        function Get_Ray_Collision_Box
           (R : Ray; Box : BoundingBox) return RayCollision
        with Import, Convention => C, External_Name => "GetRayCollisionBox";
    end Shapes_3D;

end Raylib;
