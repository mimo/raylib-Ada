with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;
---
--  for reference use :
--  https://www.adaic.org/resources/add_content/standards/05rm/html/RM-B-3.html
--  https://raw.githubusercontent.com/raysan5/raylib/3.0.0/src/raylib.h

package raylib is

   subtype int           is Interfaces.C.int;
   subtype c_float       is Interfaces.C.C_float;
   subtype unsigned      is Interfaces.C.unsigned;
   subtype unsigned_char is Interfaces.C.unsigned_char;
   subtype bool          is Interfaces.C.Extensions.bool;
   subtype chars_ptr     is Interfaces.C.Strings.chars_ptr;

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
      layout : int; -- named type in Raylib
   end record
      with Convention => C_Pass_By_Copy;

   --  Keyboard Function Keys
   type Keys is (
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

   type Mouse_Button is (
       MOUSE_LEFT_BUTTON,
       MOUSE_RIGHT_BUTTON,
       MOUSE_MIDDLE_BUTTON)
   with Convention => C;

   type Gamepad_Number is (
      GAMEPAD_PLAYER1,
      GAMEPAD_PLAYER2,
      GAMEPAD_PLAYER3,
      GAMEPAD_PLAYER4)
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
     (GAMEPAD_AXIS_UNKNOWN,
      GAMEPAD_AXIS_LEFT_X,
      GAMEPAD_AXIS_LEFT_Y,
      GAMEPAD_AXIS_RIGHT_X,
      GAMEPAD_AXIS_RIGHT_Y,
      GAMEPAD_AXIS_LEFT_TRIGGER,
      GAMEPAD_AXIS_RIGHT_TRIGGER)
   with Convention => C;

   type Pixel_Format is (
      UNCOMPRESSED_GRAYSCALE,         -- 8 bit per pixel (no alpha)
      UNCOMPRESSED_GRAY_ALPHA,        -- 8*2 bpp (2 channels)
      UNCOMPRESSED_R5G6B5,            -- 16 bpp
      UNCOMPRESSED_R8G8B8,            -- 24 bpp
      UNCOMPRESSED_R5G5B5A1,          -- 16 bpp (1 bit alpha)
      UNCOMPRESSED_R4G4B4A4,          -- 16 bpp (4 bit alpha)
      UNCOMPRESSED_R8G8B8A8,          -- 32 bpp
      UNCOMPRESSED_R32,               -- 32 bpp (1 channel - float)
      UNCOMPRESSED_R32G32B32,         -- 32*3 bpp (3 channels - float)
      UNCOMPRESSED_R32G32B32A32,      -- 32*4 bpp (4 channels - float)
      COMPRESSED_DXT1_RGB,            -- 4 bpp (no alpha)
      COMPRESSED_DXT1_RGBA,           -- 4 bpp (1 bit alpha)
      COMPRESSED_DXT3_RGBA,           -- 8 bpp
      COMPRESSED_DXT5_RGBA,           -- 8 bpp
      COMPRESSED_ETC1_RGB,            -- 4 bpp
      COMPRESSED_ETC2_RGB,            -- 4 bpp
      COMPRESSED_ETC2_EAC_RGBA,       -- 8 bpp
      COMPRESSED_PVRT_RGB,            -- 4 bpp
      COMPRESSED_PVRT_RGBA,           -- 4 bpp
      COMPRESSED_ASTC_4x4_RGBA,       -- 8 bpp
      COMPRESSED_ASTC_8x8_RGBA)       -- 2 bpp
   with Convention => C;

   for Pixel_Format'Size use Interfaces.C.int'Size;

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
      depthTexture : bool;
   end record
      with Convention => C_Pass_By_Copy;
   subtype RenderTexture is RenderTexture2D;

   --  Font character info
   type CharInfo is record
       value : int;              -- Character value (Unicode)
       offsetX : int;            -- Character offset X when drawing
       offsetY : int;            -- Character offset Y when drawing
       advanceX : int;           -- Character advance position X
       img : Image;              -- Character image data, named image in raylib.h but conflict with the type
   end record
      with Convention => C_Pass_By_Copy;

   type Font  is record
      baseSize : int;         -- Base size (default chars height)
      charsCount : int;       -- Number of characters
      texture  : Texture2D;   -- Character texture atlas
      recs : access Rectangle;  -- Characters rectangles in texture
      chars : access CharInfo; -- Characters info data
      --  CharInfo *chars;
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
      ctype : CameraType;  --  Camera type, defines projection type
   end record
      with Convention => C_Pass_By_Copy;
   type P_Camera is access all Camera3D;

   type Log is (LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_DEBUG, LOG_OTHER);
   for Log  use (
      LOG_INFO    => 1,
      LOG_WARNING => 2,
      LOG_ERROR   => 4,
      LOG_DEBUG   => 8,
      LOG_OTHER   => 16);
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

   function get_random_value (min, max : int) return int
   with
      Import => True,
      Convention => C,
      External_Name => "GetRandomValue";

   ----------------------------------------------------------------------------
   --  Window and Graphics Device Functions
   ---
   package window is
      --  [[  Window-related functions ]]  --
      procedure init (width, height : Positive; title : String);
      function  should_close return Boolean;
      procedure close
         with
            Import => True,
            Convention => C,
            External_Name => "CloseWindow";
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
   end window;

   ----------------------------------------------------------------------------
   --  Drawing-related functions
   ---
   procedure clear_background (bg_color : Color)
      with
         Import,
         Convention => C,
         External_Name => "ClearBackground";
   procedure begin_drawing
      with
         Import,
         Convention => C,
         External_Name => "BeginDrawing";
   procedure end_drawing
      with
         Import,
         Convention => C,
         External_Name => "EndDrawing";
   procedure set_target_FPS (fps : int)
      with
         Import,
         Convention => C,
         External_Name => "SetTargetFPS";

   package core is
      --  [[ Input-related functions: keyboard ]]  --
      function is_key_pressed (key : Keys) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsKeyPressed";
      function get_key_pressed return int
         with
            Import => True,
            Convention => C,
            External_Name => "GetKeyPressed";
      --  [[ Input-related functions: gamepads  ]]  --
      function is_gamepad_available (gamepad : int) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsGamepadAvailable";
      function is_gamepad_name (gamepad : int; name : chars_ptr)
         return bool
      with
         Import => True,
         Convention => C,
         External_Name => "IsGamepadName";
      function get_gamepad_name (gamepad : Gamepad_Number) return String;
      --  Detect if a gamepad button has been pressed once
      function is_gamepad_button_pressed (
         gamepad : int;
         button : Gamepad_Button)
         return bool
      with
         Import => True,
         Convention => C,
         External_Name => "IsGamepadButtonPressed";
      function is_gamepad_button_down (
         gamepad : Gamepad_Number;
         button : Gamepad_Button)
         return bool
      with
         Import => True,
         Convention => C,
         External_Name => "IsGamepadButtonDown";
      --  Get the last gamepad button pressed
      function get_gamepad_button_pressed return Gamepad_Button
      with
         Import => True,
         Convention => C,
         External_Name => "GetGamepadButtonPressed";
      --  Return gamepad axis count for a gamepad
      function get_gamepad_axis_count (gamepad : Gamepad_Number) return int
      with
         Import => True,
         Convention => C,
         External_Name => "GetGamepadAxisCount";
      --  Return axis movement value for a gamepad axis
      function get_gamepad_axis_movement (
         gamepad : Gamepad_Number;
         axis : Gamepad_Axis)
         return Float
      with
         Import => True,
         Convention => C,
         External_Name => "GetGamepadAxisMovement";
      --  [[ Input-related functions: mouse  ]] --
      function is_mouse_button_down (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonDown";
      function is_mouse_button_released (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonReleased";
      function is_mouse_button_pressed (button : Mouse_Button) return bool
         with
            Import => True,
            Convention => C,
            External_Name => "IsMouseButtonPressed";
      function get_mouse_position return Vector2
         with
            Import => True,
            Convention => C,
            External_Name => "GetMousePosition";
      procedure trace_log (ltype : Log; text : String);
   end core;

   package camera is
      procedure set_mode (camera : Camera3D; mode : CameraMode)
         with
            Import => True,
            Convention => C,
            External_Name => "SetCameraMode";
      procedure update (camera : access Camera3D)
         with
            Import => True,
            Convention => C,
            External_Name => "UpdateCamera";
   end camera;

   package shapes is
      procedure draw_line (start_posX, start_posY, end_posX, end_posy : int;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLine";

      procedure draw_line_v (start_pos, end_pos : Vector2; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLineV";

      procedure draw_line_ex (
         start_pos, end_pos : Vector2;
         thick : C_Float;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawLineEx";

      --  Draw a color-filled circle
      procedure draw_circle (centerX, centerY : int; radius : Float; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawCircle";

      procedure draw_rectangle (posX, posY, width, height : int; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangle";

      procedure draw_rectangle_rec (bounds : Rectangle; c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleRec";

      procedure draw_rectangle_lines (posX, posY, width, height : int;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleLines";

      procedure draw_rectangle_lines_ex (rec : Rectangle; line_thick : int;
         c : Color)
         with
            Import => True,
            Convention => C,
            External_Name => "DrawRectangleLinesEx";

      function check_collision_point_rec (point : Vector2; rec : Rectangle)
         return bool
         with
            Import => True,
            Convention => C,
            External_Name => "CheckCollisionPointRec";
   end shapes;

   package drawing is
      procedure begin_mode3D (camera : Camera3D);
      procedure end_mode3D;
      ------
      pragma Import (C, begin_mode3D, "BeginMode3D");
      pragma Import (C, end_mode3D, "EndMode3D");
   end drawing;

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

   ----------------------------------------------------------------------------
   --  Texture Loading and Drawing Functions
   ---
   package textures is
      --  [[ Image/Texture2D data loading/unloading/saving functions ]] --
      --  Load texture from file into GPU memory (VRAM)
      function load (filename : String) return Texture2D;
      --  Unload texture from GPU memory (VRAM)
      procedure unload (texture : Texture2D)
         with
            Import => True,
            Convention => C,
            External_Name => "UnloadTexture";
      --  [[ Texture2D drawing functions ]]  --
      procedure draw_texture (
         texture : Texture2D;
         posX, posY : int;
         tint : Color)
      with
         Import => True,
         Convention => C,
         External_Name => "DrawTexture";
      procedure draw_texture_rec (
         texture : Texture2D;
         sourceRec : Rectangle;
         position : Vector2;
         tint : Color)
      with
         Import => True,
         Convention => C,
         External_Name => "DrawTextureRec";
      procedure draw_texture_quad (
         texture : Texture2D;
         tiling : Vector2;
         offset : Vector2;
         quad : Rectangle;
         tint : Color)
      with
         Import => True,
         Convention => C,
         External_Name => "DrawTextureQuad";
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

  ---
  -- Font Loading and Text Drawing
  --
   package text is
      --  Font loading/unloading functions
      function get_font_default return Font;
      pragma Import (C, get_font_default, "GetFontDefault");
      --  Text drawing functions
      procedure draw_FPS (x, y : int);
      pragma Import (C, draw_FPS, "DrawFPS");
      procedure draw (text : String; posX, posY, fontSize : int; c : Color);
      --  Draw text using font and additional parameters
      procedure draw_ex (
         F : Font;
         text : String;
         position : Vector2;
         fontSize, spacing : Float;
         tint : Color);
      --  [[ Text misc. functions ]]  --
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

   ---
   --  Basic 3d Shapes
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


end raylib;
