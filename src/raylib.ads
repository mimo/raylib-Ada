with interfaces.C;
with system;
---
-- for reference use :
--   https://www.adaic.org/resources/add_content/standards/05rm/html/RM-B-3.html

package raylib is

   subtype int is interfaces.C.int;
   subtype c_float is interfaces.C.C_float;
   subtype unsigned is interfaces.C.unsigned;
   subtype unsigned_char is interfaces.C.unsigned_char;

   type Vector2 is record
      x, y : c_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Vector2);

   type Vector3 is record
      x, y, z : C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Vector3);

   type Vector4 is record
      x, y, z, w : C_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Vector4);

   type Color is record
      r, g, b, a : unsigned_char;
   end record;
   pragma Convention (C_Pass_By_Copy, Color);

   type Rectangle is record
     x, y, width, height : c_float;
   end record;
   pragma Convention (C_Pass_By_Copy, Rectangle);

   type Pixel_Format is (
     UNCOMPRESSED_GRAYSCALE,     -- 8 bit per pixel (no alpha)
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
     COMPRESSED_ASTC_8x8_RGBA);      -- 2 bpp

   for Pixel_Format'Size use int'Size;

   type Texture2D is record
     id : unsigned;
     width, height : int;
     mimaps : int;
     format : Pixel_Format;
   end record;
   pragma Convention (C_Pass_By_Copy, Texture2D);

   type Image is record
     data : system.Address;
     width, height, mipmaps : int;
     format : Pixel_Format;
   end record;
   pragma Convention (C_Pass_By_Copy, Image);

   -- Camera system modes
   type CameraMode is (
      CAMERA_CUSTOM,
      CAMERA_FREE,
      CAMERA_ORBITAL,
      CAMERA_FIRST_PERSON,
      CAMERA_THIRD_PERSON);

   for CameraMode use (
      CAMERA_CUSTOM  => 0,
      CAMERA_FREE    => 1,
      CAMERA_ORBITAL => 2,
      CAMERA_FIRST_PERSON => 3,
      CAMERA_THIRD_PERSON => 4);
   for CameraMode'Size use int'Size;

   -- Camera projection modes
   type CameraType is (
      CAMERA_PERSPECTIVE,
      CAMERA_ORTHOGRAPHIC);
   for CameraType use (
      CAMERA_PERSPECTIVE => 0,
      CAMERA_ORTHOGRAPHIC => 1);
   for CameraType'size use int'size;

 -- Camera type, defines a camera position/orientation in 3d space
 type Camera3D is record
    position : Vector3;  -- Camera position
    target   : Vector3;  -- Camera target it looks-at
    up   : Vector3;      -- Camera up vector (rotation over its axis)
    fovy : C_float;      -- Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
    ctype : CameraType;  -- Camera type, defines projection type: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
 end record;
 pragma Convention (C_Pass_By_Copy, Camera3D);
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
  DARKPURPLE: constant Color := (112, 31, 126, 255);
  BEIGE     : constant Color := (211, 176, 131, 255);
  BROWN     : constant Color := (127, 106, 79, 255);
  DARKBROWN : constant Color := (76, 63, 47, 255);

  WHITE     : constant Color := (255, 255, 255, 255);
  BLACK     : constant Color := (0, 0, 0, 255);
  BLANK     : constant Color := (0, 0, 0, 0);
  MAGENTA   : constant Color := (255, 0, 255, 255);
  RAYWHITE  : constant Color := (245, 245, 245, 255);

  function get_random_value (min, max : int) return int;

  ---
  -- Window and Graphics Device Functions
  --
  package window is
    -- Window-related functions
    procedure init (width, height : Positive ; title : String);
    function  should_close return Boolean;
    procedure close;
    -- Cursor-related functions
    procedure show_cursor; -- Shows cursor
    procedure hide_cursor; -- Hides cursor
    function is_cursor_hidden return Boolean; --                                 // Check if cursor is not visible
    procedure enable_cursor;  --                                  // Enables cursor (unlock cursor)
    procedure disable_cursor; --                                  // Disables cursor (lock cursor)
    ------
    pragma import (C, close, "CloseWindow");
    pragma import (C, hide_cursor, "HideCursor");
    pragma import (C, show_cursor, "ShowCursor");
    pragma import (C, enable_cursor, "EnableCursor");
    pragma import (C, disable_cursor, "DisableCursor");
  end window;

  ---
  -- Drawing-related functions
  --

  procedure clear_background (bg_color : Color);
  procedure begin_drawing;
  procedure end_drawing;
  procedure set_target_FPS (fps : int);

  function fade (c : color ; alpha : float) return Color;

  package core is
    -- Input-related functions: gamepads
    function is_gamepad_available (gamepad : int) return boolean; -- Detect if a gamepad is available
    -- RLAPI bool IsGamepadName(int gamepad, const char *name);      -- Check gamepad name (if available)
    -- RLAPI const char *GetGamepadName(int gamepad);                - Return gamepad internal name id
    function is_gamepad_button_pressed (gamepad, button : int) return Boolean; -- Detect if a gamepad button has been pressed once
    function get_gamepad_button_pressed return int; -- Get the last gamepad button pressed
    function get_gamepad_axis_count(gamepad : int) return int;  -- Return gamepad axis count for a gamepad
    function get_gamepad_axis_movement(gamepad, axis : int) return float; -- Return axis movement value for a gamepad axis
    --
    procedure trace_log (ltype : Log ; text : String);
    ------
    pragma import (C, get_gamepad_button_pressed, "GetGamepadButtonPressed");
    pragma import (C, get_gamepad_axis_count, "GetGamepadAxisCount");
    pragma import (C, get_gamepad_axis_movement, "GetGamepadAxisMovement");
  end core;

  package camera is
    procedure set_mode (camera : Camera3D; mode : CameraMode);
    procedure update (camera : P_Camera);
    ------
    pragma import (C, set_mode, "SetCameraMode");
  end camera;

  package shapes is
    procedure draw_line (start_posX, start_posY, end_posX, end_posy : int ; c : Color); -- untested
    procedure draw_line_v (start_pos, end_pos : Vector2 ; c : Color); -- untested
    procedure draw_line_ex (start_pos, end_pos : Vector2 ; thick : C_float ; c : Color);
    procedure draw_rectangle (posX, posY, width, height : int ; c : Color);
    procedure draw_rectangle_lines (posX, posY, width, height : int ; c : Color);
    ------
    pragma import (C, draw_line, "DrawLine");
    pragma import (C, draw_line_v, "DrawLineV");
    pragma import (C, draw_line_ex, "DrawLineEx");
    pragma import (C, draw_rectangle, "DrawRectangle");
    pragma import (C, draw_rectangle_lines, "DrawRectangleLines");
  end Shapes;

  package drawing is
     procedure begin_mode3D (camera : Camera3D);
     procedure end_mode3D;
     ------
     pragma import (C, begin_mode3D, "BeginMode3D");
     pragma import (C, end_mode3D, "EndMode3D");
  end drawing;

  ---
  -- Texture Loading and Drawing Functions
  --
  package textures is
    -- Image/Texture2D data loading/unloading/saving functions
    function load_texture(filename : String) return Texture2D; -- Load texture from file into GPU memory (VRAM)
    procedure unload_texture (texture : Texture2D);            -- Unload texture from GPU memory (VRAM)
    -- Texture2D drawing functions
    procedure draw_texture_rec (texture : Texture2D ; sourceRec : Rectangle ; position : Vector2 ; tint : Color);
    ------
    pragma import (C, unload_texture, "UnloadTexture");
    pragma import (C, draw_texture_rec, "DrawTextureRec" );
  end textures;

  ---
  -- Font Loading and Text Drawing
  --
  package text is
    -- Font loading/unloading functions
    procedure get_default_font;
    -- Text drawing functions
    procedure draw_FPS (x, y : Int);
    procedure draw (text : String ; posX, posY, fontSize : Int; c : Color);
    ------
    pragma import (C, get_default_font, "GetDefaultFont");
    pragma import (C, draw_FPS, "DrawFPS");
  end text;

  ---
  -- Basic 3d Shapes
  package shapes3D is
    procedure draw_plane (center_pos : Vector3 ; size : Vector2 ; c : Color);
    procedure draw_cube_v (position, size : Vector3 ; c : Color );
    procedure draw_cube_wires (position : Vector3 ; width, height, length : C_float ; c : Color);
    ------
    pragma import (C, draw_plane, "DrawPlane");
    pragma import (C, draw_cube_v, "DrawCubeV");
    pragma import (C, draw_cube_wires, "DrawCubeWires");
  end shapes3D;
private

  use interfaces.C;

  pragma import (C, get_random_value, "GetRandomValue");
  pragma import (C, clear_background, "ClearBackground");
  pragma import (C, begin_drawing, "BeginDrawing");
  pragma import (C, end_drawing, "EndDrawing");
  pragma import (C, set_target_FPS, "SetTargetFPS");

end raylib;
