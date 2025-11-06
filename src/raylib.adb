with Interfaces.C;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body raylib is

   use Interfaces.C;

   -- Helper functions for C/Ada type conversion
   function to_boolean (value : int) return Boolean
   is (if value = 0 then False else True);

   function to_c_bool (value : Boolean) return Extensions.bool
   is (if value then Extensions.True else Extensions.False);

   ---
   -- Window-related functions
   package body window is

      procedure init (width, height : Positive; title : string) is
         procedure InitWindow (w, h : int; title : strings.Chars_Ptr);
         pragma import (C, InitWindow, "InitWindow");

         ctitle : strings.Chars_Ptr := strings.new_string (title);
      begin
         InitWindow (int (width), int (height), ctitle);
      end init;

      function should_close return Boolean is
         function WindowShouldClose return int;
         pragma import (C, WindowShouldClose, "WindowShouldClose");

         close : int := WindowShouldClose;
      begin
         return (if close = 0 then FALSE else TRUE);
      end should_close;

      function is_cursor_hidden return Boolean is
         function IsCursorHidden return int;
         pragma import (C, IsCursorHidden, "IsCursorHidden");
         hidden : int := IsCursorHidden;
      begin
         return (if hidden = 0 then FALSE else TRUE);
      end is_cursor_hidden;

      function is_cursor_on_screen return Boolean is
         function IsCursorOnScreen return Extensions.bool;
         pragma import (C, IsCursorOnScreen, "IsCursorOnScreen");
      begin
         return IsCursorOnScreen = Extensions.True;
      end is_cursor_on_screen;

      function get_clipboard_text return String is
         function GetClipboardText return Strings.chars_ptr;
         pragma import (C, GetClipboardText, "GetClipboardText");
         text : Strings.chars_ptr := GetClipboardText;
      begin
         return (if text /= Strings.Null_Ptr then Strings.Value (text) else "");
      end get_clipboard_text;

   end window;

   package body textures is
      function raylib_load_image (filename : strings.Chars_Ptr) return Image
      with Import, Convention => C, External_Name => "LoadImage";

      function raylib_load_image_SVG
        (filename : strings.Chars_Ptr; width, height : int) return Image
      with Import, Convention => C, External_Name => "LoadImageSvg";

      function load_image (filename : String) return Image is
         cfilename : Strings.Chars_Ptr := Strings.New_String (filename);
      begin
         return raylib_load_image (cfilename);
      end load_image;

      function load_image_SVG
        (filename : String; width, height : int) return Image
      is
         cfilename : Strings.Chars_Ptr := Strings.New_String (filename);
      begin
         return raylib_load_image_SVG (cfilename, width, height);
      end load_image_SVG;

      function LoadTexture (filename : strings.Chars_Ptr) return Texture2D
      with Import, Convention => C, External_Name => "LoadTexture";

      function load (filename : String) return Texture2D is
         cfilename : Strings.Chars_Ptr := Strings.New_String (filename);
      begin
         return LoadTexture (cfilename);
      end load;

   end textures;

   package body text is
      ---
      -- Import C functions
      --
      procedure DrawText (text : strings.Chars_Ptr; X, Y, fs : int; c : Color);
      function LoadFont (fileName : strings.Chars_Ptr) return Font;
      function LoadFontEx
        (fileName   : strings.Chars_Ptr;
         size       : int;
         chars      : access int;
         glyphCount : int) return Font;
      function LoadFontEx2
        (fileName   : strings.Chars_Ptr;
         size       : int;
         chars      : char_list;
         glyphCount : int) return Font;
      ---
      pragma import (C, DrawText, "DrawText");
      pragma import (C, LoadFont, "LoadFont");
      pragma import (C, LoadFontEx, "LoadFontEx");
      pragma import (C, LoadFontEx2, "LoadFontEx");

      ---
      -- Wrapping functions
      --
      function load_font (file : String) return Font is
         ctext : strings.Chars_Ptr := strings.new_string (file);
      begin
         return LoadFont (ctext);
      end load_font;

      function load_font_ex
        (file : String; size : int; chars : access int; glyphCount : int)
         return Font
      is
         ctext : strings.Chars_Ptr := strings.new_string (file);
      begin
         return LoadFontEx (ctext, size, chars, glyphCount);
      end load_font_ex;

      function load_font_ex
        (file : String; size : int; chars : char_list; glyphCount : int)
         return Font
      is
         ctext : strings.Chars_Ptr := strings.new_string (file);
      begin
         return LoadFontEx2 (ctext, size, chars, glyphCount);
      end load_font_ex;

      procedure draw (text : String; posX, posY, fontSize : Int; c : Color) is
         ctext : strings.Chars_Ptr := strings.new_string (text);
      begin
         DrawText (ctext, int (posX), int (posY), int (fontSize), c);
      end draw;

      procedure draw_ex
        (F                 : Font;
         text              : String;
         position          : Vector2;
         fontSize, spacing : Float;
         tint              : Color)
      is
         procedure DrawTextEx
           (f                 : Font;
            text              : strings.Chars_Ptr;
            p                 : Vector2;
            fontSize, spacing : Float;
            tint              : Color);
         pragma Import (C, DrawTextEx, "DrawTextEx");

         ctext : strings.Chars_Ptr := strings.new_string (text);
      begin
         DrawTextEx (F, ctext, position, fontSize, spacing, tint);
      end draw_ex;

      function measure (text : String; fontSize : int) return int is
         function MeasureText
           (text : Strings.chars_ptr; fontSize : int) return int
         with Import, Convention => C, External_Name => "MeasureText";

         ctext : Strings.chars_ptr := Strings.New_String (text);
      begin
         return MeasureText (ctext, fontSize);
      end measure;

      function measure_ex
        (f : Font; text : String; fontSize, spacing : Float) return Vector2
      is
         function MeasureTextEx
           (f : Font; text : Strings.Chars_Ptr; fontSize, spacing : Float)
            return Vector2;
         ------
         pragma Import (C, MeasureTextEx, "MeasureTextEx");
         ctext : Strings.Chars_Ptr := Strings.New_String (text);
      begin
         return MeasureTextEx (f, ctext, fontSize, spacing);
      end measure_ex;

   end text; ---

   package body utils is
      procedure trace_log (ltype : Log; text : String) is
         procedure TraceLog (ltype : Log; text : strings.Chars_Ptr);
         pragma import (C, TraceLog, "TraceLog");

         ctext : strings.Chars_Ptr := strings.new_string (text);
      begin
         TraceLog (ltype, ctext);
      end trace_log;
   end utils;

   package body input is
      -- Keyboard functions
      function is_key_pressed (key : keys) return boolean is
         function IsKeyPressed (key : keys) return int;
         pragma import (C, IsKeyPressed, "IsKeyPressed");
         pressed : int := IsKeyPressed (key);
      begin
         return to_boolean (pressed);
      end is_key_pressed;

      function is_key_pressed_repeat (key : Keys) return Boolean is
         function IsKeyPressedRepeat (key : Keys) return Extensions.bool;
         pragma import (C, IsKeyPressedRepeat, "IsKeyPressedRepeat");
      begin
         return IsKeyPressedRepeat (key) = Extensions.True;
      end is_key_pressed_repeat;

      function is_key_down (key : Keys) return Boolean is
         function IsKeyDown (key : Keys) return Extensions.bool;
         pragma import (C, IsKeyDown, "IsKeyDown");
      begin
         return IsKeyDown (key) = Extensions.True;
      end is_key_down;

      function is_key_released (key : Keys) return Boolean is
         function IsKeyReleased (key : Keys) return Extensions.bool;
         pragma import (C, IsKeyReleased, "IsKeyReleased");
      begin
         return IsKeyReleased (key) = Extensions.True;
      end is_key_released;

      function is_key_up (key : Keys) return Boolean is
         function IsKeyUp (key : Keys) return Extensions.bool;
         pragma import (C, IsKeyUp, "IsKeyUp");
      begin
         return IsKeyUp (key) = Extensions.True;
      end is_key_up;

      -- Gamepad functions
      function is_gamepad_available (gamepad : int) return Boolean is
         function IsGamepadAvailable (gamepad : int) return Extensions.bool;
         pragma import (C, IsGamepadAvailable, "IsGamepadAvailable");
      begin
         return IsGamepadAvailable (gamepad) = Extensions.True;
      end is_gamepad_available;

      function get_gamepad_name (gamepad : int) return string is
         use Interfaces.C.Strings;
         function GetGamepadName (arg1 : int) return chars_ptr
         with Import, Convention => C, External_Name => "GetGamepadName";
         Gamepad_Name : chars_ptr := GetGamepadName (gamepad);
      begin
         return
           (if Gamepad_Name /= Null_Ptr then Value (Gamepad_Name) else "");
      end get_gamepad_name;

      function is_gamepad_button_pressed (gamepad, button : int) return Boolean
      is
         function IsGamepadButtonPressed (gamepad, button : int) return int;
         pragma import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");
         pressed : int := IsGamepadButtonPressed (gamepad, button);
      begin
         return to_boolean (pressed);
      end is_gamepad_button_pressed;

      function is_gamepad_button_down
        (gamepad : int; button : Gamepad_Button) return Boolean
      is
         function IsGamepadButtonDown (gamepad : int; button : Gamepad_Button)
            return Extensions.bool;
         pragma import (C, IsGamepadButtonDown, "IsGamepadButtonDown");
      begin
         return IsGamepadButtonDown (gamepad, button) = Extensions.True;
      end is_gamepad_button_down;

      function is_gamepad_button_released
        (gamepad : int; button : Gamepad_Button) return Boolean
      is
         function IsGamepadButtonReleased (gamepad : int; button : Gamepad_Button)
            return Extensions.bool;
         pragma import (C, IsGamepadButtonReleased, "IsGamepadButtonReleased");
      begin
         return IsGamepadButtonReleased (gamepad, button) = Extensions.True;
      end is_gamepad_button_released;

      function is_gamepad_button_up
        (gamepad : int; button : Gamepad_Button) return Boolean
      is
         function IsGamepadButtonUp (gamepad : int; button : Gamepad_Button)
            return Extensions.bool;
         pragma import (C, IsGamepadButtonUp, "IsGamepadButtonUp");
      begin
         return IsGamepadButtonUp (gamepad, button) = Extensions.True;
      end is_gamepad_button_up;

      -- Mouse functions
      function is_mouse_button_pressed (button : Mouse_Button) return Boolean is
         function IsMouseButtonPressed (button : Mouse_Button) return Extensions.bool;
         pragma import (C, IsMouseButtonPressed, "IsMouseButtonPressed");
      begin
         return IsMouseButtonPressed (button) = Extensions.True;
      end is_mouse_button_pressed;

      function is_mouse_button_down (button : Mouse_Button) return Boolean is
         function IsMouseButtonDown (button : Mouse_Button) return Extensions.bool;
         pragma import (C, IsMouseButtonDown, "IsMouseButtonDown");
      begin
         return IsMouseButtonDown (button) = Extensions.True;
      end is_mouse_button_down;

      function is_mouse_button_released (button : Mouse_Button) return Boolean is
         function IsMouseButtonReleased (button : Mouse_Button) return Extensions.bool;
         pragma import (C, IsMouseButtonReleased, "IsMouseButtonReleased");
      begin
         return IsMouseButtonReleased (button) = Extensions.True;
      end is_mouse_button_released;

      function is_mouse_button_up (button : Mouse_Button) return Boolean is
         function IsMouseButtonUp (button : Mouse_Button) return Extensions.bool;
         pragma import (C, IsMouseButtonUp, "IsMouseButtonUp");
      begin
         return IsMouseButtonUp (button) = Extensions.True;
      end is_mouse_button_up;

   end input;

   package body shapes is
      -- Collision detection functions
      function check_collision_recs (rec1, rec2 : Rectangle) return Boolean is
         function CheckCollisionRecs (rec1, rec2 : Rectangle) return Extensions.bool;
         pragma import (C, CheckCollisionRecs, "CheckCollisionRecs");
      begin
         return CheckCollisionRecs (rec1, rec2) = Extensions.True;
      end check_collision_recs;

      function check_collision_circles
        (center1 : Vector2; radius1 : Float; center2 : Vector2; radius2 : Float)
         return Boolean
      is
         function CheckCollisionCircles
           (center1 : Vector2; radius1 : Float; center2 : Vector2; radius2 : Float)
            return Extensions.bool;
         pragma import (C, CheckCollisionCircles, "CheckCollisionCircles");
      begin
         return CheckCollisionCircles (center1, radius1, center2, radius2) = Extensions.True;
      end check_collision_circles;

      function check_collision_circle_rec
        (center : Vector2; radius : Float; rec : Rectangle) return Boolean
      is
         function CheckCollisionCircleRec
           (center : Vector2; radius : Float; rec : Rectangle) return Extensions.bool;
         pragma import (C, CheckCollisionCircleRec, "CheckCollisionCircleRec");
      begin
         return CheckCollisionCircleRec (center, radius, rec) = Extensions.True;
      end check_collision_circle_rec;

      function check_collision_point_rec
        (point : Vector2; rec : Rectangle) return Boolean
      is
         function CheckCollisionPointRec
           (point : Vector2; rec : Rectangle) return Extensions.bool;
         pragma import (C, CheckCollisionPointRec, "CheckCollisionPointRec");
      begin
         return CheckCollisionPointRec (point, rec) = Extensions.True;
      end check_collision_point_rec;

      function check_collision_point_circle
        (point, center : Vector2; radius : Float) return Boolean
      is
         function CheckCollisionPointCircle
           (point, center : Vector2; radius : Float) return Extensions.bool;
         pragma import (C, CheckCollisionPointCircle, "CheckCollisionPointCircle");
      begin
         return CheckCollisionPointCircle (point, center, radius) = Extensions.True;
      end check_collision_point_circle;

      function check_collision_point_triangle
        (point, p1, p2, p3 : Vector2) return Boolean
      is
         function CheckCollisionPointTriangle
           (point, p1, p2, p3 : Vector2) return Extensions.bool;
         pragma import (C, CheckCollisionPointTriangle, "CheckCollisionPointTriangle");
      begin
         return CheckCollisionPointTriangle (point, p1, p2, p3) = Extensions.True;
      end check_collision_point_triangle;

      function check_collision_point_poly
        (point : Vector2; points : access Vector2; point_count : int)
         return Boolean
      is
         function CheckCollisionPointPoly
           (point : Vector2; points : access Vector2; point_count : int)
            return Extensions.bool;
         pragma import (C, CheckCollisionPointPoly, "CheckCollisionPointPoly");
      begin
         return CheckCollisionPointPoly (point, points, point_count) = Extensions.True;
      end check_collision_point_poly;

      function check_collision_lines
        (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
         collision_point                            : access Vector2)
         return Boolean
      is
         function CheckCollisionLines
           (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
            collision_point                            : access Vector2)
            return Extensions.bool;
         pragma import (C, CheckCollisionLines, "CheckCollisionLines");
      begin
         return CheckCollisionLines
           (start_pos1, end_pos1, start_pos2, end_pos2, collision_point) = Extensions.True;
      end check_collision_lines;

      function check_collision_point_line
        (point, p1, p2 : Vector2; threshold : int) return Boolean
      is
         function CheckCollisionPointLine
           (point, p1, p2 : Vector2; threshold : int) return Extensions.bool;
         pragma import (C, CheckCollisionPointLine, "CheckCollisionPointLine");
      begin
         return CheckCollisionPointLine (point, p1, p2, threshold) = Extensions.True;
      end check_collision_point_line;

   end shapes;

   package body rcamera is
      -- Camera movement functions with Boolean parameters
      procedure Camera_Move_Forward
        (camera : access Camera3D; distance : Float; moveInWorldPlane : Boolean)
      is
         procedure CameraMoveForward
           (camera : access Camera3D; distance : Float; moveInWorldPlane : Extensions.bool);
         pragma import (C, CameraMoveForward, "CameraMoveForward");
      begin
         CameraMoveForward (camera, distance, to_c_bool (moveInWorldPlane));
      end Camera_Move_Forward;

      procedure Camera_Move_Right
        (camera : access Camera3D; distance : Float; moveInWorldPlane : Boolean)
      is
         procedure CameraMoveRight
           (camera : access Camera3D; distance : Float; moveInWorldPlane : Extensions.bool);
         pragma import (C, CameraMoveRight, "CameraMoveRight");
      begin
         CameraMoveRight (camera, distance, to_c_bool (moveInWorldPlane));
      end Camera_Move_Right;

      procedure Camera_Yaw
        (camera : access Camera3D; angle : Float; rotateAroundTarget : Boolean)
      is
         procedure CameraYaw
           (camera : access Camera3D; angle : Float; rotateAroundTarget : Extensions.bool);
         pragma import (C, CameraYaw, "CameraYaw");
      begin
         CameraYaw (camera, angle, to_c_bool (rotateAroundTarget));
      end Camera_Yaw;

      procedure Camera_Pitch
        (camera                                 : access Camera3D;
         angle                                  : Float;
         lockView, rotateAroundTarget, rotateUp : Boolean)
      is
         procedure CameraPitch
           (camera                                 : access Camera3D;
            angle                                  : Float;
            lockView, rotateAroundTarget, rotateUp : Extensions.bool);
         pragma import (C, CameraPitch, "CameraPitch");
      begin
         CameraPitch (camera, angle,
                      to_c_bool (lockView),
                      to_c_bool (rotateAroundTarget),
                      to_c_bool (rotateUp));
      end Camera_Pitch;

   end rcamera;

   package body Audio is
      -- Audio device management
      function is_audio_device_ready return Boolean is
         function IsAudioDeviceReady return Extensions.bool;
         pragma import (C, IsAudioDeviceReady, "IsAudioDeviceReady");
      begin
         return IsAudioDeviceReady = Extensions.True;
      end is_audio_device_ready;

      procedure set_master_volume (volume : Float) is
         procedure SetMasterVolume (volume : C_float);
         pragma import (C, SetMasterVolume, "SetMasterVolume");
      begin
         SetMasterVolume (C_float (volume));
      end set_master_volume;

      function get_master_volume return Float is
         function GetMasterVolume return C_float;
         pragma import (C, GetMasterVolume, "GetMasterVolume");
      begin
         return Float (GetMasterVolume);
      end get_master_volume;

      -- Sound loading
      function load_sound (file : String) return Sound is
         function LoadSound (file : Strings.chars_ptr) return Sound;
         pragma import (C, LoadSound, "LoadSound");
         cfile : Strings.chars_ptr := Strings.New_String (file);
      begin
         return LoadSound (cfile);
      end load_sound;

      function is_sound_ready (s : Sound) return Boolean is
         function IsSoundReady (s : Sound) return Extensions.bool;
         pragma import (C, IsSoundReady, "IsSoundReady");
      begin
         return IsSoundReady (s) = Extensions.True;
      end is_sound_ready;

   end Audio;

end raylib;
