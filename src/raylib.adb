with Interfaces.C;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body raylib is

    use Interfaces.C;

    package body textures is
        function raylib_load_image (filename : char_array) return Image
        with Import, Convention => C, External_Name => "LoadImage";

        function raylib_load_image_SVG
           (filename : char_array; width, height : int) return Image
        with Import, Convention => C, External_Name => "LoadImageSvg";

        function load_image (filename : String) return Image is
            cfilename : char_array := To_C (filename);
        begin
            return raylib_load_image (cfilename);
        end load_image;

        function load_image_SVG
           (filename : String; width, height : int) return Image
        is
            cfilename : char_array := To_C (filename);
        begin
            return raylib_load_image_SVG (cfilename, width, height);
        end load_image_SVG;

        function LoadTexture (filename : char_array) return Texture2D
        with Import, Convention => C, External_Name => "LoadTexture";

        function load (filename : String) return Texture2D is
            cfilename : char_array := To_C (filename);
        begin
            return LoadTexture (cfilename);
        end load;

    end textures;

    package body text is
        ---
        -- Import C functions
        --
        procedure DrawText (text : char_array; X, Y, fs : int; c : Color);
        function LoadFont (fileName : char_array) return Font;
        function LoadFontEx
           (fileName   : char_array;
            size       : int;
            chars      : access int;
            glyphCount : int) return Font;
        ---
        pragma import (C, DrawText, "DrawText");
        pragma import (C, LoadFont, "LoadFont");
        pragma import (C, LoadFontEx, "LoadFontEx");

        ---
        -- Wrapping functions
        --
        function load_font (file : String) return Font is
            ctext : char_array := To_C (file);
        begin
            return LoadFont (ctext);
        end load_font;

        function load_font_ex
           (file : String; size : int; chars : access int; glyphCount : int)
            return Font
        is
            ctext : char_array := To_C (file);
        begin
            return LoadFontEx (ctext, size, chars, glyphCount);
        end load_font_ex;

        procedure draw (text : String; posX, posY, fontSize : Int; c : Color)
        is
            ctext : char_array := To_C (text);
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
                text              : char_array;
                p                 : Vector2;
                fontSize, spacing : Float;
                tint              : Color);
            pragma Import (C, DrawTextEx, "DrawTextEx");

            ctext : char_array := To_C (text);
        begin
            DrawTextEx (F, ctext, position, fontSize, spacing, tint);
        end draw_ex;

        function measure (text : String; fontSize : int) return int is
            function MeasureText (text : char_array; fontSize : int) return int
            with Import, Convention => C, External_Name => "MeasureText";

            ctext : char_array := To_C (text);
        begin
            return MeasureText (ctext, fontSize);
        end measure;

        function measure_ex
           (f : Font; text : String; fontSize, spacing : Float) return Vector2
        is
            function MeasureTextEx
               (f : Font; text : char_array; fontSize, spacing : Float)
                return Vector2;
            ------
            pragma Import (C, MeasureTextEx, "MeasureTextEx");
            ctext : char_array := To_C (text);
        begin
            return MeasureTextEx (f, ctext, fontSize, spacing);
        end measure_ex;

    end text; ---

    package body utils is
        procedure trace_log (ltype : Log; text : String) is
            procedure TraceLog (ltype : Log; text : char_array);
            pragma import (C, TraceLog, "TraceLog");

            ctext : char_array := To_C (text);
        begin
            TraceLog (ltype, ctext);
        end trace_log;
    end utils;

    package body input is
        -- Keyboard functions
        function Is_Key_Pressed (key : keys) return Boolean is
            function IsKeyPressed (key : keys) return int;
            pragma import (C, IsKeyPressed, "IsKeyPressed");
        begin
            return IsKeyPressed (key) /= 0;
        end Is_Key_Pressed;

        function Is_Key_Pressed_Repeat (key : Keys) return Boolean is
            function IsKeyPressedRepeat (key : Keys) return int;
            pragma import (C, IsKeyPressedRepeat, "IsKeyPressedRepeat");
        begin
            return IsKeyPressedRepeat (key) /= 0;
        end Is_Key_Pressed_Repeat;

        function is_key_down (key : Keys) return Boolean is
            function IsKeyDown (key : Keys) return int;
            pragma import (C, IsKeyDown, "IsKeyDown");
        begin
            return IsKeyDown (key) /= 0;
        end is_key_down;

        function is_key_released (key : Keys) return Boolean is
            function IsKeyReleased (key : Keys) return int;
            pragma import (C, IsKeyReleased, "IsKeyReleased");
        begin
            return IsKeyReleased (key) /= 0;
        end is_key_released;

        function is_key_up (key : Keys) return Boolean is
            function IsKeyUp (key : Keys) return int;
            pragma import (C, IsKeyUp, "IsKeyUp");
        begin
            return IsKeyUp (key) /= 0;
        end is_key_up;

        -- Gamepad functions
        function is_gamepad_available (gamepad : int) return Boolean is
            function IsGamepadAvailable (gamepad : int) return int;
            pragma import (C, IsGamepadAvailable, "IsGamepadAvailable");
        begin
            return IsGamepadAvailable (gamepad) /= 0;
        end is_gamepad_available;

        function get_gamepad_name (gamepad : int) return String is
            use Interfaces.C.Strings;
            function GetGamepadName (arg1 : int) return chars_ptr
            with Import, Convention => C, External_Name => "GetGamepadName";
            Gamepad_Name : chars_ptr := GetGamepadName (gamepad);
        begin
            return
               (if Gamepad_Name /= Null_Ptr then Value (Gamepad_Name) else "");
        end get_gamepad_name;

        function is_gamepad_button_pressed
           (gamepad, button : int) return Boolean
        is
            function IsGamepadButtonPressed (gamepad, button : int) return int;
            pragma
               import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");
        begin
            return IsGamepadButtonPressed (gamepad, button) /= 0;
        end is_gamepad_button_pressed;

        function is_gamepad_button_down
           (gamepad : int; button : Gamepad_Button) return Boolean
        is
            function IsGamepadButtonDown
               (gamepad : int; button : Gamepad_Button) return int;
            pragma import (C, IsGamepadButtonDown, "IsGamepadButtonDown");
        begin
            return IsGamepadButtonDown (gamepad, button) /= 0;
        end is_gamepad_button_down;

        function is_gamepad_button_released
           (gamepad : int; button : Gamepad_Button) return Boolean
        is
            function IsGamepadButtonReleased
               (gamepad : int; button : Gamepad_Button) return int;
            pragma
               import (C, IsGamepadButtonReleased, "IsGamepadButtonReleased");
        begin
            return IsGamepadButtonReleased (gamepad, button) /= 0;
        end is_gamepad_button_released;

        function is_gamepad_button_up
           (gamepad : int; button : Gamepad_Button) return Boolean
        is
            function IsGamepadButtonUp
               (gamepad : int; button : Gamepad_Button) return int;
            pragma import (C, IsGamepadButtonUp, "IsGamepadButtonUp");
        begin
            return IsGamepadButtonUp (gamepad, button) /= 0;
        end is_gamepad_button_up;

        -- Mouse functions
        function is_mouse_button_pressed (button : Mouse_Button) return Boolean
        is
            function IsMouseButtonPressed (button : Mouse_Button) return int;
            pragma import (C, IsMouseButtonPressed, "IsMouseButtonPressed");
        begin
            return IsMouseButtonPressed (button) /= 0;
        end is_mouse_button_pressed;

        function is_mouse_button_down (button : Mouse_Button) return Boolean is
            function IsMouseButtonDown (button : Mouse_Button) return int;
            pragma import (C, IsMouseButtonDown, "IsMouseButtonDown");
        begin
            return IsMouseButtonDown (button) /= 0;
        end is_mouse_button_down;

        function is_mouse_button_released
           (button : Mouse_Button) return Boolean
        is
            function IsMouseButtonReleased (button : Mouse_Button) return int;
            pragma import (C, IsMouseButtonReleased, "IsMouseButtonReleased");
        begin
            return IsMouseButtonReleased (button) /= 0;
        end is_mouse_button_released;

        function is_mouse_button_up (button : Mouse_Button) return Boolean is
            function IsMouseButtonUp (button : Mouse_Button) return int;
            pragma import (C, IsMouseButtonUp, "IsMouseButtonUp");
        begin
            return IsMouseButtonUp (button) /= 0;
        end is_mouse_button_up;

    end input;

    package body shapes is
        -- Collision detection functions
        function check_collision_recs (rec1, rec2 : Rectangle) return Boolean
        is
            function CheckCollisionRecs (rec1, rec2 : Rectangle) return int;
            pragma import (C, CheckCollisionRecs, "CheckCollisionRecs");
        begin
            return CheckCollisionRecs (rec1, rec2) /= 0;
        end check_collision_recs;

        function check_collision_circles
           (center1 : Vector2;
            radius1 : Float;
            center2 : Vector2;
            radius2 : Float) return Boolean
        is
            function CheckCollisionCircles
               (center1 : Vector2;
                radius1 : Float;
                center2 : Vector2;
                radius2 : Float) return int;
            pragma import (C, CheckCollisionCircles, "CheckCollisionCircles");
        begin
            return
               CheckCollisionCircles (center1, radius1, center2, radius2) /= 0;
        end check_collision_circles;

        function check_collision_circle_rec
           (center : Vector2; radius : Float; rec : Rectangle) return Boolean
        is
            function CheckCollisionCircleRec
               (center : Vector2; radius : Float; rec : Rectangle) return int;
            pragma
               import (C, CheckCollisionCircleRec, "CheckCollisionCircleRec");
        begin
            return CheckCollisionCircleRec (center, radius, rec) /= 0;
        end check_collision_circle_rec;

        function check_collision_point_rec
           (point : Vector2; rec : Rectangle) return Boolean
        is
            function CheckCollisionPointRec
               (point : Vector2; rec : Rectangle) return int;
            pragma
               import (C, CheckCollisionPointRec, "CheckCollisionPointRec");
        begin
            return CheckCollisionPointRec (point, rec) /= 0;
        end check_collision_point_rec;

        function check_collision_point_circle
           (point, center : Vector2; radius : Float) return Boolean
        is
            function CheckCollisionPointCircle
               (point, center : Vector2; radius : Float) return int;
            pragma
               import
                  (C, CheckCollisionPointCircle, "CheckCollisionPointCircle");
        begin
            return CheckCollisionPointCircle (point, center, radius) /= 0;
        end check_collision_point_circle;

        function check_collision_point_triangle
           (point, p1, p2, p3 : Vector2) return Boolean
        is
            function CheckCollisionPointTriangle
               (point, p1, p2, p3 : Vector2) return int;
            pragma
               import
                  (C,
                   CheckCollisionPointTriangle,
                   "CheckCollisionPointTriangle");
        begin
            return CheckCollisionPointTriangle (point, p1, p2, p3) /= 0;
        end check_collision_point_triangle;

        function check_collision_point_poly
           (point : Vector2; points : access Vector2; point_count : int)
            return Boolean
        is
            function CheckCollisionPointPoly
               (point : Vector2; points : access Vector2; point_count : int)
                return int;
            pragma
               import (C, CheckCollisionPointPoly, "CheckCollisionPointPoly");
        begin
            return CheckCollisionPointPoly (point, points, point_count) /= 0;
        end check_collision_point_poly;

        function check_collision_lines
           (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
            collision_point                            : access Vector2)
            return Boolean
        is
            function CheckCollisionLines
               (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
                collision_point                            : access Vector2)
                return int;
            pragma import (C, CheckCollisionLines, "CheckCollisionLines");
        begin
            return
               CheckCollisionLines
                  (start_pos1, end_pos1, start_pos2, end_pos2, collision_point)
               /= 0;
        end check_collision_lines;

        function check_collision_point_line
           (point, p1, p2 : Vector2; threshold : int) return Boolean
        is
            function CheckCollisionPointLine
               (point, p1, p2 : Vector2; threshold : int) return int;
            pragma
               import (C, CheckCollisionPointLine, "CheckCollisionPointLine");
        begin
            return CheckCollisionPointLine (point, p1, p2, threshold) /= 0;
        end check_collision_point_line;

    end shapes;

    package body rcamera is
        -- Camera movement functions with Boolean parameters
        procedure Camera_Move_Forward
           (camera           : access Camera3D;
            distance         : Float;
            moveInWorldPlane : Boolean)
        is
            procedure CameraMoveForward
               (camera           : access Camera3D;
                distance         : Float;
                moveInWorldPlane : int);
            pragma import (C, CameraMoveForward, "CameraMoveForward");
        begin
            CameraMoveForward
               (camera, distance, Boolean'Pos (moveInWorldPlane));
        end Camera_Move_Forward;

        procedure Camera_Move_Right
           (camera           : access Camera3D;
            distance         : Float;
            moveInWorldPlane : Boolean)
        is
            procedure CameraMoveRight
               (camera           : access Camera3D;
                distance         : Float;
                moveInWorldPlane : int);
            pragma import (C, CameraMoveRight, "CameraMoveRight");
        begin
            CameraMoveRight (camera, distance, Boolean'Pos (moveInWorldPlane));
        end Camera_Move_Right;

        procedure Camera_Yaw
           (camera             : access Camera3D;
            angle              : Float;
            rotateAroundTarget : Boolean)
        is
            procedure CameraYaw
               (camera             : access Camera3D;
                angle              : Float;
                rotateAroundTarget : int);
            pragma import (C, CameraYaw, "CameraYaw");
        begin
            CameraYaw (camera, angle, Boolean'Pos (rotateAroundTarget));
        end Camera_Yaw;

        procedure Camera_Pitch
           (camera                                 : access Camera3D;
            angle                                  : Float;
            lockView, rotateAroundTarget, rotateUp : Boolean)
        is
            procedure CameraPitch
               (camera                                 : access Camera3D;
                angle                                  : Float;
                lockView, rotateAroundTarget, rotateUp : int);
            pragma import (C, CameraPitch, "CameraPitch");
        begin
            CameraPitch
               (camera,
                angle,
                Boolean'Pos (lockView),
                Boolean'Pos (rotateAroundTarget),
                Boolean'Pos (rotateUp));
        end Camera_Pitch;

    end rcamera;

end raylib;
