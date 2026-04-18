with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body Raylib is

    use Interfaces.C;
    use Interfaces.C.Extensions;

    package body Textures is
        function raylib_load_image (filename : char_array) return Image
        with Import, Convention => C, External_Name => "LoadImage";

        function raylib_load_image_SVG
           (filename : char_array; width, height : int) return Image
        with Import, Convention => C, External_Name => "LoadImageSvg";

        function Load_Image (filename : String) return Image is
            cfilename : char_array := To_C (filename);
        begin
            return raylib_load_image (cfilename);
        end Load_Image;

        function Load_Image_SVG
           (filename : String; width, height : int) return Image
        is
            cfilename : char_array := To_C (filename);
        begin
            return raylib_load_image_SVG (cfilename, width, height);
        end Load_Image_SVG;

        function LoadTexture (filename : char_array) return Texture2D
        with Import, Convention => C, External_Name => "LoadTexture";

        function Load (filename : String) return Texture2D is
            cfilename : char_array := To_C (filename);
        begin
            return LoadTexture (cfilename);
        end load;

    end Textures;

    package body Text is
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
        function Load_Font (file : String) return Font is
            ctext : char_array := To_C (file);
        begin
            return LoadFont (ctext);
        end Load_Font;

        function Load_Font_Ex
           (file : String; size : int; chars : access int; glyphCount : int)
            return Font
        is
            ctext : char_array := To_C (file);
        begin
            return LoadFontEx (ctext, size, chars, glyphCount);
        end Load_Font_Ex;

        procedure Draw (text : String; posX, posY, fontSize : Int; c : Color)
        is
            ctext : char_array := To_C (text);
        begin
            DrawText (ctext, int (posX), int (posY), int (fontSize), c);
        end draw;

        procedure Draw_Ex
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
        end Draw_Ex;

        function Measure (text : String; fontSize : int) return int is
            function MeasureText (text : char_array; fontSize : int) return int
            with Import, Convention => C, External_Name => "MeasureText";

            ctext : char_array := To_C (text);
        begin
            return MeasureText (ctext, fontSize);
        end measure;

        function Measure_Ex
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
        end Measure_Ex;

    end Text; ---

    package body Utils is
        procedure Trace_Log (ltype : Log; text : String) is
            procedure TraceLog (ltype : Log; text : char_array);
            pragma import (C, TraceLog, "TraceLog");

            ctext : char_array := To_C (text);
        begin
            TraceLog (ltype, ctext);
        end Trace_Log;
    end Utils;

    package body Input is
        -- Keyboard functions
        function Is_Key_Pressed (key : keys) return Boolean is
            function IsKeyPressed (key : keys) return bool;
            pragma import (C, IsKeyPressed, "IsKeyPressed");
        begin
            return Boolean (IsKeyPressed (key));
        end Is_Key_Pressed;

        function Is_Key_Pressed_Repeat (key : Keys) return Boolean is
            function IsKeyPressedRepeat (key : Keys) return bool;
            pragma import (C, IsKeyPressedRepeat, "IsKeyPressedRepeat");
        begin
            return Boolean (IsKeyPressedRepeat (key));
        end Is_Key_Pressed_Repeat;

        function Is_Key_Down (key : Keys) return Boolean is
            function IsKeyDown (key : Keys) return bool;
            pragma import (C, IsKeyDown, "IsKeyDown");
        begin
            return Boolean (IsKeyDown (key));
        end Is_Key_Down;

        function Is_Key_Released (key : Keys) return Boolean is
            function IsKeyReleased (key : Keys) return bool;
            pragma import (C, IsKeyReleased, "IsKeyReleased");
        begin
            return Boolean (IsKeyReleased (key));
        end Is_Key_Released;

        function Is_Key_Up (key : Keys) return Boolean is
            function IsKeyUp (key : Keys) return bool;
            pragma import (C, IsKeyUp, "IsKeyUp");
        begin
            return Boolean (IsKeyUp (key));
        end Is_Key_Up;

        -- Gamepad functions
        function Is_Gamepad_Available (gamepad : int) return Boolean is
            function IsGamepadAvailable (gamepad : int) return bool;
            pragma import (C, IsGamepadAvailable, "IsGamepadAvailable");
        begin
            return Boolean (IsGamepadAvailable (gamepad));
        end Is_Gamepad_Available;

        function Get_Gamepad_Name (gamepad : int) return String is
            use Interfaces.C.Strings;
            function GetGamepadName (arg1 : int) return chars_ptr
            with Import, Convention => C, External_Name => "GetGamepadName";
            Gamepad_Name : chars_ptr := GetGamepadName (gamepad);
        begin
            return
               (if Gamepad_Name /= Null_Ptr then Value (Gamepad_Name) else "");
        end Get_Gamepad_Name;

        function Is_Gamepad_Button_Pressed
           (gamepad, button : int) return Boolean
        is
            function IsGamepadButtonPressed
               (gamepad, button : int) return bool;
            pragma
               import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");
        begin
            return Boolean (IsGamepadButtonPressed (gamepad, button));
        end Is_Gamepad_Button_Pressed;

        function Is_Gamepad_Button_Down
           (gamepad : int; button : Gamepad_Button) return Boolean
        is
            function IsGamepadButtonDown
               (gamepad : int; button : Gamepad_Button) return bool;
            pragma import (C, IsGamepadButtonDown, "IsGamepadButtonDown");
        begin
            return Boolean (IsGamepadButtonDown (gamepad, button));
        end Is_Gamepad_Button_Down;

        function Is_Gamepad_Button_Released
           (gamepad : int; button : Gamepad_Button) return Boolean
        is
            function IsGamepadButtonReleased
               (gamepad : int; button : Gamepad_Button) return bool;
            pragma
               import (C, IsGamepadButtonReleased, "IsGamepadButtonReleased");
        begin
            return Boolean (IsGamepadButtonReleased (gamepad, button));
        end Is_Gamepad_Button_Released;

        function Is_Gamepad_Button_Up
           (gamepad : int; button : Gamepad_Button) return Boolean
        is
            function IsGamepadButtonUp
               (gamepad : int; button : Gamepad_Button) return bool;
            pragma import (C, IsGamepadButtonUp, "IsGamepadButtonUp");
        begin
            return Boolean (IsGamepadButtonUp (gamepad, button));
        end Is_Gamepad_Button_Up;

        -- Mouse functions
        function Is_Mouse_Button_Pressed (button : Mouse_Button) return Boolean
        is
            function IsMouseButtonPressed (button : Mouse_Button) return bool;
            pragma import (C, IsMouseButtonPressed, "IsMouseButtonPressed");
        begin
            return Boolean (IsMouseButtonPressed (button));
        end Is_Mouse_Button_Pressed;

        function Is_Mouse_Button_Down (button : Mouse_Button) return Boolean is
            function IsMouseButtonDown (button : Mouse_Button) return bool;
            pragma import (C, IsMouseButtonDown, "IsMouseButtonDown");
        begin
            return Boolean (IsMouseButtonDown (button));
        end Is_Mouse_Button_Down;

        function Is_Mouse_Button_Released
           (button : Mouse_Button) return Boolean
        is
            function IsMouseButtonReleased (button : Mouse_Button) return bool;
            pragma import (C, IsMouseButtonReleased, "IsMouseButtonReleased");
        begin
            return Boolean (IsMouseButtonReleased (button));
        end Is_Mouse_Button_Released;

        function Is_Mouse_Button_Up (button : Mouse_Button) return Boolean is
            function IsMouseButtonUp (button : Mouse_Button) return bool;
            pragma import (C, IsMouseButtonUp, "IsMouseButtonUp");
        begin
            return Boolean (IsMouseButtonUp (button));
        end Is_Mouse_Button_Up;

    end Input;

    package body Shapes is
        -- Collision detection functions
        function Check_Collision_Recs (rec1, rec2 : Rectangle) return Boolean
        is
            function CheckCollisionRecs (rec1, rec2 : Rectangle) return bool;
            pragma import (C, CheckCollisionRecs, "CheckCollisionRecs");
        begin
            return Boolean (CheckCollisionRecs (rec1, rec2));
        end Check_Collision_Recs;

        function Check_Collision_Circles
           (center1 : Vector2;
            radius1 : Float;
            center2 : Vector2;
            radius2 : Float) return Boolean
        is
            function CheckCollisionCircles
               (center1 : Vector2;
                radius1 : Float;
                center2 : Vector2;
                radius2 : Float) return bool;
            pragma import (C, CheckCollisionCircles, "CheckCollisionCircles");
        begin
            return
               Boolean
                  (CheckCollisionCircles (center1, radius1, center2, radius2));
        end Check_Collision_Circles;

        function Check_Collision_Circle_Rec
           (center : Vector2; radius : Float; rec : Rectangle) return Boolean
        is
            function CheckCollisionCircleRec
               (center : Vector2; radius : Float; rec : Rectangle) return bool;
            pragma
               import (C, CheckCollisionCircleRec, "CheckCollisionCircleRec");
        begin
            return Boolean (CheckCollisionCircleRec (center, radius, rec));
        end Check_Collision_Circle_Rec;

        function Check_Collision_Point_Rec
           (point : Vector2; rec : Rectangle) return Boolean
        is
            function CheckCollisionPointRec
               (point : Vector2; rec : Rectangle) return bool;
            pragma
               import (C, CheckCollisionPointRec, "CheckCollisionPointRec");
        begin
            return Boolean (CheckCollisionPointRec (point, rec));
        end Check_Collision_Point_Rec;

        function Check_Collision_Point_Circle
           (point, center : Vector2; radius : Float) return Boolean
        is
            function CheckCollisionPointCircle
               (point, center : Vector2; radius : Float) return bool;
            pragma
               import
                  (C, CheckCollisionPointCircle, "CheckCollisionPointCircle");
        begin
            return Boolean (CheckCollisionPointCircle (point, center, radius));
        end Check_Collision_Point_Circle;

        function Check_Collision_Point_Triangle
           (point, p1, p2, p3 : Vector2) return Boolean
        is
            function CheckCollisionPointTriangle
               (point, p1, p2, p3 : Vector2) return bool;
            pragma
               import
                  (C,
                   CheckCollisionPointTriangle,
                   "CheckCollisionPointTriangle");
        begin
            return Boolean (CheckCollisionPointTriangle (point, p1, p2, p3));
        end Check_Collision_Point_Triangle;

        function Check_Collision_Point_Poly
           (point : Vector2; points : access Vector2; point_count : int)
            return Boolean
        is
            function CheckCollisionPointPoly
               (point : Vector2; points : access Vector2; point_count : int)
                return bool;
            pragma
               import (C, CheckCollisionPointPoly, "CheckCollisionPointPoly");
        begin
            return
               Boolean (CheckCollisionPointPoly (point, points, point_count));
        end Check_Collision_Point_Poly;

        function Check_Collision_Lines
           (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
            collision_point                            : access Vector2)
            return Boolean
        is
            function CheckCollisionLines
               (start_pos1, end_pos1, start_pos2, end_pos2 : Vector2;
                collision_point                            : access Vector2)
                return bool;
            pragma import (C, CheckCollisionLines, "CheckCollisionLines");
        begin
            return
               Boolean
                  (CheckCollisionLines
                      (start_pos1,
                       end_pos1,
                       start_pos2,
                       end_pos2,
                       collision_point));
        end Check_Collision_Lines;

        function Check_Collision_Point_Line
           (point, p1, p2 : Vector2; threshold : int) return Boolean
        is
            function CheckCollisionPointLine
               (point, p1, p2 : Vector2; threshold : int) return bool;
            pragma
               import (C, CheckCollisionPointLine, "CheckCollisionPointLine");
        begin
            return
               Boolean (CheckCollisionPointLine (point, p1, p2, threshold));
        end Check_Collision_Point_Line;

    end Shapes;

    package body RCamera is
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

    end RCamera;

end Raylib;
