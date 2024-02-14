with Interfaces.C;
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body raylib is

   use Interfaces.C;

   function to_boolean (value : int) return Boolean
      is (if value = 0 then False else True);

   ---
   -- Window-related functions
   package body window is

   procedure init (width, height : Positive ; title : string) is
      procedure InitWindow (w, h  : int ; title : strings.Chars_Ptr);
      pragma import (C, InitWindow, "InitWindow");

      ctitle : strings.Chars_Ptr := strings.new_string (title);
   begin
      InitWindow ( int (width), int (height), ctitle);
   end init;

   function should_close return Boolean is
      function WindowShouldClose return int;
      pragma import(C, WindowShouldClose, "WindowShouldClose");

      close : int := WindowShouldClose;
   begin
      return (if close = 0 then FALSE else TRUE);
   end should_close;

   function is_cursor_hidden return Boolean is
      function IsCursorHidden return int;
      pragma import(C, IsCursorHidden, "IsCursorHidden");
      hidden : int := IsCursorHidden;
   begin
      return (if hidden = 0 then FALSE else TRUE);
   end is_cursor_hidden;

   end window;

   package body textures is

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
   procedure DrawText (text : strings.Chars_Ptr; X, Y, fs : int ; c : Color);
   function LoadFont   (fileName : strings.Chars_Ptr) return Font;
   function LoadFontEx (fileName : strings.Chars_Ptr; size : int; chars : access int; glyphCount : int) return Font;
   function LoadFontEx2 (fileName : strings.Chars_Ptr; size : int; chars : char_list; glyphCount : int) return Font;
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

   function load_font_ex (file : String; size : int; chars : access int; glyphCount : int) return Font is
      ctext : strings.Chars_Ptr := strings.new_string (file);
   begin
      return LoadFontEx (ctext, size, chars, glyphCount);
   end load_font_ex;

   function load_font_ex (file : String; size : int; chars : char_list; glyphCount : int) return Font is
      ctext : strings.Chars_Ptr := strings.new_string (file);
   begin
      return LoadFontEx2 (ctext, size, chars, glyphCount);
   end load_font_ex;

   procedure draw (text : String ; posX, posY, fontSize : Int ; c : Color) is
      ctext : strings.Chars_Ptr := strings.new_string (text);
   begin
      DrawText (ctext, int(posX), int(posY), int(fontSize), c);
   end draw;

   procedure draw_ex (F : Font; text : String; position : Vector2; fontSize, spacing : Float; tint : Color) is
      procedure DrawTextEx (
         f :Font ;
         text : strings.Chars_Ptr;
         p : Vector2 ;
         fontSize, spacing : Float;
         tint : Color);
      pragma Import (C, DrawTextEx, "DrawTextEx");

      ctext : strings.Chars_Ptr := strings.new_string (text);
   begin
      DrawTextEx (F, ctext, position, fontSize, spacing, tint);
   end draw_ex;

   function measure (text : String; fontSize : int) return int is
      function MeasureText (text : Strings.chars_ptr; fontSize : int) return int
         with Import, Convention => C, External_Name => "MeasureText";

      ctext : Strings.chars_ptr := Strings.New_String (text);
   begin
      return MeasureText (ctext, fontSize);
   end measure;

   function measure_ex (f : Font; text : String; fontSize, spacing : Float)
      return Vector2
   is
      function MeasureTextEx (f : Font; text : Strings.Chars_Ptr; fontSize, spacing : Float) return Vector2;
      ------
      pragma Import (C, MeasureTextEx, "MeasureTextEx");
      ctext : Strings.Chars_Ptr := Strings.New_String (text);
   begin
      return MeasureTextEx (f, ctext, fontSize, spacing);
   end measure_ex;

   end text; ---

   package body utils is
      procedure trace_log (ltype : Log ; text : String) is
         procedure TraceLog (ltype : Log ; text : strings.Chars_Ptr);
         pragma import (C, TraceLog, "TraceLog");

         ctext : strings.Chars_Ptr := strings.new_string (text);
      begin
         TraceLog (ltype, ctext);
      end trace_log;
   end utils;

   package body input is
      function is_key_pressed (key : keys) return boolean is
         function IsKeyPressed (key : keys) return int;
         pragma import (C, IsKeyPressed, "IsKeyPressed");
         pressed : int := IsKeyPressed (key);
      begin
         return to_boolean (pressed);
      end is_key_pressed;

      function get_gamepad_name (gamepad : int) return string is
         use Interfaces.C.Strings;
         function GetGamepadName (arg1 : int)
            return chars_ptr
         with Import, Convention => C, External_Name => "GetGamepadName";
         Gamepad_Name : chars_ptr := GetGamepadName (gamepad);
      begin
         return (if Gamepad_Name /= Null_Ptr then Value (Gamepad_Name) else "");
      end get_gamepad_name;

   function is_gamepad_button_pressed (gamepad, button : int) return Boolean is
      function IsGamepadButtonPressed (gamepad, button : int) return int;
      pragma import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");
      pressed : int := IsGamepadButtonPressed (gamepad, button);
   begin
      return to_boolean (pressed);
   end is_gamepad_button_pressed;
end input;

end raylib;
