with interfaces.C;
with interfaces.C.strings;
with System.Address_To_Access_Conversions;

package body raylib is

package Conv is new System.Address_To_Access_Conversions (Camera3D);

   function to_boolean (value : int) return boolean is (if value = 0 then FALSE else TRUE);

   function fade (c : color ; alpha : float) return Color is
      function fade (c : color ; alpha : C_float) return Color;
      pragma import (C, fade, "Fade");
   begin
      return fade (c, C_float (alpha));
   end fade;

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

   function LoadTexture(filename : strings.Chars_Ptr) return Texture2D;
   pragma import (C, LoadTexture, "LoadTexture" );

   function load_texture(filename : String) return Texture2D is
      cfilename : strings.Chars_Ptr := strings.new_string (filename);
   begin
      return LoadTexture (cfilename);
   end load_texture;

end textures;

package body text is

   procedure draw (text : String ; posX, posY, fontSize : Int ; c : Color) is
      procedure DrawText (text : strings.Chars_Ptr; X, Y, fs : int ; c : Color);
      pragma import (C, DrawText, "DrawText");

      ctext : strings.Chars_Ptr := strings.new_string (text);
   begin
      DrawText (ctext, int(posX), int(posY), int(fontSize), c);
   end draw;

end text; ---

package body core is
   function is_gamepad_available (gamepad : int) return boolean is
   function IsGamepadAvailable (gamepad : int) return int;
      pragma import (C, IsGamepadAvailable, "IsGamepadAvailable");
      available : int := IsGamepadAvailable (gamepad);
   begin
      return to_boolean (available);
   end is_gamepad_available;
   function is_gamepad_button_pressed (gamepad, button : int) return Boolean is
      function IsGamepadButtonPressed (gamepad, button : int) return int;
      pragma import (C, IsGamepadButtonPressed, "IsGamepadButtonPressed");
      pressed : int := IsGamepadButtonPressed (gamepad, button);
   begin
     return to_boolean (pressed);
   end is_gamepad_button_pressed;

   procedure trace_log (ltype : Log ; text : String) is
      procedure TraceLog (ltype : Log ; text : strings.Chars_Ptr);
      pragma import (C, TraceLog, "TraceLog");

      ctext : strings.Chars_Ptr := strings.new_string (text);
   begin
      TraceLog (ltype, ctext);
   end trace_log;
end core;

package body camera is
   procedure UpdateCamera (camera : System.address);
   pragma import (C, UpdateCamera, "UpdateCamera");
   procedure update (camera : P_Camera) is
   begin
      UpdateCamera (conv.to_address (conv.object_pointer(camera)));
   end update;
end camera;

end raylib;
