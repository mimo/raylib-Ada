---
-- Window-related functions

with Interfaces.C; use Interfaces.C;

package body Raylib.Window is

    use type int;

    procedure Init (Width, Height : Positive; Title : String) is
        procedure InitWindow (W, H : int; Title : char_array)
        with Import, Convention => C, External_Name => "InitWindow";

        C_Title : char_array := To_C (Title);
    begin
        InitWindow (int (Width), int (Height), C_Title);
    end Init;

    function Should_Close return Boolean is
        function WindowShouldClose return int
        with Import, Convention => C, External_Name => "WindowShouldClose";
    begin
        return WindowShouldClose /= 0;
    end Should_Close;

    function Is_Window_Ready return Boolean is
        function IsWindowReady return int
        with Import, Convention => C, External_Name => "IsWindowReady";
    begin
        return IsWindowReady /= 0;
    end Is_Window_Ready;

    function Is_Window_Fullscreen return Boolean is
        function IsWindowFullscreen return int
        with Import, Convention => C, External_Name => "IsWindowFullscreen";
    begin
        return IsWindowFullscreen /= 0;
    end Is_Window_Fullscreen;

    function Is_Window_Hidden return Boolean is
        function IsWindowHidden return int
        with Import, Convention => C, External_Name => "IsWindowHidden";
    begin
        return IsWindowHidden /= 0;
    end Is_Window_Hidden;

    function Is_Window_Minimized return Boolean is
        function IsWindowMinimized return int
        with Import, Convention => C, External_Name => "IsWindowMinimized";
    begin
        return IsWindowMinimized /= 0;
    end Is_Window_Minimized;

    function Is_Window_Maximized return Boolean is
        function IsWindowMaximized return int
        with Import, Convention => C, External_Name => "IsWindowMaximized";
    begin
        return IsWindowMaximized /= 0;
    end Is_Window_Maximized;

    function Is_Window_Focused return Boolean is
        function IsWindowFocused return int
        with Import, Convention => C, External_Name => "IsWindowFocused";
    begin
        return IsWindowFocused /= 0;
    end Is_Window_Focused;

    function Is_Window_Resized return Boolean is
        function IsWindowResized return int
        with Import, Convention => C, External_Name => "IsWindowResized";
    begin
        return IsWindowResized /= 0;
    end Is_Window_Resized;

    function Is_Window_State (Flag : unsigned) return Boolean is
        function IsWindowState (Flag : unsigned) return int
        with Import, Convention => C, External_Name => "IsWindowState";
    begin
        return IsWindowState (Flag) /= 0;
    end Is_Window_State;

    procedure Set_Window_Title (Title : String) is
        procedure C_Set_Window_Title (Title : char_array)
        with Import, Convention => C, External_Name => "SetWindowTitle";

        C_Title : char_array := To_C (Title);
    begin
        C_Set_Window_Title (C_Title);
    end Set_Window_Title;

    procedure Set_Clipboard_Text (Text : String) is
        procedure C_Set_Clipboard_Text (Text : char_array)
        with Import, Convention => C, External_Name => "SetClipboardText";

        C_Text : char_array := To_C (Text);
    begin
        C_Set_Clipboard_Text (C_Text);
    end Set_Clipboard_Text;

    function Get_Clipboard_Text return String is
        use Interfaces.C.Strings;

        function C_Get_Clipboard_Text return chars_ptr
        with Import, Convention => C, External_Name => "GetClipboardText";

        Result_Ptr : constant chars_ptr := C_Get_Clipboard_Text;
    begin
        return (if Result_Ptr /= Null_Ptr then Value (Result_Ptr) else "");
    end Get_Clipboard_Text;

    function Is_Cursor_Hidden return Boolean is
        function IsCursorHidden return int
        with Import, Convention => C, External_Name => "IsCursorHidden";
    begin
        return IsCursorHidden /= 0;
    end Is_Cursor_Hidden;

    function Is_Cursor_On_Screen return Boolean is
        function IsCursorOnScreen return int
        with Import, Convention => C, External_Name => "IsCursorOnScreen";
    begin
        return IsCursorOnScreen /= 0;
    end Is_Cursor_On_Screen;

    procedure Take_Screenshot (Filename : String) is
        procedure C_Take_Screenshot (Filename : char_array)
        with Import, Convention => C, External_Name => "TakeScreenshot";

        C_Filename : char_array := To_C (Filename);
    begin
        C_Take_Screenshot (C_Filename);
    end Take_Screenshot;

    procedure Open_URL (URL : String) is
        procedure C_Open_URL (URL : char_array)
        with Import, Convention => C, External_Name => "OpenURL";

        C_URL : char_array := To_C (URL);
    begin
        C_Open_URL (C_URL);
    end Open_URL;

end Raylib.Window;
