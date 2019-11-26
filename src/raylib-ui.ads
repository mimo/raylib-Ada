with raylib;
with Interfaces.C;

package raylib.UI is

  ---
  -- UI state
  type Controls is (DEFAULT, LABEL, BUTTON, TOGGLE, SLIDER, PROGRESSBAR);
  type Properties is (
    BORDER_COLOR_NORMAL,
    BASE_COLOR_NORMAL,
    TEXT_COLOR_NORMAL,
    BORDER_COLOR_FOCUSED,
    BASE_COLOR_FOCUSED,
    TEXT_COLOR_FOCUSED,
    BORDER_COLOR_PRESSED,
    BASE_COLOR_PRESSED,
    TEXT_COLOR_PRESSED,
    BORDER_COLOR_DISABLED,
    BASE_COLOR_DISABLED,
    TEXT_COLOR_DISABLED,
    BORDER_WIDTH,
    TEXT_PADDING,
    TEXT_ALIGNEMENT,
    RESERVED);

   function Button (bounds : raylib.Rectangle ; label : String) return Boolean;

private

   type property_element is (BORDER, BASE, TEXT, OTHER);

   procedure load_style_default;
   function get_style (control : Controls ; property : Properties) return unsigned;

end raylib.UI;
