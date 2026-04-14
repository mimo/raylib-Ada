#!/usr/bin/env python3
"""Migrate raylib-ui.adb to Mixed_Case convention"""

import re
from pathlib import Path

REPLACEMENTS = [
    # Functions/procedures
    (r'\bget_state\b', 'Get_State'),
    (r'\bset_state\b', 'Set_State'),
    (r'\block\b', 'Lock'),
    (r'\bunlock\b', 'Unlock'),
    (r'\bget_alpha\b', 'Get_Alpha'),
    (r'\bset_alpha\b', 'Set_Alpha'),
    (r'\bto_text_alignment\b', 'To_Text_Alignment'),
    (r'\bto_unsigned\b', 'To_Unsigned'),
    (r'\bget_property_by_state\b', 'Get_Property_By_State'),
    (r'\bdraw_text\b', 'Draw_Text'),
    (r'\bdraw_border\b', 'Draw_Border'),
    (r'\bdraw_rectangle\b', 'Draw_Rectangle'),
    (r'\bget_property_index\b', 'Get_Property_Index'),
    (r'\bget_text_bounds\b', 'Get_Text_Bounds'),
    (r'\bget_text_width\b', 'Get_Text_Width'),
    (r'\blabel\b', 'Label'),
    (r'\bbutton\b', 'Button'),
    (r'\bcheckbox\b', 'Checkbox'),
    (r'\bpanel\b', 'Panel'),
    (r'\bstatusbar\b', 'Statusbar'),
    (r'\btoggle\b', 'Toggle'),
    (r'\btextbox\b', 'Textbox'),
    (r'\btextbox_multi\b', 'Textbox_Multi'),
    (r'\bis_activated\b', 'Is_Activated'),
    (r'\bset_style\b', 'Set_Style'),
    (r'\bget_style\b', 'Get_Style'),
    (r'\bload_style_default\b', 'Load_Style_Default'),
]

def migrate_file(filepath):
    filepath = Path(filepath)
    if not filepath.exists():
        print(f"Error: {filepath} does not exist")
        return False

    content = filepath.read_text()
    original = content

    for pattern, replacement in REPLACEMENTS:
        content = re.sub(pattern, replacement, content)

    if content != original:
        backup = filepath.with_suffix('.adb.bak')
        filepath.rename(backup)
        filepath.write_text(content)
        print(f"✓ Migrated {filepath}")
        print(f"  Backup: {backup}")
        return True
    else:
        print(f"- No changes needed")
        return False

if __name__ == '__main__':
    migrate_file('src/raylib-ui.adb')
