pub mod terminal_sequences {
  // Keyboard keys
  pub const CTRL_C: &str = "\u{3}";
  pub const CTRL_LEFT: &str = "\u{1B}[1;5D";
  pub const CTRL_L: &str = "\u{C}";
  pub const CTRL_RIGHT: &str = "\u{1B}[1;5C";
  pub const DOWN: &str = "\u{1B}[B";
  pub const DELETE: &str = "\u{1B}[3~";
  pub const END: &str = "\u{1B}[F";
  pub const HOME: &str = "\u{1B}[H";
  pub const INSERT: &str = "\u{1B}[2~";
  pub const LEFT: &str = "\u{1B}[D";
  pub const RIGHT: &str = "\u{1B}[C";
  pub const UP: &str = "\u{1B}[A";
  pub const PAGE_UP: &str = "\u{1B}[5~";
  pub const PAGE_DOWN: &str = "\u{1B}[6~";

  // xterm.js Terminal Sequences
  // See https://xtermjs.org/docs/api/vtfeatures/ for missing documentation (`SP` is ` `)
  // See https://vt100.net/docs/vt510-rm/chapter4.html for more information on terminal sequences
  // See https://notes.burke.libbey.me/ansi-escape-codes/ for more information on terminal sequences

  // Constant sequences (no parameters `Ps`, `Pm` or `Pt`)
  // C0
  /// NUL is ignored.
  pub const NUL: &str = "\u{00}";
  /// Ring the bell.
  /// The behavior of the bell is further customizable with https://docs.rs/xterm-js-rs/0.1.1/xterm_js_rs/xterm/struct.TerminalOptions.html#method.set_bell_style and https://docs.rs/xterm-js-rs/0.1.1/xterm_js_rs/xterm/struct.TerminalOptions.html#method.set_bell_sound.
  pub const BEL: &str = "\u{07}";
  /// Move the cursor one position to the left.
  /// By default it is not possible to move the cursor past the leftmost
  /// position. If `reverse wrap-around` (`CSI ? 45 h`) is set, a previous soft
  /// line wrap (DECAWM) can be undone with BS within the scroll margins. In
  /// that case the cursor will wrap back to the end of the previous row. Note
  /// that it is not possible to peek back into the scrollbuffer with the
  /// cursor, thus at the home position (top-leftmost cell) this has no effect.
  pub const BS: &str = "\u{08}";
  /// Move the cursor to the next character tab stop.
  pub const HT: &str = "\u{09}";
  /// Move the cursor one row down, scrolling if needed.
  /// Scrolling is restricted to scroll margins and will only happen on the
  /// bottom line.
  pub const LF: &str = "\u{0A}";
  /// Treated as LF.
  pub const VT: &str = "\u{0B}";
  /// Treated as LF.
  pub const FF: &str = "\u{0C}";
  /// Move the cursor to the beginning of the row.
  pub const CR: &str = "\u{0D}";
  /// Switch to an alternative character set.
  pub const SO: &str = "\u{0E}";
  /// Return to regular character set after Shift Out.
  pub const SI: &str = "\u{0F}";
  /// Start of a sequence. Cancels any other sequence.
  pub const ESC: &str = "\u{1B}";

  // C1
  /// Move the cursor one line down scrolling if needed.
  pub const IND: &str = "\u{84}";
  /// Move the cursor to the beginning of the next row.
  pub const NEL: &str = "\u{85}";
  /// Places a tab stop at the current cursor position.
  pub const HTS: &str = "\u{88}";
  /// Start of a DCS sequence.
  pub const DCS: &str = "\u{90}";
  /// Start of a CSI sequence.
  pub const CSI: &str = "\u{9B}";
  /// Terminator used for string type sequences.
  pub const ST: &str = "\u{9C}";
  /// Start of an OSC sequence.
  pub const OSC: &str = "\u{9D}";
  /// Start of a privacy message.
  pub const PM: &str = "\u{9E}";
  /// Start of an APC sequence.
  pub const APC: &str = "\u{9F}";

  // CSI
  /// Send primary device attributes.
  pub const DA1: &str = "\u{9B}c";
  /// Send secondary device attributes.
  pub const DA2: &str = "\u{9B}>c";
  /// Reset several terminal attributes to initial state.
  ///
  /// There are two terminal reset sequences - RIS and DECSTR. While RIS
  /// performs almost a full terminal bootstrap, DECSTR only resets certain
  /// attributes. For most needs DECSTR should be sufficient.
  ///
  /// The following terminal attributes are reset to default values:
  ///
  /// * IRM is reset (dafault = false)
  /// * scroll margins are reset (default = viewport size)
  /// * erase attributes are reset to default
  /// * charsets are reset
  /// * DECSC data is reset to initial values
  /// * DECOM is reset to absolute mode
  pub const DECSTR: &str = "\u{9B}!p";
  /// Save cursor position, charmap and text attributes.
  pub const SCOSC: &str = "\u{9B}s";
  /// Save cursor position, charmap and text attributes.\
  pub const SCORC: &str = "\u{9B}u";

  // ESC
  /// Save cursor position, charmap and text attributes.
  pub const SC: &str = "\u{1B}7";
  /// Restore cursor position, charmap and text attributes.
  pub const RC: &str = "\u{1B}8";
  /// Fill viewport with a test pattern (E).
  pub const DECALN: &str = "\u{1B}#8";
  /// Move the cursor one line up scrolling if needed.
  pub const IR: &str = "\u{1B}M";

  // Parameterised sequences (accepts parameters ``ps``, `Pm` or `Pt`)
  // CSI
  /// ICH: Insert `ps` (blank) characters (default = 1).
  /// The ICH sequence inserts `ps` blank characters. The cursor remains at the
  /// beginning of the blank characters. Text between the cursor and right
  /// margin moves to the right. Characters moved past the right margin are
  /// lost.
  pub fn insert_characters(ps: usize) -> String {
    String::from(format!("{}{}@", CSI, ps))
  }

  /// SL: Scroll viewport `ps` times to the left.
  /// Moves the content of all lines within the scroll margins `ps` times to the
  /// left. Has no effect outside of the scroll margins.
  pub fn scroll_left(ps: usize) -> String {
    String::from(format!("{}{} @", CSI, ps))
  }

  /// CUU: Move cursor `ps` times up (default=1).
  /// If the cursor would pass the top scroll margin, it will stop there.
  pub fn cursor_up(ps: usize) -> String {
    String::from(format!("{}{}A", CSI, ps))
  }

  /// SR: Scroll viewport `ps` times to the right.
  /// Moves the content of all lines within the scroll margins `ps` times to the
  /// right. Content at the right margin is lost. Has no effect outside of the
  /// scroll margins.
  pub fn scroll_right(ps: usize) -> String {
    String::from(format!("{}{} A", CSI, ps))
  }

  /// CUD: Move cursor `ps` times down (default=1).
  /// If the cursor would pass the bottom scroll margin, it will stop there.
  pub fn cursor_down(ps: usize) -> String {
    String::from(format!("{}{}B", CSI, ps))
  }

  /// CUF: Move cursor `ps` times forward (default=1).
  pub fn cursor_forwards(ps: usize) -> String {
    String::from(format!("{}{}C", CSI, ps))
  }

  /// CUB: Move cursor `ps` times backward (default=1).
  pub fn cursor_backwards(ps: usize) -> String {
    String::from(format!("{}{}D", CSI, ps))
  }

  /// CNL: Move cursor `ps` times down (default=1) and to the first column.
  /// Same as CUD, additionally places the cursor at the first column.
  pub fn cursor_next_line(ps: usize) -> String {
    String::from(format!("{}{}E", CSI, ps))
  }

  /// CPL: Move cursor `ps` times up (default=1) and to the first column.
  /// Same as CUU, additionally places the cursor at the first column.
  pub fn cursor_previous_line(ps: usize) -> String {
    String::from(format!("{}{}F", CSI, ps))
  }

  /// CHA: Move cursor to `ps`-th column of the active row (default=1).
  pub fn cursor_horizontal_absolute(ps: usize) -> String {
    String::from(format!("{}{}G", CSI, ps))
  }

  /// CUP: Set cursor to position [`ps_a`, `ps_b`] (default = [1, 1]).
  /// If ORIGIN mode is set, places the cursor to the absolute position within
  /// the scroll margins. If ORIGIN mode is not set, places the cursor to the
  /// absolute position within the viewport. Note that the coordinates are
  /// 1-based, thus the top left position starts at 1 ; 1.
  pub fn cursor_position(ps_a: usize, ps_b: usize) -> String {
    String::from(format!("{}{};{}H", CSI, ps_a, ps_b))
  }

  /// CHT: Move cursor `ps` times tabs forward (default=1).
  pub fn cursor_horizontal_tabulation(ps: usize) -> String {
    String::from(format!("{}{}I", CSI, ps))
  }

  /// ED: Erase various parts of the viewport.
  /// `ps` effects:
  /// 0: Erase from the cursor through the end of the viewport.
  /// 1: Erase from the beginning of the viewport through the cursor.
  /// 2: Erase complete viewport.
  /// 3: Erase scrollback.
  pub fn erase_in_display(ps: usize) -> String {
    String::from(format!("{}{}J", CSI, ps))
  }

  /// DECSED: Currently the same as ED.
  pub fn selective_erase_in_display(ps: usize) -> String {
    String::from(format!("{}?{}J", CSI, ps))
  }

  /// EL: Erase various parts of the active row.
  /// `ps` effects:
  /// 0: Erase from the cursor through the end of the row.
  /// 1: Erase from the beginning of the line through the cursor.
  /// 2: Erase complete line.
  pub fn erase_in_line(ps: usize) -> String {
    String::from(format!("{}{}K", CSI, ps))
  }

  /// DECSEL: Currently the same as EL.
  pub fn selective_erase_in_line(ps: usize) -> String {
    String::from(format!("{}?{}K", CSI, ps))
  }

  /// IL: Insert `ps` blank lines at active row (default=1).
  /// For every inserted line at the scroll top one line at the scroll bottom
  /// gets removed. The cursor is set to the first column. IL has no effect if
  /// the cursor is outside the scroll margins.
  pub fn insert_lines(ps: usize) -> String {
    String::from(format!("{}{}L", CSI, ps))
  }

  /// DL: Delete `ps` lines at active row (default=1).
  /// For every deleted line at the scroll top one blank line at the scroll
  /// bottom gets appended. The cursor is set to the first column. DL has no
  /// effect if the cursor is outside the scroll margins.
  pub fn delete_lines(ps: usize) -> String {
    String::from(format!("{}{}M", CSI, ps))
  }

  /// DCH: Delete `ps` characters (default=1).
  /// As characters are deleted, the remaining characters between the cursor and
  /// right margin move to the left. Character attributes move with the
  /// characters. The terminal adds blank characters at the right margin.
  pub fn delete_characters(ps: usize) -> String {
    String::from(format!("{}{}P", CSI, ps))
  }

  /// SU: Scroll `ps` lines up (default=1).
  pub fn scroll_up(ps: usize) -> String {
    String::from(format!("{}{}S", CSI, ps))
  }

  /// SD: Scroll `ps` lines down (default=1).
  pub fn scroll_down(ps: usize) -> String {
    String::from(format!("{}{}T", CSI, ps))
  }

  /// ECH: Erase `ps` characters from current cursor position to the right
  /// (default=1). ED erases `ps` characters from current cursor position to
  /// the right. ED works inside or outside the scrolling margins.
  pub fn erase_characters(ps: usize) -> String {
    String::from(format!("{}{}X", CSI, ps))
  }

  /// CBT: Move cursor `ps` tabs backward (default=1).
  pub fn cursor_backward_tabulation(ps: usize) -> String {
    String::from(format!("{}{}Z", CSI, ps))
  }

  /// HPA: Same as CHA.
  pub fn horizontal_position_absolute(ps: usize) -> String {
    String::from(format!("{}{}`", CSI, ps))
  }

  /// HPR: Same as CUF.
  pub fn horizontal_position_relative(ps: usize) -> String {
    String::from(format!("{}{}a", CSI, ps))
  }

  /// REP: Repeat preceding character `ps` times (default=1).
  /// Repeats the previous character `ps` times advancing the cursor, also
  /// wrapping if DECAWM is set. Has no effect if the sequence does not follow a
  /// printable ASCII character (NOOP for any other sequence in between or NON
  /// ASCII characters).
  pub fn repeat_preceding_character(ps: usize) -> String {
    String::from(format!("{}{}b", CSI, ps))
  }

  /// VPA: Move cursor to `ps`-th row (default=1).
  pub fn vertical_position_absolute(ps: usize) -> String {
    String::from(format!("{}{}d", CSI, ps))
  }

  /// VPR: Move cursor `ps` times down (default=1).
  pub fn vertical_position_relative(ps: usize) -> String {
    String::from(format!("{}{}e", CSI, ps))
  }

  /// HVP: Same as CUP.
  pub fn horizontal_and_vertical_position(ps_a: usize, ps_b: usize) -> String {
    String::from(format!("{}{};{}f", CSI, ps_a, ps_b))
  }

  /// TBC: Clear tab stops at current position (0) or all (3) (default=0).
  /// Clearing tabstops off the active row (`ps` = 2, VT100) is currently not
  /// supported.
  pub fn tab_clear(ps: usize) -> String {
    String::from(format!("{}{}g", CSI, ps))
  }

  /// SM: Set various terminal modes.
  /// Supported param values:
  /// 4: Insert Mode (IRM).
  pub fn set_mode(ps: Vec<usize>) -> String {
    if ps.len() == 0 {
      return String::from(format!("{}h", CSI));
    }
    let (mut out, max_index) = (String::from(CSI), ps.len() - 1);
    for (index, param) in ps.into_iter().enumerate() {
      if index < max_index {
        out.push_str(&format!("{};", param));
      }
      else {
        out.push_str(&format!("{}h", param));
      }
    }
    out
  }

  /// DECSET: Set various terminal attributes.
  /// Supported param values:
  /// 1: Application Cursor Keys (DECCKM).
  /// 2: Designate US-ASCII for character sets G0-G3 (DECANM).
  /// 3: 132 Column Mode (DECCOLM).
  /// 6: Origin Mode (DECOM).
  /// 7: Auto-wrap Mode (DECAWM).
  /// 9: X10 xterm mouse protocol.
  /// 12: Start Blinking Cursor.
  /// 25: Show Cursor (DECTCEM).
  /// 45: Reverse wrap-around.
  /// 47: Use Alternate Screen Buffer.
  /// 66: Application keypad (DECNKM).
  /// 1000: X11 xterm mouse protocol.
  /// 1002: Use Cell Motion Mouse Tracking.
  /// 1003: Use All Motion Mouse Tracking.
  /// 1004: Send FocusIn/FocusOut events.
  /// 1006: Enable SGR Mouse Mode.
  /// 1047: Use Alternate Screen Buffer.
  /// 1048: Save cursor as in DECSC.
  /// 1049: Save cursor and switch to alternate buffer clearing it.
  /// 2004: Set bracketed paste mode.
  pub fn dec_private_set_mode(ps: Vec<usize>) -> String {
    if ps.len() == 0 {
      return String::from(format!("{}?h", CSI));
    }
    let (mut out, max_index) =
      (String::from(format!("{}?", CSI)), ps.len() - 1);
    for (index, param) in ps.into_iter().enumerate() {
      if index < max_index {
        out.push_str(&format!("{};", param));
      }
      else {
        out.push_str(&format!("{}h", param));
      }
    }
    out
  }

  /// RM: Reset various terminal modes.
  /// Supported param values:
  /// 4: Replace Mode (IRM). (default)
  pub fn reset_mode(ps: Vec<usize>) -> String {
    if ps.len() == 0 {
      return String::from(format!("{}l", CSI));
    }
    let (mut out, max_index) = (String::from(CSI), ps.len() - 1);
    for (index, param) in ps.into_iter().enumerate() {
      if index < max_index {
        out.push_str(&format!("{};", param));
      }
      else {
        out.push_str(&format!("{}l", param));
      }
    }
    out
  }

  /// DECRST: Reset various terminal attributes.
  /// Supported param values:
  /// 1: Normal Cursor Keys (DECCKM).
  /// 6: Normal Cursor Mode (DECOM).
  /// 7: No Wraparound Mode (DECAWM).
  /// 9: Don’t send Mouse X & Y on button press.
  /// 12: Stop Blinking Cursor.
  /// 25: Hide Cursor (DECTCEM).
  /// 45: No reverse wrap-around.
  /// 47: Use Normal Screen Buffer.
  /// 66: Numeric keypad (DECNKM).
  /// 1000: Don’t send Mouse reports.
  /// 1002: Don’t use Cell Motion Mouse Tracking.
  /// 1003: Don’t use All Motion Mouse Tracking.
  /// 1004: Don’t send FocusIn/FocusOut events.
  /// 1006: Disable SGR Mouse Mode.
  /// 1047: Use Normal Screen Buffer (clearing screen if in alt).
  /// 1048: Restore cursor as in DECRC.
  /// 1049: Use Normal Screen Buffer and restore cursor.
  /// 2004: Reset bracketed paste mode.
  pub fn dec_private_reset_mode(ps: Vec<usize>) -> String {
    if ps.len() == 0 {
      return String::from(format!("{}?l", CSI));
    }
    let (mut out, max_index) =
      (String::from(format!("{}?", CSI)), ps.len() - 1);
    for (index, param) in ps.into_iter().enumerate() {
      if index < max_index {
        out.push_str(&format!("{};", param));
      }
      else {
        out.push_str(&format!("{}l", param));
      }
    }
    out
  }

  /// DSR: Request cursor position (CPR) with `ps` = 6.
  pub fn device_status_report(ps: usize) -> String {
    String::from(format!("{}{}n", CSI, ps))
  }

  /// DECDSR: Only CPR is supported (same as DSR).
  pub fn dec_device_status_report(ps: usize) -> String {
    String::from(format!("{}?{}n", CSI, ps))
  }

  /// DECSCUSR: Set cursor style.
  /// Supported param values:
  /// 0 or 1: steady block
  /// 2: blink block
  /// 3: steady underline
  /// 4: blink underline
  /// 5: steady bar
  /// 6: blink bar
  pub fn set_cursor_style(ps: usize) -> String {
    String::from(format!("{}{} q", CSI, ps))
  }

  /// DECSTBM: Set top and bottom margins of the viewport [top;bottom] (default
  /// = viewport size).
  pub fn set_top_and_bottom_margin(ps_a: usize, ps_b: usize) -> String {
    String::from(format!("{}{};{}r", CSI, ps_a, ps_b))
  }

  /// DECIC: Insert `ps` columns at cursor position.
  /// Inserts `ps` times blank columns at the cursor position for all lines with
  /// the scroll margins, moving content to the right. Content at the right
  /// margin is lost. Has no effect outside the scrolling margins.
  pub fn insert_columns(ps: usize) -> String {
    String::from(format!("{}{}'}}", CSI, ps))
  }

  /// DECDC: Delete `ps` columns at cursor position.
  /// Deletes `ps` times columns at the cursor position for all lines with the
  /// scroll margins, moving content to the left. Blank columns are added at the
  /// right margin. Has no effect outside the scrolling margins.
  pub fn delete_columns(ps: usize) -> String {
    String::from(format!("{}{}'~", CSI, ps))
  }

  /// 2: Set window title.
  /// xterm.js does not manipulate the title directly, instead exposes changes
  /// via the event `Terminal.onTitleChange`.
  pub fn set_window_title(pt: String) -> String {
    String::from(format!("{}2;{}{}", OSC, pt, BEL))
  }
}
