-- should be in "C:\Users\XXXXXX\.config\wezterm"
-- In vim:
--   :!yes | cp % /mnt/c/Users/XXXXXX/.config/wezterm/wezterm.lua

local wezterm = require "wezterm"
local act = wezterm.action

local launch_menu = {}

table.insert(launch_menu, { label = "PowerShell", args = { "powershell.exe", "-NoLogo" } })
table.insert(launch_menu, { label = "cmd.exe",    args = { "cmd.exe" } })

local c = {

    col_ddk = "#18262F",
    col_dbk = "#38464F",
    col_ddr = "#EF5253",
    col_dbr = "#FF6263",
    col_ddg = "#7CC844",
    col_dbg = "#8CD854",
    col_ddy = "#E4B51C",
    col_dby = "#F4C52C",
    col_ddb = "#33B5E1",
    col_dbb = "#43C5F1",
    col_ddm = "#A363D5",
    col_dbm = "#B373E5",
    col_ddc = "#52CBB0",
    col_dbc = "#62DBC0",
    col_ddw = "#C6CfD8",
    col_dbw = "#D6DFD8",

    col_bdk = "#f8f4f4",
    col_bbk = "#f8f4f4",
    col_bdr = "#e22",
    col_bbr = "#f33",
    col_bdg = "#060",
    col_bbg = "#282",
    col_bdy = "#c07700",
    col_bby = "#c88018",
    col_bdb = "#11a",
    col_bbb = "#33c",
    col_bdm = "#a59",
    col_bbm = "#b6a",
    col_bdc = "#088",
    col_bbc = "#0aa",
    col_bdw = "#111",
    col_bbw = "#111",

}

local ansi    = { c.col_bdk, c.col_bdr, c.col_bdg, c.col_bdy, c.col_bdb, c.col_bdm, c.col_bdc, c.col_bdw }
local brights = { c.col_bdk, c.col_bdr, c.col_bdg, c.col_bdy, c.col_bdb, c.col_bdm, c.col_bdc, c.col_bdw }

local mycolors = {
    foreground="#444", background="rgba(245,245,245,91%)",
    cursor_fg="#fff", cursor_bg="#000",
    ansi=ansi, brights=brights,
    indexed = { [238] = "#d0cccc", [236] = "#c0bcbc" },
    -- indexed = { [238] = "#444444", [236] = "#303030" },
    -- compose_cursor = c.col_bdg,
}

return {
  -- font = wezterm.font "Droid Sans Mono",
  font_size = 10.5, cell_width = 0.9, line_height = 1.06,
  -- font_size = 10.5, cell_width = 0.55, line_height = 1,
  font = wezterm.font_with_fallback {
    -- "UnifontMyTheBold",
    "Droid Sans Mono",
    "Droid Sans Japanese",
    -- "Yu Gothic Medium",
  },

  -- color_scheme = "Solar Flare (base16)", colors = { background="rgba(10,15,20,83%)" },
  -- color_scheme = "Solar Flare Light (base16)",
  colors = mycolors,

  hide_tab_bar_if_only_one_tab = true,

  default_prog = { "wsl.exe", "--distribution", "Arch", "--cd", "~" },
  launch_menu = launch_menu,

  keys = {
      { key = "t",   mods = "SHIFT|CTRL", action = "ShowLauncher" },
      { key = "Tab", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(1) },
  },

  check_for_updates = false,
  window_close_confirmation = "NeverPrompt",

}

