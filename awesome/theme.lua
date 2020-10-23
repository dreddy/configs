
-- {{{ Main
theme = {}
theme.default_themes_path = "/usr/share/awesome/themes"
theme.wallpaper = os.getenv("HOME") .. "/.config/awesome/bg.png"
theme.colors = {}
theme.colors.base4   = "#e48822ff"
theme.colors.base3   = "#1e1e1eff"
theme.colors.base2   = "#073642ff"
theme.colors.base1   = "#586e75ff"
theme.colors.base0   = "#859ba3ff"
-- }}}

-- {{{ Styles
theme.font		= "Sans Oblique 10"
theme.taglist_font      = "Sans Oblique 10"

-- {{{ Colors
theme.fg_normal  = theme.colors.base0
theme.fg_focus   = theme.colors.base4
theme.fg_urgent  = theme.colors.base3

theme.bg_normal  = theme.colors.base3
theme.bg_focus   = theme.colors.base3
theme.bg_urgent  = theme.colors.base4
theme.bg_systray = theme.bg_normal
-- }}}

-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = theme.colors.base2
theme.border_focus  = theme.bg_urgent
theme.border_marked = theme.bg_normal
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal
-- }}}

-- {{{ Mouse finder
theme.mouse_finder_color = theme.colors.base2
-- mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Menu
-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_height = "20"
theme.menu_width  = "120"
theme.menu_font  = "Sans Oblique 10"
-- }}}

-- {{{ Icons
-- {{{ Taglist
theme.taglist_squares_sel   = theme.default_themes_path.."/zenburn/taglist/squarefz.png"
theme.taglist_squares_unsel = theme.default_themes_path.."/zenburn/taglist/squarez.png"
theme.taglist_squares_resize = "false"
-- }}}

theme.tasklist_disable_icon = "true"
-- {{{ Misc
theme.awesome_icon           = theme.default_themes_path.."/zenburn/awesome-icon.png"
theme.menu_submenu_icon      = theme.default_themes_path.."/default/submenu.png"
-- }}}



theme.useless_gap = 25

-- }}}

return theme
