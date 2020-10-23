
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- Config Settings
config = {}
config.global = {
   theme    = awful.util.getdir("config") .. "/theme.lua",
   terminal = "urxvt",
   editor   = os.getenv("EDITOR") or "emacs",
   browser  = "google-chrome",
   mod1key  = "Mod1",
   mod4key  = "Mod4"
}

-- {{{ Variable definitions

-- Themes define colours, icons, and wallpapers
beautiful.init(config.global.theme)
-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end


-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.floating,
    awful.layout.suit.floating,
}
-- }}}
-- Tags table
tprop = {
  {
     names = { " Terminal ", " Editor ", "Internet", " Current " },
     layout = { layouts[1], layouts[1], layouts[2] , layouts[2] },
  },
  {
     names = { " Internet ", " Rdesktop1 ", " Rdesktop2 " },
     layout = { layouts[2], layouts[2], layouts[2] },
     mwfact = {1.0, 1.0, 1.0},
  }
}

tags = {}
for s = 1, screen.count() do
   -- Each screen has its own tag table.
   local tidx = (s % (screen.count()) + 1)
   tags[s] = awful.tag(tprop[s].names, s, tprop[s].layout)
end
awful.tag.setproperty(tags[1][1], "mwfact", 0.45)
awful.tag.setproperty(tags[1][2], "mwfact", 0.45)

-- }}}

-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
awful.menu.menu_keys = { up  = { "k", "Up" },
                         down = { "j", "Down" },
                         exec = { "l", "Return", "Right" },
                         enter = { "Right" },
                         back = { "h", "Left" },
                         close = { "q", "Escape" },
                       }

-- main menu
vnc_cli = "vncviewer -PasswordFile=/home/dreddy/.vnc/passwd -fullcolor "
rdp_cli = "rdesktop -u dreddy -E -z -x 0xE3 -0 -r sound:remote "
rdp_opt = " "
--rdp_cli = "xfreerdp /d:AMR /u:dreddy /monitor:1 /audio-mode:1 /a:cliprdr /sec:rdp "
--rdp_opt = " /fonts /compression /clipboard /wallpaper /monitors:1 "
remote_items = {
   { "dreddy-&win8", rdp_cli .. rdp_opt ..
        " -d AMR -g 1920x1080 dreddy-desk3.amr.corp.intel.com"},
   { "date-&itp", rdp_cli .. rdp_opt ..
        " -g 1280x1024 date-itp.jf.intel.com"},
   { "&franklin", vnc_cli .. "franklin:1" },
}
remotemenu = awful.menu({items=remote_items})

desktopmenu = awful.menu( { items = {
                               { "terminal",      config.global.terminal },
                               { "editor",        config.global.editor },
                               { "www browser",   config.global.browser },
                               { "restart", awesome.restart },
                               { "quit", awesome.quit },
},
                            submenu_icon = beautiful.submenu_icon,
                          })

-- Menubar configuration
menubar.utils.terminal = config.global.terminal -- Set the terminal for applications that require it
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()

-- Create a wibox for each screen and add it
spacer = wibox.widget.textbox()
spacer:set_text('    ')

leftbr = wibox.widget.textbox('[ ')
rightbr = wibox.widget.textbox(' ]')

separator = wibox.widget.imagebox()
separator:set_image(beautiful.widget_sep)

-- Create a wibox for each screen and add it
wibox_top = {}
mylayoutbox = {}
mypromptbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly)
)
mytasklist  = {}
mytasklist.buttons = awful.util.table.join (
   awful.button({ }, 1, function (c)
                   if c == client.focus then
                      c.minimized = true
                   else
                      c.minimized = false
                      if not c:isvisible() then
                         awful.tag.viewonly(c:tags()[1])
                      end
                      client.focus = c
                      c:raise()
                   end
                        end))

mytextclock = awful.widget.textclock(" %Y/%m/%d %H:%M ")
mytextclock:set_align("RIGHT")
--mytextclock:set_font("Arial bold 12")
mysystray = wibox.widget.systray()

for s = 1, screen.count() do
   local w_width = screen[s].geometry.width
   -- Create a prompt widget
   mypromptbox[s] = awful.widget.prompt()
   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s,
                                       awful.widget.taglist.filter.all,
                                       mytaglist.buttons)

   -- Widgets that are aligned to the right
   local left_layout = wibox.layout.fixed.horizontal()
   left_layout:add(mysystray)
   left_layout:add(mytextclock)
   left_layout:add(mypromptbox[s])
   -- Widgets that are aligned to the left
   local right_layout = wibox.layout.fixed.horizontal()
   right_layout:add(mytaglist[s])

   -- Create the wibox
   wibox_top[s] = awful.wibox({ position = "bottom",
                                border_color=beautiful.border_normal,
                                border_width = 1,
                                height = 20,
                                align = "center",
                                screen = s })

   -- Now bring it all together (with the tasklist in the middle)
   local layout = wibox.layout.align.horizontal()
   layout:set_left(left_layout)
   layout:set_right(right_layout)

   wibox_top[s]:set_widget(layout)
end
-- }}}


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
		awful.button({ }, 3,
		   function ()
		      desktopmenu:hide()
		      remotemenu:toggle()
		end),
		awful.button({ }, 1,
		   function ()
		      remotemenu:hide()
		      desktopmenu:toggle()
		end)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
   -- show desktopmenu
   awful.key({ config.global.mod4key }, "e",
             function ()
                remotemenu:hide()
                desktopmenu:toggle()
             end),
   -- show a remote menu
   awful.key({ config.global.mod4key }, "x",
             function ()
                desktopmenu:hide()
                remotemenu:toggle()
             end),

   -- Standard program
   awful.key({ config.global.mod4key, "Control" }, "r", awesome.restart),
   awful.key({ config.global.mod4key, "Control" }, "q", awesome.quit),

   -- applications
   awful.key({ config.global.mod4key, }, "Return",
             function () awful.util.spawn("urxvt") end),

   -- Naughty
   awful.key({ config.global.mod4key, }, "n", naughty.toggle),

   -- Screen
   awful.key({ config.global.mod4key, }, "Escape",
             function () awful.screen.focus_relative(1) end),

   -- Switch clients
   awful.key({ config.global.mod4key, }, "Tab",
             function ()
                awful.client.focus.byidx(-1)
                if client.focus then client.focus:raise() end
             end),
   awful.key({ config.global.mod4key, "Shift" }, "Tab",
             function ()
                awful.client.focus.history.previous()
                if client.focus then client.focus:raise() end
             end),
   awful.key({ config.global.mod1key, }, "Tab",
             function ()
                awful.menu.menu_keys.down = { "Down", "Alt_L", "Tab", "j" }
                awful.menu.menu_keys.up = { "Up", "k" }
                awful.menu.clients({theme = {width = 400}}, {keygrabber = true})
             end),

   awful.key({ config.global.mod4key, }, "u", awful.client.urgent.jumpto),

   awful.key({ config.global.mod4key }, "l",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),

   awful.key({ config.global.mod4key }, "F12",
              function ()
	      	       local ts = awful.tag.selected()
		       local orig_screen = ts.screen

		       ts.screen = awful.util.cycle(screen.count(), ts.screen + 1)

		       for _,c in pairs(ts:clients()) do
		       	   c.screen = ts.screen
			   c:tags({ts})
		       end
		       io.stderr:write("Hello World\n")
              end)
   -- Layout manipulation

   -- Lock Screen

   -- capture screenshot
)


clientkeys = awful.util.table.join(
   -- Close the client
   awful.key({ config.global.mod1key, "Control" }, "q", function (c) c:kill()  end),

   -- hide the client
   awful.key({ config.global.mod4key, }, "h", function (c) c.hidden = true end),

   -- Swap the client with master
   awful.key({ config.global.mod1key, }, "Return",
             function (c) c:swap(awful.client.getmaster()) end),
   -- Move to the left screen
   awful.key({ config.global.mod1key, "Control" }, "Left",
             function (c)
                awful.client.movetoscreen(c)
                awful.placement.no_overlap(c)
             end),
   -- Move to the right screen
   awful.key({ config.global.mod1key, "Control" }, "Right",
             function(c)
                awful.client.movetoscreen (c)
                awful.placement.no_overlap(c)
             end),
   -- Vertical resize
   awful.key({config.global.mod4key, "Shift" }, "Down",
             function () awful.client.swap.bydirection("down") end),
   awful.key({config.global.mod4key, "Shift" }, "Up",
             function () awful.client.swap.bydirection("up") end),
   awful.key({config.global.mod4key, "Shift" }, "Right",
             function () awful.client.swap.bydirection("right") end),
   awful.key({config.global.mod4key, "Shift" }, "Left",
             function () awful.client.swap.bydirection("left") end),

   -- Max toggle
   awful.key({config.global.mod4key, "Shift" }, "m",
             function(c)
                c.maximized_horizontal = not c.maximized_horizontal
                c.maximized_vertical = not c.maximized_vertical
             end),

   -- Resize
   awful.key({ config.global.mod4key, "Control"}, "Next",  function () awful.client.moveresize( 20,  20, -40, -40) end),
   awful.key({ config.global.mod4key, "Control"}, "Prior", function () awful.client.moveresize(-20, -20,  40,  40) end),
   awful.key({ config.global.mod4key, "Control"}, "Down",  function () awful.client.moveresize(  0,  20,   0,   0) end),
   awful.key({ config.global.mod4key, "Control"}, "Up",    function () awful.client.moveresize(  0, -20,   0,   0) end),
   awful.key({ config.global.mod4key, "Control"}, "Left",  function () awful.client.moveresize(-20,   0,   0,   0) end),
   awful.key({ config.global.mod4key, "Control"}, "Right",  function () awful.client.moveresize(20,   0,   0,   0) end),

   -- Navigation
   awful.key({ config.global.mod4key }, "Left", function() awful.client.focus.bydirection("left") end),
   awful.key({ config.global.mod4key }, "Right", function() awful.client.focus.bydirection("right") end),
   awful.key({ config.global.mod4key }, "Up", function() awful.client.focus.bydirection("up") end),
   awful.key({ config.global.mod4key }, "Down", function() awful.client.focus.bydirection("down") end),

   -- Trigger client mode on mod4 + space.
   -- Perform actions on the client corresponding to the key pressed in client mode.
   awful.key({ config.global.mod4key, }, "space",
             function(c)
                keygrabber.run(function(mod, key, event)
                                  if event == "release" then return end
                                  keygrabber.stop()
                                  if client_mode[key] then client_mode[key](c) end
                               end)
             end)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.

for i = 1, 9 do
   globalkeys = awful.util.table.join(
      globalkeys,
      awful.key({ config.global.mod4key }, i,
                function ()
                   local screen = mouse.screen
		   local tag = awful.tag.gettags(screen)[i]
                   if tag then
                      awful.tag.viewonly(tag)
                   end
                end),
      awful.key({ config.global.mod4key, "Shift" }, i,
                function ()
                   if client.focus then
                      local tag = awful.tag.gettags(client.focus.screen)[i]
                      local fc = client.focus
                      if tag then
                         awful.client.movetotag(tag)
                      end
                   end
                end),
      awful.key({ config.global.mod4key, "Control" }, i,
                function ()
		   local screen = client.focus.screen
		   local tag = awful.tag.gettags(screen)[i]
                   if client.focus and tag then
                      awful.client.toggletag(tag)
                   end
                end)
   )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ config.global.mod4key }, 1, awful.mouse.client.move),
    awful.button({ config.global.mod4key }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
awful.rules.rules = {
   -- All clients will match this rule.
   {
      rule = { },
      properties = { border_width = beautiful.border_width,
		     size_hints_honor = true,
		     border_color = beautiful.border_normal,
		     focus = true,
		     keys = clientkeys,
		     buttons = clientbuttons,
		     maximized_horizontal = false,
		     maximized_vertical   = false
     },
     -- callback   = awful.client.setslave
   },
   { 
      rule = { class = "gimp" },
      properties = { floating = true }
   },
   {
      rule_any = { class = {"Iceweasel", "google-chrome", "google-chrome-beta"} },
      except_any = { name = { "Chat" }, role = { "Manager" } },
      properties = { tag = tags[1][3], 
		     switchtotag=true,
		     border_width=1
      }
   },
   {
      rule = { role = "Preferences" },
      properties = { floating = true },
   },
   {
      rule_any = { class = {"xfreerdp", "rdesktop"} },
      properties = { border_width=0, switchtotag=true }
   }
}

-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("focus",
                      function(c)
                         c.border_color = beautiful.border_focus
                         c:raise()
                      end)

client.connect_signal("unfocus",
                      function(c)
                         c.border_color = beautiful.border_normal
                      end)

-- Signal function to execute when a new client appears.
function manage_handler(c, startup)
   c:connect_signal('property::urgent',
                    function(c)
                       if not c.urgent then
                          c.hidden = false
                       end
                    end)

   if not startup then
      if not c.size_hints.user_position and
      not c.size_hints.program_position then
         awful.placement.no_overlap(c)
         awful.placement.no_offscreen(c)
      end
      if c.transient_for then
         awful.placement.centered(c, c.transient_for)
      end
   end
end

client.connect_signal("manage", manage_handler)

-- notify when the layout of a tag changes
awful.tag.attached_connect_signal(1, "property::layout", function(t)
                                     layout = awful.tag.getproperty(t, "layout").name
                                     naughty.notify({text = layout, timeout = 1, icon = beautiful["layout_"..layout]})
                                                         end)

-- notify when the nmaster of a tag changes
awful.tag.attached_connect_signal(1, "property::nmaster",
                                  function(t)
                                     nmaster = awful.tag.getnmaster(t)
                                     naughty.notify(
                                        {text = "nmaster: "..nmaster, timeout = 1}
                                     )
                                  end)

-- notify when the ncol of a tag changes
awful.tag.attached_connect_signal(1, "property::ncol",
                                  function(t)
                                     ncol = awful.tag.getncol(t)
                                     naughty.notify({text = "ncol: "..ncol, timeout = 1})
                                  end)

-- }}}

awful.util.spawn_with_shell("setxkbmap -option \"ctrl:nocaps\"")

-- }}}
