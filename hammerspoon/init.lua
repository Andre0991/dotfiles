hs.hotkey.bind({"cmd"}, "M", function()
  local frontmostApplication = hs.application.frontmostApplication()
  hs.alert.show(frontmostApplication:name())
  
  if frontmostApplication and frontmostApplication:name() == "Emacs" then
    hs.alert.show("In emacs!")
    hs.eventtap.keyStroke({"alt"}, "x")
    hs.eventtap.keyStrokes("apt-quick-edit-end")
    hs.eventtap.keyStroke({}, "return")
    quick_edit_app:focus()
    hs.eventtap.keyStroke({"cmd"}, "a")
    hs.eventtap.keyStroke({"cmd"}, "v")

  else
    quick_edit_app = hs.window.focusedWindow()
    hs.alert.show("In other!")
    hs.eventtap.keyStroke({"cmd"}, "a")
    hs.eventtap.keyStroke({"cmd"}, "c")
    hs.application.launchOrFocus("Emacs")
    hs.eventtap.keyStroke({"alt"}, "x")
    hs.eventtap.keyStrokes("apt-quick-edit-start")
    hs.eventtap.keyStroke({}, "return")

  
  end

end)

function aptLaunchApp(appName)
       local app = hs.application.find(appName)

    if app == nil then
        hs.alert.show("Launching app")
        hs.application.launchOrFocus(appName)
    elseif app:isFrontmost() then
        app:hide()
    else
        local win = app:mainWindow()
        win:focus()
    end
end

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "e", function()
      aptLaunchApp("emacs")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "s", function()
      aptLaunchApp("slack")
end)

-- b stands for browser
hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "w", function()
      aptLaunchApp("safari")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "l", function()
      aptLaunchApp("logseq")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "c", function()
      aptLaunchApp("chrome")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "d", function()
      aptLaunchApp("calendar")
end)

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "t", function()
      aptLaunchApp("terminal")
end)
