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

hs.hotkey.bind({"cmd", "alt", "ctrl", "shift"}, "e", function()
    local app = hs.application.find("emacs")

    if app == nil then
        hs.alert.show("Launching emacs")
        hs.application.launchOrFocus("emacs")
    elseif app:isFrontmost() then
        app:hide()
    else
        local win = app:mainWindow()
        win:focus()
    end
end)
