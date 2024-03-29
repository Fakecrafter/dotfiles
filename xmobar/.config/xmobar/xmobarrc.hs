Config { font = "Cascadia Code 12"
       , additionalFonts = []
       , borderColor = "#000000"
       , border = FullB
       , borderWidth = 0
       --, bgColor = "#32302f"
       , bgColor = "#0f0f0f"
       --, fgColor = "#ebdbb2"
       , fgColor = "#a89984"
       , alpha = 255
       , position = Bottom
       , textOffset = 1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [
                      Run Cpu ["-t","CPU: <total>%"] 10
                    , Run Memory ["-t","RAM: <usedratio>%"] 10
                    , Run Com "uname" ["-s","-r"] "" 36000
                    , Run Date "%_d.%m.%Y %H:%M:%S" "date" 10
                    , Run Locks
                    , Run Kbd [("de-latin1", "DE"), ("de", "DE"), ("us", "US")]
                    , Run UnsafeStdinReader
                    , Run Com "screencast" ["status"] "screencast" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%UnsafeStdinReader% <fc=#fb4934>%locks%</fc> } {<fc=#83a598>%cpu%</fc> | <fc=#d3869b>%memory%</fc> | <fc=#fb4934>%date%</fc> "
       }

