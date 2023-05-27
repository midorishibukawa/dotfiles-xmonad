Config  { font              = "xft:Source Sans Pro:style=bold:size=11:antialias=true:hinting=true"
        , additionalFonts   =
                            [ "xft:SauceCodePro Nerd Font Mono:style=regular:size=16:antialias=true:hinting=true"
                            , "xft:SauceCodePro Nerd Font Mono:style=regular:size=14:antialias=true:hinting=true"
                            , "xft:midori\-icons:style=regular:size=13:antialias=true:hinting=true"
                            ]
        , borderColor       = "#fcfaf6"
        , border            = NoBorder
        , bgColor           = "#fcfaf6"
        , fgColor           = "#fcfaf6"
        , alpha             = 102
        , position          = Static
                            { xpos          = 4
                            , ypos          = 4
                            , width         = 2552
                            , height        = 21
                            }
        , textOffset        = 0
        --, textOffsets       = [ 18, 17, 18 ]
        , iconOffset        = -1
        , lowerOnStart      = True
        , pickBroadest      = False
        , persistent        = False
        , hideOnStart       = False
        , iconRoot          = "./icons"
        , allDesktops       = True
        , overrideRedirect  = True
        , commands          =
                            [ Run Date "%H:%M:%S" "time" 10
                            , Run Com "/home/midori/.local/bin/network-status" [] "network" 10
                            , Run Com "/home/midori/.local/bin/volume-status" [] "volume" 10
                            , Run Com "/home/midori/.local/bin/updates-status" [] "updates" 10
                            , Run UnsafeStdinReader
                            ]
        , sepChar           = "%"
        , alignSep          = "}{"
        , template          = " %UnsafeStdinReader% } <action=rofi-date>%time%</action> { <fn=3>%updates%</fn><fn=3>%network%</fn><fn=3>%volume%</fn> "
        }
