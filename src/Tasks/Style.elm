module Tasks.Style exposing (Style, darkStyle, lightStyle, paddingScale)

import Element exposing (Color, rgb255)
import Tasks.Utils exposing (epoch, intModular)
import Time


type alias Style =
    { background : Color
    , taskBackground : Color
    , doneBackground : Color
    , lateBackground : Color
    , highPrioBackground : Color
    , lowPrioBackground : Color
    , buttonBackground : Color
    , textColor : Color
    , textSize : Int -> Int
    , currentTime : Time.Posix
    , timeZone : Time.Zone
    }


lightStyle : Style
lightStyle =
    { background = rgb255 225 225 225
    , taskBackground = rgb255 200 200 200
    , doneBackground = rgb255 150 200 150
    , lateBackground = rgb255 200 100 100
    , highPrioBackground = rgb255 200 200 250
    , lowPrioBackground = rgb255 200 200 150
    , buttonBackground = rgb255 180 180 180
    , textColor = rgb255 0 0 0
    , textSize = intModular 16 1.2
    , currentTime = epoch
    , timeZone = Time.utc
    }


darkStyle : Style
darkStyle =
    { background = rgb255 50 50 50
    , taskBackground = rgb255 75 75 75
    , doneBackground = rgb255 75 100 75
    , lateBackground = rgb255 100 75 75
    , highPrioBackground = rgb255 75 75 100
    , lowPrioBackground = rgb255 75 75 50
    , buttonBackground = rgb255 100 100 100
    , textColor = rgb255 255 255 255
    , textSize = intModular 16 1.2
    , currentTime = epoch
    , timeZone = Time.utc
    }


paddingScale : Int -> Int
paddingScale n =
    5 * n
