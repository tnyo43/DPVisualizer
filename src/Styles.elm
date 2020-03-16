module Styles exposing (..)

import Css exposing (..)
import Html.Styled.Attributes exposing (..)


dpTableCel =
    [ Html.Styled.Attributes.width 200
    , style "text-align" "right"
    , css
        [ border3 (px 1) solid black
        ]
    ]

dpTableSelectedCel =
    [ css
        [ backgroundColor red
        ]
    ]
    ++
    dpTableCel

dpTableRowColSelectedCel =
    [ css
        [ backgroundColor orange
        ]
    ]
    ++
    dpTableCel

dpTable =
    [ css
        [ border3 (px 1) solid black
        , borderCollapse collapse
        ]
    ]

dpTableIndex =
    [ css
        [ backgroundColor lightGray
        ]
    ]
    ++
    dpTableCel



-- Color

black = rgb 0 0 0
lightGray = rgb 192 192 192

red = rgb 255 0 0
orange = rgb 255 165 0