module Draw exposing (stuff)

import Canvas exposing (lineTo, moveTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)

import Color exposing (rgb)

import Html exposing ( div, pre, text )

stuff : List (Html.Html msg)
stuff = [ div [] [text "boo", Canvas.toHtmlWith
          { width = 200, height = 200
          , textures = []

          } []
            [ shapes 
              [ fill <| rgb 0 0.5 0.5 ]
              [ rect (0, 0) 200.0 200.0 ]
            , shapes
              [ stroke <| rgb 1.0 1.0 0.0 ]
              [ path (10.0, 10.0)
                [ lineTo (180.0, 90.0)
                ]
              ]
            ]
          ]
        ]


