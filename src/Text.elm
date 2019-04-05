module Text exposing (..)


import Element exposing (..)
import Element.Background as Background
import Element.Font as Font

panel : Element msg
panel =
    column [ width (px 400), height (px 500), scrollbarY, Font.size 14, paddingXY 12 24, spacing 12]
            [ el [ Font.bold, Font.size 18 ] (text "About the Schelling model")
            , paragraph []
                [
                   text """In 1971 Thomas Schelling formulated what is now called an agent-based model
                   in an attempt to understand patterns of racial segregation. The model can be
                   thought of in terms of a game.  There are two kinds of players, which are green and yellow
                   in the case of the app on the left.  The players are distributed on grid,
                   with certain percentage of the squares vacant — indicated in dark brown in the
                   case of the app.  The players can be distributed in any way whatsoever; we have
                   distributed them at random.
                   """
                ]

            , paragraph [ ] [
                el [Font.bold] (text "Rules of the game. ")
                , text """Each player prefers that a certain percentage of his neighbors to be "like" him — perhaps
                20% or 40%, for example.  If the percentage of neighbors like a given players,
                 is too small, the player becomes "uncomfortable"
                and will move to a vacant square when his turn comes.  In one round of the game, each player
                (at random) asseses his comfort level and either moves or stays in place.  There is no cooperation
                among players and there is no central control; each acts independently according simple rules.
                 In the case at hand the rule depends on a single number, the percentage of neighbors like him.
                 Perhaps surprisingly,
                these "micro" behaviors can lead to "macro" effect, e.g, spontaneous formation of homogeneous
                neighborhoods.
                  """]
            , paragraph
                [] [
                el [Font.bold] (text "The app")
                 , text
                    """Tha app is based on simple notion of "neighbors." Unless a player is on the edge of the board,
                    he has eight immediate neighbors.  Players are distributed on a 40x40 grid.  There are two full
                    rounds of play each second.  It is written in Elm, a language of pure functions.
                                     """
                    ]
            , paragraph
                [spacing 12]
                [  el [Font.bold] (text "References")
                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                    { url ="https://en.wikipedia.org/wiki/Thomas_Schelling"
                    , label = el [] (text "Wikipedia article on Thomas Schelling") })

                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                       {  url ="http://jasss.soc.surrey.ac.uk/15/1/6.html"
                         ,label = el [] (text "The Schelling Model of Ethnic Residential Dynamics") })

                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                        { url ="http://nifty.stanford.edu/2014/mccown-schelling-model-segregation/"
                          , label = el [] (text "The Schelling's Model of Segregation") })

                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                        { url ="https://www.stat.berkeley.edu/%7Ealdous/157/Papers/Schelling_Seg_Models.pdf"
                         , label = el [] (text "Schelling's original article") })
                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                        { url ="https://elm-lang.ord"
                          , label = el [] (text "Elm-lang.org") })
                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                       { url ="https://github.com/jxxcarlson/schelling"
                        , label = el [] (text "Code on GitHub") })
                   ]


               ]


