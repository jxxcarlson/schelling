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
                   text """In 1971, the economist Thomas Schelling formulated a simple model
                   to help understand patterns of racial segregation. It can be
                   thought of in terms of a game.  There are two kinds of players — green and yellow
                   in the case of the app on the left.  The players are distributed on a grid,
                   witha  certain percentage of the squares vacant — indicated here in dark reddish brown.
                   The players can be distributed in any way whatsoever. We have
                   placed them at random.
                   """
                ]

            , paragraph [ ] [
                el [Font.bold] (text "Rules of the game. ")
                , text """Each player prefers that a certain number of his neighbors be "like" him — perhaps
                2 or 3, for example.  If the number of neighbors like a given players
                 is too small (under the threshold), the player becomes "uncomfortable"
                and will move to a vacant square when his turn comes.  A round of the game is played by first
                lining up the players in a random order, then having them play in turn.
                Each player  determines his comfort level and either
                moves or stays in place.  There is no cooperation
                among players and there is no central control; each player acts independently according simple rules.
                 In the case at hand the rule depends only on the identities of a player's neighbors.
                 Perhaps surprisingly,
                these "micro" behaviors can produce "macro" effects, e.g, spontaneous formation of homogeneous
                neighborhoods.
                  """
                  , el [Font.color (rgb 0 0 1)] (text " <Scroll down>")
                  ]

            , paragraph
                [] [
                el [Font.bold] (text "The app. ")
                 , text
                    """Neighbors are those players living immediately to the North, South, East,
                    and West of a given player, plus those in the adjacent corners.
                    Players are distributed on a 40x40 grid, and two full
                    rounds are played each second.  Schelling played his first "game" on a large
                    cardboard grid with coins that were moved around according the to rules.
                    The game took months to play.  The app you see on the left is written in Elm,
                    a language of pure functions.
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
                        { url ="https://elm-lang.org"
                          , label = el [] (text "Elm-lang.org") })
                 , el [] (newTabLink [Font.color (rgb 0 0 1)]
                       { url ="https://github.com/jxxcarlson/schelling"
                        , label = el [] (text "Code on GitHub") })
                   ]


               ]


--- https://link.medium.com/RuadsCU1DV