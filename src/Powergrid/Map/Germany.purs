module Powergrid.Map.Germany (germany) where

import Prelude

import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Powergrid.NetworkMap (Area(..), City(..), NetworkMap, newNetworkMap)

germany :: NetworkMap
germany = newNetworkMap $ fromFoldable [
    Tuple flensburg (fromFoldable [Tuple kiel 4]),
    Tuple kiel (fromFoldable [Tuple hamburg 8]),
    Tuple hamburg (fromFoldable [Tuple cukhaven 11, Tuple bremen 11, Tuple hannover 17, Tuple schwerin 8, Tuple lubeck 6]),
    Tuple cukhaven (fromFoldable [Tuple bremen 8]),
    Tuple bremen (fromFoldable [Tuple hannover 10, Tuple wilhelmshaven 11, Tuple osnabrueck 11]),
    Tuple osnabrueck (fromFoldable [Tuple wilhelmshaven 14, Tuple muenster 7]),
    Tuple hannover (fromFoldable [Tuple osnabrueck 16, Tuple kassel 15, Tuple magdeburg 15, Tuple schwerin 19]),
    Tuple muenster (fromFoldable [Tuple dortmund 2, Tuple essen 6]),
    Tuple essen (fromFoldable [Tuple duisburg 0, Tuple duesseldorf 2]),
    Tuple duesseldorf (fromFoldable [Tuple koeln 4, Tuple aachen 9]),
    Tuple dortmund (fromFoldable [Tuple koeln 10, Tuple kassel 18, Tuple frankfurtm 20]),
    Tuple koeln (fromFoldable [Tuple aachen 7, Tuple wiesbaden 21, Tuple trier 20]),
    Tuple kassel (fromFoldable [Tuple fulda 8, Tuple frankfurtm 13]),
    Tuple fulda (fromFoldable [Tuple frankfurtm 8]),
    Tuple frankfurtm (fromFoldable [Tuple wiesbaden 0, Tuple wuerzburg 13]),
    Tuple aachen (fromFoldable [Tuple trier 19]),
    Tuple wiesbaden (fromFoldable [Tuple trier 18, Tuple mannheim 11, Tuple saarbruecken 10]),
    Tuple trier (fromFoldable [Tuple saarbruecken 11]),
    Tuple mannheim (fromFoldable [Tuple saarbruecken 11, Tuple stuttgart 6, Tuple wuerzburg 10]),
    Tuple saarbruecken (fromFoldable [Tuple stuttgart 17]),
    Tuple stuttgart (fromFoldable [Tuple freiburg 16, Tuple konstanz 16, Tuple augsburg 15, Tuple wuerzburg 12]),
    Tuple freiburg (fromFoldable [Tuple konstanz 14]),
    Tuple konstanz (fromFoldable [Tuple augsburg 17]),
    Tuple wuerzburg (fromFoldable [Tuple augsburg 19, Tuple fulda 11]), 
    Tuple augsburg (fromFoldable [Tuple muenchen 6, Tuple regensburg 13, Tuple nuernberg 18]),
    Tuple muenchen (fromFoldable [Tuple regensburg 10, Tuple passau 14]),
    Tuple regensburg (fromFoldable [Tuple passau 12, Tuple nuernberg 18, Tuple nuernberg 12]),
    Tuple nuernberg (fromFoldable [Tuple wuerzburg 8, Tuple erfurt 21]),
    Tuple erfurt (fromFoldable [Tuple fulda 13, Tuple kassel 15, Tuple hannover 19, Tuple dresden 19, Tuple halle 6]),
    Tuple halle (fromFoldable [Tuple leipzig 0, Tuple berlin 17, Tuple magdeburg 11]),
    Tuple leipzig (fromFoldable [Tuple dresden 13, Tuple frankfurto 21]),
    Tuple dresden (fromFoldable [Tuple frankfurto 16]),
    Tuple berlin (fromFoldable [Tuple frankfurto 6]),
    Tuple magdeburg (fromFoldable [Tuple berlin 10]),
    Tuple lubeck (fromFoldable [Tuple schwerin 6]),
    Tuple schwerin (fromFoldable [Tuple rostock 6, Tuple berlin 18, Tuple magdeburg 16, Tuple torgelow 19]),
    Tuple rostock (fromFoldable [Tuple torgelow 19]),
    Tuple torgelow (fromFoldable [Tuple berlin 15])  
  ]
  where
    ne = Area "ne"
    nw = Area "nw"
    e = Area "e"
    w = Area "w"
    sw = Area "sw"
    se = Area "se"

    flensburg = City "flensburg" nw
    kiel = City "kiel" nw
    hamburg = City "hamburg" nw
    cukhaven = City "cukhaven" nw
    bremen = City "bremen" nw
    hannover = City "hannover" nw
    wilhelmshaven = City "wilhelmshaven" nw
    osnabrueck = City "osnabrueck" w
    muenster = City "muenster" w
    dortmund = City "dortmund" w
    essen = City "essen" w
    duisburg = City "duisburg" w
    duesseldorf = City "duesseldorf" w
    koeln = City "koeln" sw
    aachen = City "aachen" sw
    kassel = City "kassel" w
    frankfurtm = City "frankfurtm" sw
    fulda = City "fulda" e
    wiesbaden = City "wiesbaden" sw
    trier = City "trier" sw
    mannheim = City "mannheim" sw
    saarbruecken = City "saarbruecken" sw
    stuttgart = City "stuttgart" se
    freiburg = City "freiburg" se
    konstanz = City "konstanz" se
    augsburg = City "augsburg" se
    wuerzburg = City "wuerzburg" e
    regensburg = City "regensburg" se
    passau = City "passau" se
    nuernberg = City "nuernberg" e
    erfurt = City "erfurt" e
    dresden = City "dresden" e
    halle = City "halle" e
    leipzig = City "leipzig" e
    frankfurto = City "frankfurto" ne
    magdeburg = City "magdeburg" ne
    berlin = City "berlin" ne
    schwerin = City "schwerin" ne
    lubeck = City "lubeck" ne
    rostock = City "rostock" ne
    torgelow = City "torgelow" ne
    muenchen = City "muenchen" se  