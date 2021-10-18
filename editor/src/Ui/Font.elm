module Ui.Font exposing (..)

import Element.Font as F


ibmPlex : F.Font
ibmPlex =
  F.external
    { url = "https://fonts.googleapis.com/css?family=IBM+Plex+Sans"
    , name = "IBM Plex Sans"
    }


courierPrime : F.Font
courierPrime =
  F.external
    { url = "https://fonts.googleapis.com/css?family=Courier+Prime"
    , name = "Courier Prime"
    }
