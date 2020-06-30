port module Ports exposing (IncomingData, OutgoingData, elmToJs, jsToElm)

import Json.Decode as Decode
import Json.Encode as Encode


type alias OutgoingData =
    { dataType : String
    , payload : Maybe Encode.Value
    }


type alias IncomingData =
    { dataType : String
    , payload : Decode.Value
    }


port elmToJs : OutgoingData -> Cmd msg


port jsToElm : (IncomingData -> msg) -> Sub msg
