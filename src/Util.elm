module Util exposing (..)


done : a -> ( a, Cmd msg )
done model =
    ( model, Cmd.none )
