module Util exposing (done)


done : a -> ( a, Cmd msg )
done model =
    ( model, Cmd.none )
