module BasicTypes exposing (..)

import Lamdera exposing (ClientId)

{--
Shared fe/be file needs to include game files
Maybe call it Shared, so basic types like PlayerId can go in Types


--}
-- games



-- use client ids for ease of debugging, switch to sessions later
type alias PlayerId = ClientId


--getGame : GameId -> BackendModel -> Maybe Game
--getGame id model = Array.get id model.games

--updateGame : GameId -> Game -> BackendModel -> Array Game
--updateGame id updated model = Array.set id updated model.games
