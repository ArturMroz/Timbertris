module Messages exposing (Msg(..))

import Time


type Msg
    = Rotate
    | Shift ( Int, Int )
    | MsgNewTetromino Int
      -- | GetNewTetromino
    | Tick Time.Posix
    | FillRow Int
      -- | RemoveFullRow
    | HardDrop
    | NewGame
    | Nop
