module Rort.Window.Types where
import Data.Text (Text)

data WindowEvent
  = WindowError !Text
  | WindowClose
  | WindowResize !Int !Int
