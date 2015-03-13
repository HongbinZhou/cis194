module Main2 where

import Scrabble
import Sized
import JoinList
import JoinListBuffer
import Editor
import Buffer

------------------------------------------------------------
-- Still Not work yet!!!
------------------------------------------------------------
-- main2 = runEditor editor $ fromString $ unlines
main2 =  (Buffer.fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]):: JoinList (Score, Size) String
