module Main where

import Control.Arrow ((***))
import Data.Function ((&))

import Control.Monad.State
import Syntax
import PiMonad
import Interpreter
import Utilities (FMap)
import Backend

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import PPrint

main :: IO ()
main = do
  print $ start defs startE & trace [0,0] & readInp 0 (VI 10) & trace [] & ppBState

-- some tests

{-   p0 = (nu i) c!i .
           i?{PLUS -> i?<x,y> . i!(x+y) . end
              NEG  -> i?x . i!(-x). end}
     p1 = c?j . j!PLUS . j!<3,4> . j?z . StdOut!z . end
     p2 = c?j . StdIn?x . j!NEG . j!x . j?z . StdOut!z . end

     main = p0 | p1 | p2

-}

recv c xs p = Recv c [(xs,p)]
send c v p = Send c v p
choices c xss = Recv c (map (PL *** id) xss)

neg x = EMinus (EV (VI 0)) x

defs = [(NS "p0",
         Nu i (send c (eN i)
          (choices i
            [("PLUS", (recv i (PT [PN x, PN y])
                  (send i (EPlus (eN x) (eN y)) (Call (NS "p0"))))),
             ("NEG", (recv i (PN x)
                 (send i (neg (eN x)) (Call (NS "p0")))))])))
       ,(NS "p1",
          recv c (PN j)
              (send j (eL "PLUS")
                (send j (ETup [eI 3, eI 4])
                  (recv j (PN z)
                    (Send (NR StdOut) (eN z) End)))))
       ,(NS "p2",
          recv c (PN j)
           (recv (NR StdIn) (PN x)
            (send j (eL "NEG")
             (send j (eN x)
               (recv j (PN z)
                 (Send (NR StdOut) (eN z) End))))))
       ]
    where [i,j,c,x,y,z] = map NS ["i","j","c","x","y","z"]

startSt :: PiMonad St
startSt = lineup defs [startE] ([],[],[], [])

ppstartSt' =
  vsep (map (\s -> ppMsgSt s <> line) (runPiM 0 startSt))

startE = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")

-- try in GHCi:
-- start defs startE & trace [0,0] & readInp 0 (VI 10) & trace [0,0,0,0,1,1,0,0,0,0] & ppBState
