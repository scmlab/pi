module Tests where

import Control.Arrow ((***))
import Control.Monad.State
import Syntax
import PiMonad
import Interpreter


-- some tests

{-   p0 = (new i. c!i .
           i?{PLUS -> i?<x,y> . i!(x+y) . end
              NEG  -> i?x . i!(-x). end} )
     p1 = c?j . j!PLUS . j!<3,4> . j?z . stdout!z . end
     p2 = c?j . j!NEG . j!5 . j?z . stdout!z . end
-}

recv c xs p = Recv (eN c) [(xs,p)]
send c v p = Send (eN c) v p
choices c xss = Recv (eN c) (map (PL *** id) xss)

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
                    (Send (eR StdOut) (eN z) End)))))
       ,(NS "p2",
          recv c (PN j)
           (send j (eL "NEG")
             (send j (eI 5)
               (recv j (PN z)
                 (Send (eR StdOut) (eN z) End)))))
       ]
    where [i,j,c,x,y,z] = map NS ["i","j","c","x","y","z"]

startSt :: St
startSt = ([startE], [], [], Nothing)

startE = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = (f x >>= iterateM f) >>= (return . (x:))

traceIt = trace defs 0 startSt
runIt n = run defs n 0 startSt
