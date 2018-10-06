module Tests where

import Control.Arrow ((***))
import Control.Monad.State
import Syntax
import PiMonad
import Interpreter
import Utilities (FMap)
import Backend

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import PPrint

-- some tests

{-   p0 = (nu i) c!i .
           i?{PLUS -> i?<x,y> . i!(x+y) . end
              NEG  -> i?x . i!(-x). end}
     p1 = c?j . j!PLUS . j!<3,4> . j?z . stdout!z . end
     p2 = c?j . j!NEG . j!5 . j?z . stdout!z . end

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
           (send j (eL "NEG")
             (send j (eI 5)
               (recv j (PN z)
                 (Send (NR StdOut) (eN z) End)))))
       ]
    where [i,j,c,x,y,z] = map NS ["i","j","c","x","y","z"]

startSt :: PiMonad St
startSt = lineup defs [startE] ([],[],[])

ppstartSt' =
  vsep (map (\s -> ppMsgSt s <> line) (runPiM 0 startSt))

startE = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")



{-
ppTrace :: (St -> Doc a) -> Trace -> (Doc a, Maybe BState)
ppTrace ppSt Stop = (pretty "stopped" <> line, Nothing)
ppTrace ppSt (Deadlock st) =
  (vsep [pretty "Deadlock at:", ppSt st], Nothing)
ppTrace ppSt (Error msg) = (pretty msg <> line, Nothing)
ppTrace ppSt (TOut v (defs,st,bk)) =
  (vsep [pretty "Output:" <+> pretty v,
         ppSt st]
  , Just (defs,st,bk))
ppTrace ppSt (TIn (defs,st,bk)) =
  (vsep [pretty "Waiting for input.",
         ppSt st]
  , Just (defs,st,bk))
ppTrace ppSt (Next st tr) =
  let (doc, bst) = ppTrace ppSt tr
  in (vsep [ppSt st, line, doc], bst)

fromJust (Just x) = x

resume = ppTrace ppStPi . trace1 . fromJust . snd
-}
