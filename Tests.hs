module Tests where

import Control.Arrow ((***))
import Control.Monad.State
import Syntax
import PiMonad
import Interpreter

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util (putDocW)
import PPrint

-- some tests

{-   p0 = (new i. c!i .
           i?{PLUS -> i?<x,y> . i!(x+y) . end
              NEG  -> i?x . i!(-x). end} )
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

startSt :: St
startSt = ([startE], [], [], Nothing)

startE = Call (NS "p0") `Par` Call (NS "p1") `Par` Call (NS "p2")

traceIt = trace defs 0 startSt
runIt n = run defs n 0 startSt

ppTraceIt i = ppMsgSt (traceIt !! i)
ppRunIt n =
  vsep [pretty "Output:" <+> hsep (map pretty sout),
        pretty "-- current state --",
        ppMsgSt st]
  where (sout, st) = runIt n

-- pretty printing

ppMsgSt :: Either ErrMsg St -> Doc a
ppMsgSt (Left msg) = pretty "error:" <+> pretty msg
ppMsgSt (Right st) = ppSt st

ppSt :: St -> Doc a
ppSt (ps, waits, news, sout) =
 vsep [pretty "Running:" <+>
         nest 2 (pretty ps),
       pretty "Waiting:",
         indent 2 (vsep (map ppWaiting waits)),
       encloseSep (pretty "New: ") (pretty ".") comma
          (map pretty news),
       pretty "OutQueue:" <+> pretty sout]

ppWaiting :: (Name, Waiting) -> Doc a
ppWaiting (c, Senders ps) =
  pretty c <> pretty "!" <>
   align (encloseSep lbracket rbracket comma
           (map ppPs ps))
 where ppPs (x,p) =
        pretty x <+> pretty "->" <+> nest 2 (ppPi p 0)
ppWaiting (c, Receivers pps) =
  pretty c <> pretty "?" <>
   align (encloseSep lbracket rbracket comma
           (map ppPs pps))
 where ppPs [(x,p)] = ppSingle (x,p)
       ppPs ps =
        align (encloseSep lbrace rbrace sepa (map ppSingle ps))
       ppSingle (x,p) = pretty x <+> pretty "->" <+> nest 2 (ppPi p 0)
       sepa = flatAlt mempty (pretty "; ")
