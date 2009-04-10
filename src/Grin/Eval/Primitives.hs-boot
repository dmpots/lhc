module Grin.Eval.Primitives where

import CompactString
import Grin.Eval.Types

runPrimitive :: CompactString -> [EvalValue] -> Eval EvalValue
runEvalPrimitive :: EvalValue -> Eval EvalValue 
