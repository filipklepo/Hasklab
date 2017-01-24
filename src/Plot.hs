module Plot where

import Data.Char
import Data.List
import Tree
import Graphics.Rendering.Chart.Easy hiding (Const)
import Graphics.Rendering.Chart.Gtk
import Transform

-- Checks whether the given symbolic tree can be plotted.
-- A symbolic tree can be plotted if it contains less than two distinct
-- variables.
plottable :: SymTree -> (Bool, Maybe String)
plottable t = case nub (variables t) of
                [] -> (True, Nothing)
                xs -> if length xs > 1 then (False, Nothing)
                      else (True, Just $ head xs)

-- Takes a symbolic tree and a optional variable it depends on and
-- generates tree's values at given range.
values :: SymTree -> Maybe String -> [Double] -> [(Double,Double)]
values t (Just n) xs = [(x, evaluate $ substitute n t x) | x <- xs]
values t Nothing  xs = [(x, constVal) | x <- xs]
  where constVal = evaluate t

hlPlot :: SymTree -> IO ()
hlPlot t = toWindow 500 500 $ do
    layout_title .= "Hasklab plot"
    let (isPlottable, mVar) = plottable t
    if not isPlottable then
      return $ error "Given tree can not be plotted."
    else
      plot (line (show t) [values t mVar [0,(0.1)..20]])
