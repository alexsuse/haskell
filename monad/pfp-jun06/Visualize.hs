module Visualize where

import Probability
import PrintList
import List (nub, sort, sortBy)
import Foreign (unsafePerformIO)


{- TO DO:

* Change function representation in Plot to 
    xs :: [Float]
    ys :: [Float]
  and add functions to create this representation from 
   functions, distributions, and lists
   (i.e. plotF, plotD, plotL)

-}


-- global settings for one figure
-- 
data FigureEnv = FE { fileName :: String,
                      title    :: String,
                      xLabel   :: String,
                      yLabel   :: String }
                 deriving Show

-- default settings for figure environment
-- 
figure = FE { fileName = "FuSE.R",
              title    = "Output",
              xLabel   = "x",
              yLabel   = "f(x)" }


-- types to represent settings for individual plots
-- 
data Color = Black | Blue | Green | Red | Brown | Gray
           | Purple | DarkGray | Cyan | LightGreen | Magenta
           | Orange | Yellow | White | Custom Int Int Int
           deriving Eq

instance Show Color where
  show Black      = "\"black\""
  show Blue       = "\"blue\""
  show Green      = "\"green\""
  show Red        = "\"red\""
  show Brown      = "\"brown\""
  show Gray       = "\"gray\""
  show Purple     = "\"purple\""
  show DarkGray   = "\"darkgray\""
  show Cyan       = "\"cyan\""
  show LightGreen = "\"lightgreen\""
  show Magenta    = "\"magenta\""
  show Orange     = "\"orange\""
  show Yellow     = "\"yellow\""
  show White      = "\"white\""
  show (Custom r g b) = "rgb("++(show r)++", "++(show g)++", "++(show b)++")"

data LineStyle = Solid | Dashed | Dotted | DotDash | LongDash | TwoDash
                 deriving Eq

instance Show LineStyle where
  show Solid    = "1"
  show Dashed   = "2"
  show Dotted   = "3"
  show DotDash  = "4"
  show LongDash = "5"
  show TwoDash  = "6"

type PlotFun = Float -> Float

    
-- settings for individual plots
-- 
data Plot = Plot { ys        :: [Float],
                   xs        :: [Float],
                   color     :: Color,
                   lineStyle :: LineStyle,
                   lineWidth :: Int,
                   label     :: String }

instance Show Plot where
  show _ = "Individual plots cannot be printed.\nPlease use plots \
            \ as arguments to the fig function."


-- default plotting environment
-- 
plot = Plot { ys        = [0],
              xs        = [0],
              color     = Black,
              lineStyle = Solid,
              lineWidth = 1,
              label     = "" }

colors :: [Color]
colors = [Blue,Green,Red,Purple,Black,Orange,Brown,Yellow]

setColor :: Plot -> Color -> Plot
setColor p c = p{color=c}

autoColor :: [Plot] -> [Plot]
autoColor ps | length ps <= n = zipWith setColor ps colors
             | otherwise      = error ("autoColor works for no more than "++
                                       show n++" plots.")
                                where n=length colors

-- create a plot from a distribution
-- 
plotD :: ToFloat a => Dist a -> Plot
--plotD d = plot{ys = map (\x->(dp $ prob' x d')) (extract d'),
--		xs = extract d'}
plotD d = plot{xs = tfl, ys = pdl}
          where d' = mapD toFloat d
		d'' = norm d'
		pl = unD d''
		pl' = sortBy (\(a,p)->(\(a',p')->(compare (toFloat a) (toFloat a')))) pl 
		(tfl, pdl) = unzip pl'
                dp (P p) = p
		-- pl'' = map dp pdl		

-- create a plot from a function
-- 
plotF :: (FromFloat a,ToFloat b) => (Float,Float,Float) -> (a -> b) -> Plot
plotF xd g = plot{ys = map (\x->toFloat (g (fromFloat x))) (xvals xd),xs = xvals xd}
                  where xvals (a,b,d) | a > b = []
                                      | True  = a:xvals (a+d,b,d)

-- create a plot from a list
-- 
plotL  :: ToFloat a => [a] -> Plot
plotL vs = plot{ys = map toFloat vs, xs = map toFloat [1..length vs]}


plotRL :: ToFloat a => R [a] -> Plot
plotRL a = plotL (unsafePerformIO a)

plotRD :: ToFloat a => RDist a -> Plot
plotRD a = plotD (unsafePerformIO a)
		

--yls :: ToFloat a => [a] -> [Plot] -> [[Float]]
--yls xs (p:ps) = [f p (toFloat v) | v <- xs ]:yls xs ps
--yls _  []     = []

yls :: [Float] -> Plot -> Plot
yls xl p = p{xs=x', ys=y'}
	where 	t = zip (xs p) (ys p)
		t' = metaTuple xl t
		(x', y') = unzip t'	

metaTuple :: [Float] -> [(Float,Float)] -> [(Float,Float)]
metaTuple (x:xl) ((p,v):px) | p == x = (p,v):(metaTuple xl px)
metaTuple (x:xl) p'@( (p,v):px ) | p > x = (x,0):(metaTuple xl p')
metaTuple x [] = map (\v->(v,0)) x
metaTuple x y = error $ (show x)++(show y)

-- we want to increase the bounds absolutely
-- account for negative numbers
-- 
incr x | x > 0 = x * 1.05
incr x = x * 0.95

decr x | x > 0 = x * 0.95
decr x = x * 1.05

-- Visualization output
-- 
type Vis = IO ()


-- creating figures
-- 
fig :: [Plot] -> Vis
fig = figP figure

figP :: FigureEnv -> [Plot] -> Vis
figP fe ps = do let xl = sort $ nub $ concatMap xs ps
                let minx = minimum xl
                let maxx = maximum xl
                let n = length xl
                let ys' = map ys (map (yls xl) ps) -- yls xl ps
                let miny = minimum (map minimum ys')
                let maxy = maximum (map maximum ys')
                let out0' = out0 (fileName fe)
                let out1' = out1 (fileName fe)
                out0' ("x <- "++(vec xl))
                out1' ("y <- "++(vec $ (decr miny):(replicate (n-1) (incr maxy))))
                out1' ("plot(x,y,type=\"n\",main=\""++
                        title  fe++"\",xlab=\""++
                        xLabel fe++"\",ylab=\""++
                        yLabel fe++"\")")
                mapM out1' ([drawy (n+1) (ps !! n) (ys' !! n) |
                                n <- [0..length ys' - 1]])
                if null (concatMap label ps) then
                    return ()
                 else 
                   out1' $ legend (incr minx) maxy ps
                out1' ("dev2bitmap(\""++(fileName fe)++".pdf\", type=\"pdfwrite\")")

{-

define: 
  * autoLabel
  * showParams
-}

showParams :: Show a => [a] -> [String] -> String
showParams xs ss = asTuple id (zipWith (\x s-> x++":"++s) (map show xs) ss)

legend :: Float -> Float -> [Plot] -> String
legend x y ps = "legend("++(show x)++", "++(show y)++","++
                "lty="++vec (map lineStyle ps)++","++
                "col="++vec (map color ps)++","++
                "lwd="++vec (map lineWidth ps)++","++
                "legend="++vec (map label ps)++")"

drawy :: ToFloat a => Int -> Plot -> [a] -> String
drawy yn p fl = "y"++(show yn)++" <- "++(vec (map toFloat fl))++"\n"++
                "lines(x,y"++(show yn)++",col="++(show $ color p)++","++
                "lty="++(show $ lineStyle p)++",lwd="++(show $ lineWidth p)++")"


vec :: Show a => [a] -> String
vec xs = "c"++asTuple show xs

out0 :: String -> String -> IO ()
out0 f s = writeFile (f) (s++"\n")
                         
out1 :: String -> String -> IO ()
out1 f s = appendFile (f) (s++"\n")

