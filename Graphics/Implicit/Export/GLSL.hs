{-# LANGUAGE DeriveFunctor, RecordWildCards #-}

module Graphics.Implicit.Export.GLSL (writeGLSL) where

import Graphics.Implicit.Definitions
    (ℝ, SymbolicObj2(Circle, UnionR2, RectR, Translate2, Scale2, IntersectR2, DifferenceR2, PolygonR),
    SymbolicObj3(Rect3R, UnionR3, IntersectR3, Translate3, Sphere, Cylinder, DifferenceR3, Rotate3, Mirror3, ExtrudeR, RotateExtrude, Scale3))

import Prelude
    (FilePath, IO, String, ($), (++), writeFile, readFile, show, error, Maybe(Just, Nothing), Either(Left), Show, length, null,
    foldr1, foldl1, map, concat, unlines, Int, (+), (-), (*), (||), (&&), pi, (/), max, min, Monoid(mappend, mempty), Bool,
    reverse, minimum, (!!), (<), fst, compare, snd,
    Monad(return, (>>=)), Applicative(pure, (<*>)), Functor(fmap), mapM, (==), (<=), all, otherwise, (.), uncurry, zip)

import Data.Semigroup (Semigroup((<>)))
import Data.Char (isAlphaNum)
import Data.Function (on)
import Data.List (intersperse, minimumBy)
import Data.VectorSpace (normalized, magnitude, (^-^), (<.>), (^*))

-- utilities for creating GLSL expressions that don't introduce new variables:

call :: String -> [String] -> String
call f args = f ++ "(" ++ concat (intersperse ", " args) ++ ")"

asVec3 :: (ℝ, ℝ, ℝ) -> String
asVec3 (x, y, z) = call "vec3" [show x, show y, show z]

asVec2 :: (ℝ, ℝ) -> String
asVec2 (x, y) = call "vec2" [show x, show y]

opSubtraction, opIntersection, opUnion :: String -> String -> String
opSubtraction a b = call "opSubtraction" [b, a]
opIntersection a b = call "opIntersection" [a, b]
opUnion a b = call "opUnion" [a, b]

-- monad for generating GLSL expressions that can also introduce new variables:

data PolySegment = PolySegment
    { polySegStart, polySegEnd, polySegDir :: (ℝ, ℝ) -- dir must be normalized
    , polySegSize :: ℝ -- must be > 0
    , polySegIndex, polySegPrev, polySegNext :: Int
    }

instance Show PolySegment where
    show PolySegment{..} =
        call "PolySeg" [asVec2 polySegStart,
                        asVec2 polySegEnd,
                        asVec2 polySegDir,
                        show polySegSize,
                        show polySegIndex,
                        show polySegPrev,
                        show polySegNext]

data State = State { nextVar, nextPolySeg :: Int }

data Output = Output { declarations :: [String], polySegs :: [PolySegment] }

instance Semigroup Output where
    Output a b <> Output a' b' = Output (a <> a') (b <> b')

instance Monoid Output where
    mempty = Output mempty mempty

newtype Writer a = Writer { runWriter :: State -> (a, State, Output)}
    deriving Functor

instance Applicative Writer where
    pure x = Writer $ \i -> (x, i, mempty)
    Writer a <*> Writer b = Writer $ \i ->
        let
            (x, j, o1) = a i
            (y, k, o2) = b j
        in
            (x y, k, o1 <> o2)

instance Monad Writer where
    return x = Writer $ \s -> (x, s, mempty)
    Writer f >>= g = Writer $ \s ->
        let
            (x, s', o1) = f s
            Writer h = g x
            (y, s'', o2) = h s'
        in
            (y, s'', o1 <> o2)

defineVar :: String -> String -> Writer String
defineVar t init
    | all isAlphaNum init = return init
    | otherwise = Writer $ \(State i npv) -> let j = i + 1 in
        ("v" ++ show j, State j npv, Output [t ++ " v" ++ show j ++ " = " ++ init ++ ";"] [])

definePolySegs :: [(ℝ, ℝ)] -> Writer (Int, Int) -- returns start and one-past-end index of stored segments
definePolySegs points = Writer $ \State{..} ->
    let
        newSegs = makePolySegments nextPolySeg $ if counterClockwise points then reverse points else points
        newNextSeg = nextPolySeg + length newSegs
    in ((nextPolySeg, newNextSeg), State nextVar newNextSeg, Output [] newSegs )

loopSegments :: [a] -> [(a, a)] -- e.g. loopSegments "bla" = [('b', l'), ('l', a'), ('a', b)]
loopSegments xs@(h:t) = zip xs (t ++ [h])

distSqrd :: (ℝ, ℝ) -> ℝ
distSqrd (x, y) = x * x + y * y

isInside :: [PolySegment] -> (ℝ, ℝ) -> Bool
isInside segs p = (if onRhs segA (polySegEnd segB) then (&&) else (||)) (onRhs segA p) (onRhs segB p)
    where
        t = [ (d, (seg, sp))
            | seg@PolySegment{..} <- segs
            , let q = p ^-^ polySegStart
            , let sp = max 0 (min polySegSize (q <.> polySegDir))
            , let d = distSqrd (q ^-^ (polySegDir ^* sp)) ]
        (nearest, sp) = snd $ minimumBy (compare `on` fst) t
        halfSize = 0.5 * polySegSize nearest
        segA = segs !! (if sp < halfSize then polySegPrev nearest else polySegIndex nearest)
        segB = segs !! (if sp < halfSize then polySegIndex nearest else polySegNext nearest)

counterClockwise :: [(ℝ, ℝ)] -> Bool
counterClockwise points =
    isInside (makePolySegments 0 points) (minimum (map fst points) - 1, 0)

onRhs :: PolySegment -> (ℝ, ℝ) -> Bool
onRhs PolySegment{polySegDir=(dirx, diry), ..} p =
    0 < ((p ^-^ polySegStart) <.> (diry, -dirx))

makePolySegments :: Int -> [(ℝ, ℝ)] -> [PolySegment]
makePolySegments startIndex points = map f $ zip [startIndex..] $ loopSegments points
    where
        lastIndex = startIndex + length points - 1
        f (i, (start, end)) =
            let
                v = end ^-^ start
                prev = if i == startIndex then lastIndex else i - 1
                next = if i == lastIndex then startIndex else i + 1
            in
                PolySegment start end (normalized v) (magnitude v) i prev next

-- defineVar is basically used to introduce variables initialized with
-- potentially big expressions that could otherwise end up getting duplicated.

-- 3d distance functions:

distance3 :: String -> SymbolicObj3 -> Writer String

distance3 v (Cylinder _ _ _) = return $ call "sdSphere" [v, "0.01"] -- bad

distance3 v (Sphere r) = return $ call "sdSphere" [v, show r]
distance3 v (Rect3R 0 (0, 0, 0) q) =  return $ call "sdBox" [v, asVec3 q]
distance3 v (Rect3R 0 start end) = distance3 v (Translate3 start (Rect3R 0 (0, 0, 0) (end ^-^ start)))

distance3 v (UnionR3 0 objs) = do
    v' <- defineVar "vec3" v
    foldr1 opUnion `fmap` mapM (distance3 v') objs
distance3 v (IntersectR3 0 objs) = do
    v' <- defineVar "vec3" v
    foldr1 opIntersection `fmap` mapM (distance3 v') objs
distance3 v (DifferenceR3 0 objs) = do
    v' <- defineVar "vec3" v
    foldl1 opSubtraction `fmap` mapM (distance3 v') objs

distance3 v (Translate3 off obj) =
    distance3 ("(" ++ v ++ ") - " ++ asVec3 off) obj
distance3 v (Scale3 (x, y, z) obj) = do
        r <- distance3 (call "divComponents" [v, asVec3 (x, y, z)]) obj
        return $ "(" ++ r ++ ") * " ++ show (min x $ min y z)
            -- Beware, for non-uniform scaling the use of 'minimum' means we underestimate distances.
            -- See http://jamie-wong.com/2016/07/15/ray-marching-signed-distance-functions/#non-uniform-scaling-and-beyond

distance3 v (Rotate3 (0, 0, a) obj) = distance3 (call "rotateZ" [v, show a]) obj
distance3 v (Rotate3 (0, a, 0) obj) = distance3 (call "rotateY" [v, show a]) obj
distance3 v (Rotate3 (a, 0, 0) obj) = distance3 (call "rotateX" [v, show a]) obj

distance3 v (Mirror3 w obj) = distance3 (call "mirror" [v, asVec3 $ normalized w]) obj

distance3 v (ExtrudeR 0 obj2d h) = do
    v' <- defineVar "vec3" v
    d2 <- distance2 (v' ++ ".xy") obj2d
    return $ call "max" [d2, call "abs" [v' ++ ".z - " ++ show h ++ " * 0.5"] ++ " - " ++ show h ++ " * 0.5"]

distance3 v (RotateExtrude angle round translate rotate obj2d)
    | round == Nothing || round == Just 0
    , Left (0, 0) <- translate
    , Left 0 <- rotate
    , rad <- pi / 180 * min (max angle (-360)) 360 = do
        v' <- defineVar "vec3" v
        let p2d = call "vec2" [call "length" [v'++".xy"], v'++".z"]
        d2 <- distance2 p2d obj2d
        return $ opSubtraction d2 $
            if 0 <= rad
                then (if rad <= pi then opUnion else opIntersection)
                        (call "negativeSpaceY" [v'])
                        (call "negativeSpaceY" [call "rotateZ" [v', show $ rad - pi]])
                else (if -rad <= pi then opUnion else opIntersection)
                        (call "positiveSpaceY" [v'])
                        (call "positiveSpaceY" [call "rotateZ" [v', show $ pi + rad]])

distance3 _ obj = error $ "unimplemented: distance3 of: " ++ show obj

-- 2d distance functions:

distance2 :: String -> SymbolicObj2 -> Writer String

distance2 v (Circle r) = return $ call "sdCircle" [v, show r]

distance2 v (UnionR2 0 objs) = do
    v' <- defineVar "vec2" v
    foldr1 opUnion `fmap` mapM (distance2 v') objs
distance2 v (IntersectR2 0 objs) = do
    v' <- defineVar "vec2" v
    foldr1 opIntersection `fmap` mapM (distance2 v') objs
distance2 v (DifferenceR2 0 objs) = do
    v' <- defineVar "vec2" v
    foldl1 opSubtraction `fmap` mapM (distance2 v') objs

distance2 v (Translate2 off obj) =
    distance2 ("(" ++ v ++ ") - " ++ asVec2 off) obj
distance2 v (Scale2 (x, y) obj) = do
        r <- distance2 (call "divComponents" [v, asVec2 (x, y)]) obj
        return $ "(" ++ r ++ ") * " ++ show (min x y)
-- todo: rotate, mirror

distance2 v (PolygonR 0 points) = do
    (startIndex, endIndex) <- definePolySegs points -- (makePolySegments points)
    return $ call "sdPolygon" [v, show startIndex, show endIndex]

distance2 _ obj = error $ "unimplemented: distance2 of: " ++ show obj

-- exported functions:

polyDefs :: [PolySegment] -> String
polyDefs segs = unlines
    [ "struct PolySeg { vec2 start, end, dir; float size; int index, prev, next; };"
    , if null segs then "PolySeg segs[1];" else
      "PolySeg segs[] = " ++ call "PolySeg[]" (map show segs) ++ ";"]

writeGLSL :: ℝ -> FilePath -> SymbolicObj3  -> IO ()
writeGLSL _ path obj = do
    let obj' = Rotate3 (-pi * 0.5, 0, 0) obj -- because OpenSCAD's orientation doesn't match the shader's
    raymarch <- readFile "raymarch.glsl"
    let (expr, _, Output{..}) = runWriter (distance3 "v0" obj') (State 0 0)
    writeFile path $ polyDefs polySegs ++ raymarch ++ unlines (
        ["float scene(in vec3 v0)", "{"] ++
        map ("  " ++) declarations ++
        ["  return " ++ expr ++ ";", "}"])
