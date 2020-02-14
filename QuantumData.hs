module QuantumData where


class (Eq a, Ord a) => Basis a where
    basis :: [a]

instance Basis Bool where
    basis = [False, True]

newtype FiniteMap a b = FiniteMap [(a, b)]

finiteMap :: FiniteMap a b
finiteMap = FiniteMap []

finiteMapUnit :: a -> b -> FiniteMap a b
finiteMapUnit x y = FiniteMap [(x, y)]

finiteMapAppend :: (a, b) -> FiniteMap a b -> FiniteMap a b
finiteMapAppend t (FiniteMap m) = FiniteMap (t:m)

finiteMapSearch :: (Eq a) => a -> FiniteMap a b -> Maybe b
finiteMapSearch _ (FiniteMap []) = Nothing
finiteMapSearch x (FiniteMap (y:ys))
    | x == key = Just value
    | otherwise = finiteMapSearch x (FiniteMap ys)
    where (key, value) = y


finiteMapSearchDefault :: (Eq a) => FiniteMap a b -> b -> a -> b
finiteMapSearchDefault (FiniteMap []) def _ = def
finiteMapSearchDefault (FiniteMap (y:ys)) def k
    | k == key = value
    | otherwise = finiteMapSearchDefault (FiniteMap ys) def k
    where (key, value) = y

finiteMapSearchDefault' :: (Eq a) => b -> FiniteMap a b -> a -> b
finiteMapSearchDefault' def finmap = finiteMapSearchDefault finmap def

listToFiniteMap :: [(a, b)] -> FiniteMap a b
listToFiniteMap = FiniteMap

data Complex a = Complex a a
re :: Complex a -> a
re (Complex x _) = x

im :: Complex a -> a
im (Complex _ y) = y

complex :: a -> a -> Complex a
complex = Complex

complexZero :: (Num a) => Complex a
complexZero = Complex 0 0

complexOne :: (Num a) => Complex a
complexOne = Complex 1 0

complexOneOnSqrtX :: (Floating a) => a -> Complex a
complexOneOnSqrtX n = Complex (1 / sqrt n) 0

complexOneOnX :: (Floating a) => a -> Complex a
complexOneOnX n = Complex (1 / n) 0

type PA = Complex Double
type QV a = FiniteMap a PA

qv :: (Basis a) => [(a, PA)] -> QV a
qv = listToFiniteMap

pr :: (Basis a) => FiniteMap a PA -> a -> PA
pr fm = finiteMapSearchDefault fm complexZero

qFalse, qTrue, qFT :: QV Bool
qFalse = finiteMapUnit False complexOne
qTrue  = finiteMapUnit True complexOne
qFT    = qv [(False, complexOneOnSqrtX 2), (True, complexOneOnSqrtX 2)]

instance Basis Integer where
    basis = [0..]

qInteger :: QV Integer
qInteger = qv [(i, complexOneOnX $ fromIntegral i) | i <- basis, i /= 0]

instance (Basis a, Basis b) => Basis (a, b) where
    basis = [(a, b) | a <- basis, b <- basis]

p1, p2, p3 :: QV (Bool, Bool)
p1 = qv [((False, False), complexOne), ((False, True), complexOne)]
p2 = qv [((False, False), complexOne), ((True, True), complexOne)]
p3 = qv [ ((False, False), complexOne)
        , ((False, True), complexOne)
        , ((True, False), complexOne)
        , ((True, True), complexOne)
        ]
-- This is amazing <3