module Matrix ( Matrix
              , repeat
              , get
              , set
              , toIndexedList
              , map
              , mapAt
              , mapAtMany
              , mapAtMany'
              , getMany
              , setMany
              , diff
              ) where

import Array exposing (Array)


type alias Matrix a = Array (Array a)


repeat : Int -> Int -> a -> Matrix a
repeat w h e = Array.repeat h (Array.repeat w e)


get : Int -> Int -> Matrix a -> Maybe a
get x y a =
  let
    row = Array.get x a
  in
    case row of
      Nothing -> Nothing
      Just row -> Array.get y row


set : Int -> Int -> a -> Matrix a -> Matrix a
set x y e a =
  let
    row = Array.get x a
  in
    case row of
      Nothing -> a
      Just row -> Array.set x (Array.set y e row) a


update : Int -> Int -> (a -> a) -> Matrix a -> Matrix a
update x y f a =
  let
    value = get x y a
  in
    case value of
      Nothing -> a
      Just e -> set x y (f e) a


-- given list of indexes, return list of objects in corresponding cells
getMany : List (Int, Int) -> Matrix a -> List a
getMany is a = List.filterMap identity
               <| List.foldl (\(x,y) acc -> get x y a::acc) [] is


-- given list of indexes and list of objects,
-- update multiple cells of an array
setMany : List (Int, Int) -> List a -> Matrix a -> Matrix a
setMany is os a =
  let
    step ((x,y),o) acc = set x y o acc -- stupid foldl arguments
  in
    List.foldl step a (List.map2 (,) is os)


toIndexedList : Matrix a -> List (Int, Int, a)
toIndexedList = flatten << indexedMap (\x y e -> (y,x,e))


map : (a -> b) -> Matrix a -> Matrix b
map f a = Array.map (Array.map f) a


indexedMap : (Int -> Int -> a -> b) -> Matrix a -> Matrix b
indexedMap f a =
  Array.indexedMap (\y row -> Array.indexedMap (\x e -> f x y e) row) a


-- apply function at index
mapAt : (a -> a) -> Int -> Int -> Matrix a -> Matrix a
mapAt f x y a =
  let
    e = get x y a
  in
    case e of
      Nothing -> a
      Just e -> set x y (f e) a


mapAtMany : (a -> a) -> List (Int, Int) -> Matrix a -> Matrix a
mapAtMany f is a =
  let
    step (x,y) acc =
      case get x y a of
        Nothing -> acc
        Just e -> set x y (f e) acc
  in
    List.foldl step a is


-- Takes list of indexes and list of objects;
-- combines old object under index with new object using function.
mapAtMany' : (a -> b -> a) -> List (Int, Int) -> List b -> Matrix a -> Matrix a
mapAtMany' f is os a =
  let
    step ((x,y),o) acc =
      case get x y a of
        Nothing -> acc
        Just e -> set x y (f e o) acc
  in
    List.foldl step a (List.map2 (,) is os)

-- remove all elements from both lists that don't satisfy the predicate,
-- then apply the function
filterMap2 : (a -> b -> Bool) -> (a -> b -> c) -> List a -> List b -> List c
filterMap2 p f xs ys = List.map (uncurry f)
                       <| List.filter (uncurry p) (List.map2 (,) xs ys)


-- return indexes of cells that are not equal
diff : Matrix a -> Matrix a -> List (Int, Int)
diff a1 a2 =
  let
    il1 = toIndexedList a1
    il2 = toIndexedList a2
    filteredList = filterMap2 (/=)
                   (\(x,y,e) _ -> (x,y))
                   il1 il2
  in
    filteredList


flatten : Matrix a -> List a
flatten = List.concat << toList


flattenArray : Matrix a -> Array a
flattenArray = Array.foldl Array.append Array.empty


toList : Matrix a -> List (List a)
toList = List.map Array.toList << Array.toList


getRow : Int -> Matrix a -> Maybe (Array a)
getRow n a = Array.get n a


getCol : Int -> Matrix a -> Maybe (Array a)
getCol n a = maybeSequenceArray <| Array.map (Array.get n) a


width : Matrix a -> Int
width a =
  let
    row = Array.get 0 a
  in
    case row of
      Nothing -> 0
      Just row -> Array.length row


height : Matrix a -> Int
height a = Array.length a


size : Matrix a -> (Int, Int)
size a = (width a, height a)


-- sequence on Maybe monad
-- a helper function
maybeSequence : List (Maybe a) -> Maybe (List a)
maybeSequence = listTraverse identity


listTraverse : (a -> Maybe b) -> List a -> Maybe (List b)
listTraverse f =
  let
    step e acc =
      case f e of
        Nothing -> Nothing
        Just x -> Maybe.map ((::)x) acc
  in
    List.foldr step (Just [])


listAp : List (a -> b) -> List a -> List b
listAp fs xs =
  List.concatMap (\x -> List.map (\f -> f x) fs) xs


maybeSequenceArray : Array (Maybe a) -> Maybe (Array a)
maybeSequenceArray = arrayTraverse identity


arrayTraverse : (a -> Maybe b) -> Array a -> Maybe (Array b)
arrayTraverse f =
  let
    step e acc =
      case f e of
        Nothing -> Nothing
        Just x -> Maybe.map (Array.push x) acc
  in
    Array.foldl step (Just Array.empty)
