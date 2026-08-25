{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, TemplateHaskell, UnboxedTuples #-}
module Graph
  ( Sorted(..)
  --
  , SCC(..)
  , toSCC
  --
  , Node(..)
  --
  , Component
  , withComponent
  --
  , MinimalCycle(..)
  , isSelfRecursive
  --
  , RootSelector(..)
  , withMinimalCycle
  )
  where


import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.Graph as G
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC.Base (isTrue#)
import GHC.Exts (Int(..))
import GHC.Prim
import GHC.ST (ST(ST), runST)
import GHC.Word (Word32(..))

import qualified Crash



-- SORTED
--
-- Explicitly label lists that have been topologically sorted with the toSCC
-- function. This is useful for type aliases, which can use a much smaller
-- binary format if stored in topological order.
--


newtype Sorted a =
  Sorted [a]



-- STRONGLY CONNECTED COMPONENTS


data SCC k v
  = Acyclic (Node k v)
  | Cyclic (Component k v)


toSCC :: Ord k => [Node k v] -> [SCC k v]
toSCC nodes =
    List.map toComponent (G.scc graph)
  where
    (graph, vertices) = toGraph nodes

    toComponent (G.Node index trees) =
      case trees of
        [] ->
          if List.elem index (graph ! index)
          then Cyclic (Component (vertices!index) [])
          else Acyclic (vertices!index)

        _ ->
          Cyclic (Component (vertices!index) (List.foldr addTree [] trees))

    addTree (G.Node i ts) vs =
      (vertices!i) : List.foldr addTree vs ts



-- NODES


data Node k v =
  Node
    { _key :: k
    , _value :: v
    , _edges :: [k]
    }


instance Functor (Node k) where
  fmap f (Node k v e) = Node k (f v) e


toGraph :: Ord k => [Node k v] -> (G.Graph, Array Int (Node k v))
toGraph nodes =
    (graph, vertices)
  where
    vmax     = List.length nodes - 1
    bounds0  = (0,vmax)
    inodes   = List.zipWith (,) [0..vmax] (List.sortBy compareKey nodes)

    graph    = Array.array bounds0 $ List.map (fmap toInternalEdges) inodes
    vertices = Array.array bounds0 inodes

    toInternalEdges (Node _ _ edges) =
      Maybe.mapMaybe (lookupIndex vertices vmax) edges


lookupIndex :: Ord k => Array Int (Node k v) -> Int -> k -> Maybe Int
lookupIndex vertices vmax key =
    findVertex 0 vmax
  where
    findVertex lo hi =
      if lo > hi
      then Nothing
      else
        case compare key (_key (vertices ! mid)) of
          LT -> findVertex lo (mid-1)
          EQ -> Just mid
          GT -> findVertex (mid+1) hi
        where
          mid = lo + (hi - lo) `div` 2


compareKey :: (Ord k) => Node k v -> Node k v -> Ordering
compareKey (Node k1 _ _) (Node k2 _ _) =
  compare k1 k2



-- COMPONENTS
--
-- Components definitely contain cycles, but they may contain many of them.
-- So care is needed when displaying cycles for error messages.


data Component k v =
  Component (Node k v) [Node k v]


withComponent :: Component k v -> (Node k v -> [Node k v] -> r) -> r
withComponent (Component x xs) cont =
  cont x xs


instance Functor (Component k) where
  fmap f (Component n ns) = Component (fmap f n) (fmap (fmap f) ns)



-- TO MINIMAL CYCLE
--
-- Must account for cases where the SCC is not a simple cycle. For example:
--
--     ----<---
--    /        \
--   A -> B --> D
--    \        /
--     --> C --
--
-- In such cases, it makes sense to just show a simple cycle.


data MinimalCycle a =
  MinimalCycle a [a]


isSelfRecursive :: MinimalCycle a -> Bool
isSelfRecursive (MinimalCycle _ xs) =
  case xs of
    []  -> True
    _:_ -> False


newtype RootSelector k v root =
  RootSelector (Node k v -> [Node k v] -> (k, root))


withMinimalCycle :: (Ord k) => RootSelector k v root -> Component k v -> (Node k v -> a) -> (root -> MinimalCycle a -> r) -> r
withMinimalCycle (RootSelector selector) (Component node nodes) extract cont =
  runST $ ST $ \s0 ->
    case newByteArray# size                    s0 of { (# s1, backEdges #) ->
    case setByteArray# backEdges 0# size 0xFF# s1 of {    s2               ->
    case bfs backEdges (Queue [root] [])       s2 of { (# s3, I# final  #) ->
    case toPath backEdges final []             s3 of { (# s4, (i,is)    #) ->
      (# s4, cont rootInfo (MinimalCycle (finalize i) (List.map finalize is)) #)
    }}}}
  where
    !size = 4# *# len
    !(I# len) = 1 + List.length nodes

    (graph, vertices) = toGraph (node:nodes)

    (rootKey, rootInfo) = selector node nodes
    !root@(I# root#) = find rootKey vertices

    finalize i =
      extract (vertices ! i)

    bfs backEdges queue s0 =
      let
        (i, queue1) = dequeue queue
        edges = graph ! i
      in
      if List.elem root edges
      then (# s0, i #)
      else
        case enqueue backEdges i edges queue1 s0 of
          (# s1, queue2 #) -> bfs backEdges queue2 s1

    toPath backEdges i path s0 =
      case readWord32Array# backEdges i s0 of
        (# s1, w #) ->
          case word2Int# (word32ToWord# w) of
            j ->
              if isTrue# (j ==# root#)
              then (# s1, (root, I# i : path) #)
              else toPath backEdges j (I# i : path) s1


data Queue =
  Queue [Int] [Int]


enqueue :: MutableByteArray# s -> Int -> [Int] -> Queue -> State# s -> (# State# s, Queue #)
enqueue backEdges i edges0 (Queue front back0) s =
    loop edges0 back0 s
  where
    !(W32# i#) = fromIntegral i

    loop edges back s0 =
      case edges of
        [] ->
          (# s0, Queue front back #)

        j@(I# j#) : js ->
          case readWord32Array# backEdges j# s0 of
            (# s1, prev #) ->
              case prev of
                0xFFFFFFFF#Word32 ->
                  case writeWord32Array# backEdges j# i# s1 of
                    s2 -> loop js (j:back) s2

                _ ->
                  loop js back s1


dequeue :: Queue -> (Int, Queue)
dequeue (Queue front back) =
  case front of
    [] ->
      case List.reverse back of
        []   -> $(Crash.crash 'withMinimalCycle) "these nodes are not a Strongly Connected Component"
        x:xs -> (x, Queue xs [])

    x:xs ->
      (x, Queue xs back)


find :: (Eq k) => k -> Array.Array Int (Node k v) -> Int
find key vertices =
    loop zero
  where
    (zero, len) = Array.bounds vertices

    loop i =
      if i < len
      then
        if _key (vertices ! i) == key
        then i
        else loop (i + 1)
      else
        $(Crash.crash 'withMinimalCycle) "bug in RootSelector when calculating minimal cycle"

