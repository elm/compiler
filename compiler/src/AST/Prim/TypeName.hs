{-# LANGUAGE MagicHash, QuasiQuotes #-}
module AST.Prim.TypeName
  ( Name
  , nameSize
  , nameToChars
  , nameToBuilder
  , nameToName
  , nameToString
  , nameToByteArray
  --
  , nameFromAddr
  , nameFromString
  , nameFromByteArray
  --
  , bool, char, int, float, string
  , maybe, result, list, array, never
  , program, cmd, sub, task, router
  , vec2, vec3, vec4, mat4, texture, shader
  , value, node
  --
  , encode
  , decode
  )
  where


import Prelude hiding (maybe, sequence)
import qualified Data.ByteString.Builder as B
import GHC.Prim (Addr#, ByteArray#, coerce)

import qualified Bytes.Decode as D
import qualified Bytes.Encode as E
import String (ascii)
import qualified String as S

import qualified AST.Prim.Name as N



-- LOCAL TYPE NAMES
--
-- Name type names like Bool, Maybe, Result, etc.
--


newtype Name =
  Name S.String


nameSize :: Name -> Int
nameSize (Name s) =
  S.size s


nameToChars :: Name -> [Char]
nameToChars (Name s) =
  S.toChars s


nameToBuilder :: Name -> B.Builder
nameToBuilder (Name s) =
  S.toBuilder s


nameToName :: Name -> N.Name
nameToName s =
  N.fromString (coerce s)


nameToString :: Name -> S.String
nameToString (Name s) =
  s


nameToByteArray :: Name -> ByteArray#
nameToByteArray (Name (S.String ba)) =
  ba


nameFromAddr :: Addr# -> Addr# -> IO Name
nameFromAddr pos end =
  Name <$> S.fromAddr pos end


nameFromString :: S.String -> Name
nameFromString =
  coerce


nameFromByteArray :: ByteArray# -> Name
nameFromByteArray ba =
  Name (S.String ba)



-- INSTANCES


instance Eq Name where
  (==) (Name s1) (Name s2) =
    S.equal s1 s2


instance Ord Name where
  compare (Name s1) (Name s2) =
    S.compareFast s1 s2



-- NAMES


{-# NOINLINE bool    #-}; bool    :: Name; bool    = Name [ascii|Bool|]
{-# NOINLINE char    #-}; char    :: Name; char    = Name [ascii|Char|]
{-# NOINLINE int     #-}; int     :: Name; int     = Name [ascii|Int|]
{-# NOINLINE float   #-}; float   :: Name; float   = Name [ascii|Float|]
{-# NOINLINE string  #-}; string  :: Name; string  = Name [ascii|String|]
{-# NOINLINE maybe   #-}; maybe   :: Name; maybe   = Name [ascii|Maybe|]
{-# NOINLINE result  #-}; result  :: Name; result  = Name [ascii|Result|]
{-# NOINLINE list    #-}; list    :: Name; list    = Name [ascii|List|]
{-# NOINLINE array   #-}; array   :: Name; array   = Name [ascii|Array|]
{-# NOINLINE never   #-}; never   :: Name; never   = Name [ascii|Never|]
{-# NOINLINE program #-}; program :: Name; program = Name [ascii|Program|]
{-# NOINLINE cmd     #-}; cmd     :: Name; cmd     = Name [ascii|Cmd|]
{-# NOINLINE sub     #-}; sub     :: Name; sub     = Name [ascii|Sub|]
{-# NOINLINE task    #-}; task    :: Name; task    = Name [ascii|Task|]
{-# NOINLINE router  #-}; router  :: Name; router  = Name [ascii|Router|]

{-# NOINLINE vec2    #-}; vec2    :: Name; vec2    = Name [ascii|Vec2|]
{-# NOINLINE vec3    #-}; vec3    :: Name; vec3    = Name [ascii|Vec3|]
{-# NOINLINE vec4    #-}; vec4    :: Name; vec4    = Name [ascii|Vec4|]
{-# NOINLINE mat4    #-}; mat4    :: Name; mat4    = Name [ascii|Mat4|]
{-# NOINLINE texture #-}; texture :: Name; texture = Name [ascii|Texture|]
{-# NOINLINE shader  #-}; shader  :: Name; shader  = Name [ascii|Shader|]

{-# NOINLINE value   #-}; value   :: Name; value   = Name [ascii|Value|]
{-# NOINLINE node    #-}; node    :: Name; node    = Name [ascii|Node|]



-- ENCODE/DECODE


encode :: Name -> B.Builder
encode (Name s) =
  E.string8 s


decode :: D.Decoder Name
decode =
  Name <$> D.string8

