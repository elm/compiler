
module Values where

import Dict

main = constant . plainText . show . values $ empty

