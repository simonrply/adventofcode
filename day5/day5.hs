import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8 as B
import Data.List as L

listDoorId = [ (genericIndex x 5, genericIndex x 6) | i <- [0..], let x = show  $ md5 $ B.pack ("abbhdwsy" ++ show i), L.isPrefixOf "00000" x]