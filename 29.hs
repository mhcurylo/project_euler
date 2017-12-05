import qualified Data.Set as S
import Control.Monad (liftM2)

main = print $ length . S.toList . S.fromList $ liftM2 (^) [2..100] [2..100]
