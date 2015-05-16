import System.Random
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.MVar

import Data.Word
import Data.Functor
import qualified Data.Set as S
import Data.PQueue.Prio.Min

-- PQueue stuff
newtype Key = Key (Int,Bool)
              deriving (Eq,Show,Ord)

newtype IP4 = IP4 { unIP4 :: Word32 }
              deriving (Eq,Show,Ord)

loIP = IP4 0x00000000
hiIP = IP4 0x00000010

instance Random IP4 where
  random g = let (a,g') = random g
             in  (IP4 a, g')
  randomR (lo,hi) g = let (a,g') = randomR (unIP4 lo, unIP4 hi) g
                      in  (IP4 a, g')

type IP4PQ = MinPQueue Key IP4
type IP4Set = S.Set IP4

startKey :: Key
startKey = Key (0,False)


--
--
main :: IO ()
main = do
  q <- atomically $ newTBQueue 10

  _ <- forkIO (builder q)

  forever $ do
    val <- atomically $ readTBQueue q
    print val
    --
    threadDelay (10^6)


builder :: TBQueue IP4 -> IO ()
builder q = do
  mv <- newMVar (empty,S.empty) :: IO (MVar (IP4PQ,IP4Set))
  _ <- forkIO (printer mv)
  forever $ do
    g <- getStdGen
    let (ip,g') = randomR (loIP,hiIP) g
    setStdGen g'
    --
    (pq,set) <- takeMVar mv
    let (pq',set')    = if S.member ip set
                           then (pq, set)
                           else (insert startKey ip pq, S.insert ip set)
        (minIP,pq'') = bumpMin pq'
    putMVar mv (pq'',set')
    --
    atomically $ do
      e <- isEmptyTBQueue q
      when e $ writeTBQueue q minIP
    --
    threadDelay (10^5)

bumpMin :: IP4PQ -> (IP4,IP4PQ)
bumpMin pq = (a,pq'')
  where
    ((k,a), pq') = deleteFindMin pq
    pq'' = insert (bumpKey k) a pq'
    --
    bumpKey (Key (s,b)) = if b
                             then Key (s+1,True)
                             else Key (s  ,True)

printer :: MVar (IP4PQ,IP4Set) -> IO ()
printer mv = forever $ do
  (pq,set) <- readMVar mv
  putStrLn "\n-------PQ     --------"
  mapM_ print (toList pq)
  putStrLn "------ End PQ --------\n"
  --
  threadDelay (5 * 10^6)

