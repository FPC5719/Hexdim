module Hexdim.Emu where

import Prelude

import Control.Monad.RWS.Lazy
import Control.Lens
import Data.Array
import Data.Maybe
import Data.List (minimumBy)
import Data.Function
import Data.Functor
import Data.Default

import Hexdim.Pipe.Data
import Hexdim.Prim

data Env = Env
  { _pipeState :: PipeState
  , _lastPipeW :: Maybe PipeW
  , _envPC     :: Addr
  , _instrMem  :: Array Addr Wire
  , _dataMem   :: Array Addr Wire
  , _envReg    :: Array Reg Wire
  , _status    :: (Bool, Bool)
  }
makeLenses ''Env

instance Default Env where
  def = Env
    { _pipeState = def
    , _lastPipeW = Nothing
    , _envPC = 0
    , _instrMem = listArray (0, 255) ([0 :: Wire .. 255] $> 0)
    , _dataMem = listArray (0, 255) ([0 :: Wire .. 255] $> 0)
    , _envReg = listArray (0, 3) ([0 :: Wire .. 3] $> 0)
    , _status = (False, False)
    }

emulate :: Env -> IO Env
emulate e = do
  let lastW = fromMaybe
        PipeW { _pcNext    = mempty
              , _instrMemA = mempty
              , _dataMemW  = mempty
              , _ioW       = mempty
              , _regS1     = mempty
              , _regS2     = mempty
              , _regW      = mempty
              , _statusW   = mempty
              }
        (e ^. lastPipeW)
  let lw :: Lens' PipeW (First a) -> Maybe a
      lw l = getFirst $ lastW ^. l

  ioRes <- case lw ioW of
    Nothing -> return 0
    Just (Right _) -> return 0
    Just (Left addr) -> do
      putStrLn $ "Reading IO[" ++ show addr ++ "]"
      read <$> getLine
  
  ((), s', w) <- mfix $ \ ~(_, _, w) -> do
    let mw :: Lens' PipeW (First a) -> Maybe a
        mw l = getFirst $ w ^. l
    let reg' = (e ^. envReg) //
          ( case w ^. regW of
              [] -> []
              xs -> [snd . minimumBy (compare `on` fst) $ xs]
          )
    ((), s', w') <- runRWST (pipeM ())
      PipeR { _cycle0 = False
            , _pc = e ^. envPC
            , _instrMemR = maybe 0 ((e ^. instrMem) !) $ lw instrMemA
            , _dataMemR = case lw dataMemW of
                Nothing -> Nothing
                Just (Right _) -> Nothing
                Just (Left addr) -> Just $ (e ^. dataMem) ! addr
            , _ioR = case lw ioW of
                Nothing -> Nothing
                Just (Right _) -> Nothing
                Just (Left _) -> Just $ ioRes
            , _regR1 = maybe 0 (reg' !) $ mw regS1
            , _regR2 = maybe 0 (reg' !) $ mw regS2
            , _statusR = e ^. status
            }
      (e ^. pipeState)
    return ((), s', w')

  case lw ioW of
    Nothing -> return ()
    Just (Left _) -> return ()
    Just (Right (addr, wire)) -> putStrLn $
      "Writing IO[" ++ show addr ++ "]: " ++ show wire

  let nw :: Lens' PipeW (First a) -> Maybe a
      nw l = getFirst $ w ^. l
  return $ Env
    { _pipeState = s'
    , _lastPipeW = Just w
    , _envPC = case join $ nw pcNext of
        Nothing -> e ^. envPC + 1
        Just pcn -> pcn
    , _instrMem = e ^. instrMem
    , _dataMem = (e ^. dataMem) //
      ( case nw dataMemW of
          Nothing -> []
          Just (Left _) -> []
          Just (Right upd) -> [upd]
      )
    , _envReg = (e ^. envReg) //
      ( case w ^. regW of
          [] -> []
          xs -> [snd . minimumBy (compare `on` fst) $ xs]
      )
    , _status = fromMaybe (e ^. status) (join $ nw statusW)
    }
