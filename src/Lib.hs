{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( server
    ) where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
import Control.Exception.Base

server :: IO ()
server = do
  chan <- newChan :: IO (Chan (Int, String))
  sock <- socket AF_INET Stream 0

  forkIO $ fix $ \loop -> do
    (_, msg) <- readChan chan
    loop

  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  chatLoop sock chan 0

chatLoop :: Socket -> Chan (Int, String) -> Int -> IO ()
chatLoop s ch clientId = do
  conn <- accept s
  forkIO (serveClient conn ch clientId)
  chatLoop s ch (clientId + 1)

serveClient :: (Socket, SockAddr) -> Chan (Int, String) -> Int -> IO ()
serveClient (s, _) ch clientId = do
  let broadcast msg = writeChan ch (clientId, msg)
  commLine <- dupChan ch
  h <- socketToHandle s ReadWriteMode

  hPutStrLn h "Sup o/\n"

  reader <- forkIO $ fix $ \loop -> do
    (lineId, line) <- readChan commLine
    when (clientId /= lineId) $ hPutStrLn h line
    loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- liftM init (hGetLine h)
    case line of
      "quit" -> hPutStrLn h "Bye!"
      _      -> broadcast line >> loop

  killThread reader
  hClose h
