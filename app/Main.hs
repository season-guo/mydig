module Main(main) where

import Transport(initUDPSocket, bindSocket, sendMsg, rootSeverA, recvMsg, rootServerB)
import Message (testFlag, testForFlag, testForHeader, testHeader, testId, testForId, testReq, testForReq, parseReq, runDecode)
import Prelude

main :: IO ()
main = do
  flag <- testForFlag testFlag
  head <- testForHeader testHeader
  id <- testForId testId
  req <- testForReq testReq
  putStrLn id
  putStrLn flag
  putStrLn head 
  putStrLn req
  sock <- initUDPSocket
  case sock of 
    Left err -> do
      putStrLn (show err)
    Right a -> do 
      let sock = a
      putStrLn "init ok"
      two <- bindSocket sock
      case two of
        Left err -> do
          putStrLn (show err)
        Right _ -> do
          putStrLn "bind ok"
          ans <- sendMsg sock (parseReq testReq) rootServerB
          case ans of
            Left err -> do
              putStrLn (show err)
            Right _ -> do
              putStrLn "send ok"
              receive <- recvMsg sock 1024
              case receive of
                Left err -> do
                  putStrLn (show err)
                Right k-> do
                  putStrLn "recv ok"
                  putStrLn ("msg: " ++ show (fst k) ++ " from " ++ show (snd k))
                  putStrLn ("after decode, it means" ++ show (runDecode (fst k)))
              return ()