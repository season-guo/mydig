module Main(main) where

import Transport(handleMsg, rootServerB, defaultSocket, sendUntilGetAns, check)
import Message (buildReq ,parseReq, runDecode, setReqNoRecursive)
import Prelude

main :: IO ()
main = do
  let testReq = setReqNoRecursive $ buildReq "www.example.com" 
  sock <- defaultSocket
  initMsg <- handleMsg sock (parseReq testReq) rootServerB
  msg <- sendUntilGetAns sock (check $ runDecode initMsg) testReq False
  putStrLn $ "it means :\n" ++ show msg
  return ()