module Main(main) where

import Transport(handleMsg, rootServerB, defaultSocket, sendUntilGetAns)
import Message (buildReq ,parseReq, runDecode, setReqNoRecursive)
import Prelude

main :: IO ()
main = do
  let testReq = setReqNoRecursive $ buildReq "leetcode.cn" 
  sock <- defaultSocket
  initMsg <- handleMsg sock (parseReq testReq) rootServerB
  msg <- sendUntilGetAns sock (runDecode initMsg) testReq False
  putStrLn $ "it means :\n" ++ show msg
  return ()