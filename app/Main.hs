module Main(main) where

import Transport(defaultDNS)
import Prelude

main :: IO ()
main = do
  msg <- defaultDNS "leetcode.cn"
  putStrLn $ "it means :\n" ++ show msg
  return ()