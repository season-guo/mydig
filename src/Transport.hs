module Transport(initUDPSocket, bindSocket, recvMsg, sendMsg, rootSeverA, rootServerB) where

import Control.Exception(try, SomeException)
import Network.Socket(Socket, socket, bind, SockAddr(SockAddrInet), Family(AF_INET), SocketType(Datagram), defaultProtocol, tupleToHostAddress)
import Network.Socket.Address (recvFrom, sendTo)
import qualified Data.ByteString as B



initUDPSocket :: IO (Either SomeException Socket)
initUDPSocket = try $ socket AF_INET Datagram defaultProtocol

bindSocket :: Socket -> IO(Either SomeException ())
bindSocket sock = try $ bind sock (SockAddrInet 8080 (tupleToHostAddress (0, 0, 0, 0)))

sendMsg :: Socket -> B.ByteString -> SockAddr -> IO (Either SomeException Int)
sendMsg sock msg addr = try $ sendTo sock msg addr

recvMsg :: Socket -> Int -> IO (Either SomeException (B.ByteString, SockAddr))
recvMsg sock len = try $ recvFrom sock len

rootSeverA :: SockAddr
rootSeverA = SockAddrInet 53 (tupleToHostAddress(198, 41, 0, 4))

rootServerB :: SockAddr
rootServerB = SockAddrInet 53 (tupleToHostAddress(199, 9, 14, 201))

rootServerC :: SockAddr
rootServerC = SockAddrInet 53 (tupleToHostAddress(192, 33, 4, 12))

