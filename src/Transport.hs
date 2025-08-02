module Transport(handleMsg, rootServerA, rootServerB, defaultSocket, sendUntilGetAns) where

import Control.Exception(try, SomeException)
import Network.Socket(Socket, socket, bind, SockAddr(SockAddrInet), Family(AF_INET), SocketType(Datagram), defaultProtocol, tupleToHostAddress)
import Network.Socket.Address (recvFrom, sendTo)
import qualified Data.ByteString as B
import Message(parseReq, runDecode, Res, getIPv4FromAns, getIPv4FromAdditional, isAnswer, getDomainFromRes, rebuildReq, Req, noAdditional)

initUDPSocket :: IO (Either SomeException Socket)
initUDPSocket = try $ socket AF_INET Datagram defaultProtocol

bindSocket :: Socket -> IO(Either SomeException ())
bindSocket sock = try $ bind sock (SockAddrInet 8080 (tupleToHostAddress (0, 0, 0, 0)))

defaultSocket :: IO Socket
defaultSocket = do
    sock <- initUDPSocket
    case sock of
        Left err -> error $ "Failed to initialize socket: " ++ show err
        Right s -> do
            bindResult <- bindSocket s
            case bindResult of
                Left err -> error $ "Failed to bind socket: " ++ show err
                Right _ -> return s

sendMsg :: Socket -> B.ByteString -> SockAddr -> IO (Either SomeException Int)
sendMsg sock msg addr = try $ sendTo sock msg addr

recvMsg :: Socket -> Int -> IO (Either SomeException (B.ByteString, SockAddr))
recvMsg sock len = try $ recvFrom sock len

handleSend :: Socket -> B.ByteString -> SockAddr -> IO ()
handleSend sock msg addr = 
    sendMsg sock msg addr >>= \result ->
    case result of
        Left err -> putStrLn $ "Error sending message: " ++ show err
        Right _ -> putStrLn $ "Message sent to " ++ show addr 

handleRecv :: Socket -> Int -> IO (B.ByteString)
handleRecv sock len =
    recvMsg sock len >>= \result ->
    case result of
        Left err -> putStrLn ("Error receiving message: " ++ show err) >> error "Failed to receive message"
        Right (msg, addr) -> putStrLn ("Received message from " ++ show addr) >> return msg

handleMsg :: Socket -> B.ByteString -> SockAddr -> IO B.ByteString
handleMsg sock msg addr = do
    handleSend sock msg addr
    handleRecv sock 1024

sendUntilGetAns :: Socket -> Res -> Req -> Bool -> IO Res
sendUntilGetAns sock res req isTmp = do
    if isAnswer res then
        if isTmp then 
            do
                let addr = SockAddrInet 53 (tupleToHostAddress $ getIPv4FromAns res)
                newMsg <- handleMsg sock (parseReq req) addr
                sendUntilGetAns sock (runDecode newMsg) req False
        else return res
    else if noAdditional res then
        do 
            let domain = getDomainFromRes res
            let newReq = rebuildReq req domain
            newMsg <- handleMsg sock (parseReq newReq) rootServerA
            tmpRes <- sendUntilGetAns sock (runDecode newMsg) newReq False
            sendUntilGetAns sock tmpRes req True
    else 
        do
            let addr = SockAddrInet 53 (tupleToHostAddress $ getIPv4FromAdditional res)
            newMsg <- handleMsg sock (parseReq req) addr
            sendUntilGetAns sock (runDecode newMsg) req False


rootServerA :: SockAddr
rootServerA = SockAddrInet 53 (tupleToHostAddress(198, 41, 0, 4))

rootServerB :: SockAddr
rootServerB = SockAddrInet 53 (tupleToHostAddress(199, 9, 14, 201))


