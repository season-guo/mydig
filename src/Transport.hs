module Transport(defaultDNS) where

import Control.Exception(try, SomeException)
import Network.Socket(Socket, socket, bind, SockAddr(SockAddrInet), Family(AF_INET), SocketType(Datagram), defaultProtocol, tupleToHostAddress)
import Network.Socket.Address (recvFrom, sendTo)
import qualified Data.ByteString as B
import Message(buildReq ,parseReq, runDecode, Res, isAnswer, rebuildReq, Req, noAdditional, getIPv4sFromAnswer, getIPv4sFromAdditional, getDomainsFromRes)
import Data.Word(Word8)

dnsAddr :: (Word8, Word8, Word8, Word8) -> SockAddr
dnsAddr = SockAddrInet 53 . tupleToHostAddress 

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

handleSend :: Socket -> B.ByteString -> SockAddr -> IO (Either String Int)
handleSend sock msg addr = 
    sendMsg sock msg addr >>= \result ->
    case result of
        Left err -> putStrLn ("Error sending message: " ++ show err) >> return (Left "Failed to send message")
        Right bytesSent -> putStrLn ("Message sent to " ++ show addr) >> return (Right bytesSent)

handleRecv :: Socket -> Int -> IO (Either String B.ByteString)
handleRecv sock len =
    recvMsg sock len >>= \result ->
    case result of
        Left err -> putStrLn ("Error receiving message: " ++ show err) >> return (Left "Failed to receive message")
        Right (msg, addr) -> putStrLn ("Received message from " ++ show addr) >> return (Right msg)

handleMsg :: Socket -> B.ByteString -> SockAddr -> IO (Either String B.ByteString)
handleMsg sock msg addr = do
    sendResult <- handleSend sock msg addr
    case sendResult of
        Left err -> return (Left err)
        Right _ -> handleRecv sock 1024

unwrap :: Either String a -> a
unwrap (Left err) = error $ "Error decoding response: " ++ err
unwrap (Right res) = res

mapUntilGet :: [Either String a] -> (a -> IO (Either String b)) -> IO (Either String b)
mapUntilGet [] _ = return $ Left "try all but failed"
mapUntilGet (x : left) f = 
    case x of 
        Left err -> putStrLn err >> mapUntilGet left f
        Right inner -> do
            result <- f inner
            case result of 
                Left err -> putStrLn err >> mapUntilGet left f
                Right ans -> return (Right ans)


sendUntilGetAns :: Socket -> Res -> Req -> Bool -> IO (Either String Res)
sendUntilGetAns sock res req isTmp = do
    if isAnswer res then
        if isTmp then 
            do
                newMsg <- mapUntilGet (getIPv4sFromAnswer res) (handleMsg sock (parseReq req) . dnsAddr)
                sendUntilGetAns sock (unwrap $ runDecode (unwrap newMsg)) req False
        else return $ Right res
    else if noAdditional res then
        do 
            newSitu <- mapUntilGet (getDomainsFromRes res) (\domain -> handleMsg sock (parseReq (rebuildReq req domain)) rootServerA >>= 
                \result -> case result of
                Left err -> return (Left err)
                Right byte -> return (Right (byte, domain))
                )
            case newSitu of
                Left err -> return (Left err)
                Right (newMsg, domain) -> do
                    let newReq = rebuildReq req domain
                    tmpResSitu <- sendUntilGetAns sock (unwrap $ runDecode newMsg) newReq False
                    case tmpResSitu of
                        Left err -> putStrLn err >> return (Left err)
                        Right tmpRes -> sendUntilGetAns sock tmpRes req True
    else 
        do
            newMsg <- mapUntilGet (getIPv4sFromAdditional res) (handleMsg sock (parseReq req) . dnsAddr)
            sendUntilGetAns sock (unwrap $ runDecode (unwrap newMsg)) req False

rootServerA :: SockAddr
rootServerA = SockAddrInet 53 (tupleToHostAddress(198, 41, 0, 4))

rootServerB :: SockAddr
rootServerB = SockAddrInet 53 (tupleToHostAddress(199, 9, 14, 201))

rootServerC :: SockAddr
rootServerC = SockAddrInet 53 (tupleToHostAddress(192, 33, 4, 12))

rootServerD :: SockAddr
rootServerD = SockAddrInet 53 (tupleToHostAddress(199, 78, 55, 201))

rootServerE :: SockAddr
rootServerE = SockAddrInet 53 (tupleToHostAddress(192, 5, 5, 5))

rootServerF :: SockAddr
rootServerF = SockAddrInet 53 (tupleToHostAddress(192, 112, 36, 4))

rootServers :: [SockAddr]
rootServers = [rootServerA, rootServerB, rootServerC, rootServerD, rootServerE, rootServerF]

defaultServerDirectedDNS :: Req -> SockAddr -> IO (Either String Res)
defaultServerDirectedDNS req addr = do
    sock <- defaultSocket
    initMsg <- handleMsg sock (parseReq req) addr
    sendUntilGetAns sock (unwrap (runDecode $ unwrap initMsg)) req False

defaultDNS :: String -> IO (Either String Res)
defaultDNS domain = mapUntilGet (map Right rootServers) $ defaultServerDirectedDNS (buildReq domain)