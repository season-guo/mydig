{-# LANGUAGE InstanceSigs #-}
module Message(parseReq, runDecode, Res, getIPv4FromAdditional, getIPv4FromAns, buildReq, isAnswer, getReq, setReqNoRecursive, Req, noAdditional, getDomainFromRes, rebuildReq) where

import Data.Bits
import Data.Word (Word16, Word8, Word32)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Builder as Builder
import Control.Monad.State(get, put, StateT,  runStateT, lift)
import Numeric (showHex)
import qualified Data.Map as Map

data Id = Id Word16
    deriving Show

data Flag = Flag Word16

newtype QDcount = QDcount Word16
    deriving Show

newtype ANcount = ANcount Word16
    deriving Show

newtype NScount = NScount Word16
    deriving Show

newtype ARcount = ARcount Word16
    deriving Show

parseId :: Id -> B.ByteString
parseId (Id i) = wordToByte i 

type ErrStateT = StateT (B.ByteString, (Word16, Map.Map Word16 String)) (Either String)

handleErr :: Either String a -> ErrStateT a
handleErr = either (lift . Left) return

decodeId :: ErrStateT (Flag -> QDcount -> ANcount -> NScount -> ARcount -> Header)
decodeId = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> return (Header $ Id (byteToword $ B.take 2 byte))

testId :: Id
testId = Id 1
testForId :: Id -> IO(String)
testForId = return . show . parseId

class HeaderSection f where
    empty :: f 
    getBits :: f -> Word16
    changeCode :: (Word16 -> Int -> Word16) -> [Int] -> f -> f 
    setCode :: [Int] -> f -> f 
    setCode = changeCode setBit
    clearCode :: [Int] -> f  -> f 
    clearCode = changeCode clearBit
    checkCode :: Int -> f -> Bool
    checkCode n p = testBit (getBits p) n

instance HeaderSection Flag  where
    empty = Flag 0
    getBits (Flag i) = i
    changeCode op xs f = Flag $ foldl op (getBits f) xs 

instance Show Flag where
    show (Flag f) = "Flag: " ++ showHex f ""

wordToByte :: Word16 -> B.ByteString
wordToByte = B.toStrict . Builder.toLazyByteString . Builder.word16BE

word8ToByte :: Word8 -> B.ByteString
word8ToByte = B.toStrict . Builder.toLazyByteString . Builder.word8

byteToword :: B.ByteString -> Word16
byteToword b = case B.unpack b of
    [a, b] -> (fromIntegral a `shiftL` 8) .|. (fromIntegral b)
    _ -> 0

byteToword32 :: B.ByteString -> Word32
byteToword32 b = case B.unpack b of
    [a, b, c, d] -> (fromIntegral a `shiftL` 24) .|. (fromIntegral b `shiftL` 16) .|. (fromIntegral c `shiftL` 8) .|. (fromIntegral d)
    _ -> 0

setReq :: Flag -> Flag
setReq = clearCode [15]

setNormalQuery :: Flag -> Flag
setNormalQuery = clearCode [14, 13, 12, 11]

setBackQuery :: Flag -> Flag
setBackQuery = setCode [14] . clearCode [13, 12, 11]

setRecursive :: Flag -> Flag
setRecursive = setCode [8]

setNoRecursive :: Flag -> Flag
setNoRecursive = clearCode [8]

getIsFlagRecursive :: Flag -> Bool 
getIsFlagRecursive = checkCode 8

defaultFlag :: Flag
defaultFlag = setReq . setNormalQuery . setNoRecursive $ empty

defaultRecursiveFlag :: Flag 
defaultRecursiveFlag = setReq . setNormalQuery . setRecursive $ empty

parseFlag :: Flag -> B.ByteString
parseFlag (Flag f) = wordToByte f

testForFlag :: Flag -> IO(String)
testForFlag = return . show . parseFlag

testFlag :: Flag
testFlag = Flag 1

decodeFlag :: (Flag -> QDcount -> ANcount -> NScount -> ARcount -> Header) -> ErrStateT (QDcount -> ANcount -> NScount -> ARcount -> Header) 
decodeFlag f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> return (f $ Flag (byteToword $ B.take 2 byte))

data Header = Header {tid :: Id, flag :: Flag, qdCount :: QDcount, anCount :: ANcount, nsCount :: NScount, arCount :: ARcount}

instance Show Header where
    show (Header i f qd an ns ar) =
       "--------------------------" ++ "\nHeader:\n" ++ show i ++ "\n" ++ show f ++ "\n" ++ show qd ++ "\n" ++ show an ++ "\n" ++ show ns ++ "\n" ++ show ar ++ "\n"

getIsRecursive :: Header -> Bool
getIsRecursive = getIsFlagRecursive . flag

getODcount :: Header -> Word16
getODcount h = case qdCount h of QDcount i -> i

getAnscount :: Header -> Word16
getAnscount (Header _ _ _ (ANcount i) _ _) = i 

getNscount :: Header -> Word16
getNscount (Header _ _ _ _ (NScount i) _) = i

getArcount :: Header -> Word16 
getArcount (Header _ _ _ _ _ (ARcount i)) = i

decodeQDcount :: (QDcount -> ANcount -> NScount -> ARcount -> Header) -> ErrStateT (ANcount -> NScount -> ARcount -> Header)
decodeQDcount f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> return (f (QDcount (byteToword $ B.take 2 byte)))

decodeANcount :: (ANcount -> NScount -> ARcount -> Header) -> ErrStateT (NScount -> ARcount -> Header)
decodeANcount f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> return (f (ANcount (byteToword $ B.take 2 byte)))

decodeNScount :: (NScount -> ARcount -> Header) -> ErrStateT (ARcount -> Header)
decodeNScount f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> return (f (NScount (byteToword $ B.take 2 byte)))

decodeARcount :: (ARcount -> Header) -> ErrStateT Header
decodeARcount f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> return (f (ARcount (byteToword $ B.take 2 byte)))

parseHeader :: Header -> B.ByteString
parseHeader h = B.concat [parseId $ tid h ,parseFlag $ flag h, wordToByte $ getODcount h, wordToByte $ case anCount h of ANcount i -> i, wordToByte $ case nsCount h of NScount i -> i, wordToByte $ case arCount h of ARcount i -> i]

decodeHeader :: ErrStateT  Header
decodeHeader = decodeId >>= decodeFlag >>= decodeQDcount >>= decodeANcount >>= decodeNScount >>= decodeARcount 

data Question = Question String QRtype QRclass

instance Show Question where
    show (Question domain qtype qclass) = "--------------------------" ++ "\nQuestion:\nDomain: " ++ domain ++ "\nType: " ++ show qtype ++ "\nClass: " ++ show qclass ++ "\n"

data QRtype = A | NS | CName | SOA | MX | TXT | AAAA | OPT | PRSIG | NSEC | DNSKEY | AXFR | MAILB | MAILA | ANY 
    deriving Show

data QRclass = IN | CH | HS
    deriving Show

parseDomain :: String -> B.ByteString
parseDomain domain = (B.concat $ map (\s ->  word8ToByte (fromIntegral (length s)) <> BC.pack s) (words $ map (\c -> if c == '.' then ' ' else c) domain)) <> word8ToByte 0

decodeDomainPart :: ErrStateT String
decodeDomainPart = get >>= \(byte, (nowlen, domainMap)) ->
    let len = fromIntegral $ B.head byte 
        domainPart = BC.unpack $ B.take (fromIntegral len) $ B.tail byte
    in put (B.drop (fromIntegral $ len + 1) byte, (nowlen + len + 1, domainMap)) >> return domainPart

decodeOneDomain :: ErrStateT String
decodeOneDomain = get >>= \(_, (nowlen, _)) -> decodeDomainPart >>= \part -> 
    if null part then return "" 
    else decodeOneDomain >>= \f -> get >>= \(byte, (nowlen', domainMap)) ->
        put (byte, (nowlen', Map.insert (nowlen - 1) (part ++ "." ++ f) domainMap)) >> return (part ++ "." ++ f)

decodeDomain :: ErrStateT (QRtype -> QRclass -> Question)
decodeDomain = decodeOneDomain >>= return . Question

parseQRtype :: QRtype -> Word16 
parseQRtype A = 0x0001
parseQRtype NS = 0x0002
parseQRtype CName = 0x0005
parseQRtype SOA = 0x0006
parseQRtype MX = 0x000F
parseQRtype TXT = 0x0010    
parseQRtype AAAA = 0x001C
parseQRtype OPT = 0x0029
parseQRtype PRSIG = 0x002E
parseQRtype NSEC = 0x002F
parseQRtype DNSKEY = 0x0030
parseQRtype AXFR = 0x00FC
parseQRtype MAILB = 0x00FD
parseQRtype MAILA = 0x00FE
parseQRtype ANY = 0x00FF

preDecodeQRtype :: B.ByteString -> Either String QRtype
preDecodeQRtype b = case byteToword b of
    0x0001 -> Right A
    0x0002 -> Right NS
    0x0005 -> Right CName
    0x0006 -> Right SOA
    0x000F -> Right MX
    0x0010 -> Right TXT
    0x001C -> Right AAAA
    0x0029 -> Right OPT
    0x002E -> Right PRSIG
    0x002F -> Right NSEC
    0x0030 -> Right DNSKEY
    0x00FC -> Right AXFR
    0x00FD -> Right MAILB
    0x00FE -> Right MAILA
    0x00FF -> Right ANY
    _ -> Left $ "Unknown QRtype: " ++ show (byteToword b)

decodeQRtype :: (QRtype -> QRclass -> Question) -> ErrStateT (QRclass -> Question)
decodeQRtype f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> handleErr (preDecodeQRtype $ B.take 2 byte) >>= return . f

parseQRclass :: QRclass -> Word16
parseQRclass IN = 0x0001
parseQRclass CH = 0x0003
parseQRclass HS = 0x0004

preDecodeQRclass :: B.ByteString -> Either String QRclass
preDecodeQRclass b = case byteToword b of
    0x0001 -> Right IN
    0x0003 -> Right CH
    0x0004 -> Right HS
    _      -> Left $ "Unknown QRclass: " ++ show (byteToword b)

decodeQRclass :: (QRclass -> Question) -> ErrStateT Question
decodeQRclass f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> handleErr (preDecodeQRclass $ B.take 2 byte) >>= return . f

parseQuestion :: Question -> B.ByteString
parseQuestion (Question domain qtype qclass) = B.concat [parseDomain domain, wordToByte $ parseQRtype qtype, wordToByte $ parseQRclass qclass]

decodeQuestion :: ErrStateT (Question)
decodeQuestion = decodeDomain >>= decodeQRtype >>= decodeQRclass

data Req = Req Header Question

instance Show Req where
    show (Req h que) = show h ++ show que

parseReq :: Req -> B.ByteString
parseReq (Req h que) = B.concat [parseHeader h, parseQuestion que]

testHeader :: Header
testHeader = Header (Id 1) defaultRecursiveFlag (QDcount 1) (ANcount 0) (NScount 0) (ARcount 0)

testForHeader :: Header -> IO(String)
testForHeader = return . show . parseHeader

testReq :: Req
testReq = Req testHeader $ Question "leetcode.cn" A IN

buildReq :: String -> Req
buildReq domain = Req testHeader $ Question domain A IN

rebuildReq :: Req -> String -> Req 
rebuildReq (Req h _) domain = Req h (Question domain A IN)

setReqNoRecursive :: Req -> Req
setReqNoRecursive (Req h que) = Req (h {flag = setNoRecursive $ flag h}) que

testForReq :: Req -> IO String
testForReq = return . show . parseReq

data IPv4 = IPv4 Word8 Word8 Word8 Word8
data IPv6 = IPv6 Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16

instance Show IPv4 where
    show :: IPv4 -> String
    show (IPv4 a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d

instance Show IPv6 where
    show :: IPv6 -> String
    show (IPv6 a b c d e f g h) = showHex a "" ++ ":" ++ showHex b "" ++ ":" ++ showHex c "" ++ ":" ++ showHex d "" ++ ":" ++ showHex e "" ++ ":" ++ showHex f "" ++ ":" ++ showHex g "" ++ ":" ++ showHex h ""

decodeIPv4 :: ErrStateT IPv4
decodeIPv4 = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 4 byte, (nowlen + 4, domainMap)) >>
    case B.unpack (B.take 4 byte) of
        [a, b, c, d] -> return $ IPv4 a b c d
        _            -> handleErr $ Left "Invalid IPv4 address"

decodeIPv6 :: ErrStateT IPv6
decodeIPv6 = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 16 byte, (nowlen + 16, domainMap)) >>
    case map byteToword $ [B.take 2 byte, B.take 2 $ B.drop 2 byte, B.take 2 $ B.drop 4 byte, B.take 2 $ B.drop 6 byte, B.take 2 $ B.drop 8 byte, B.take 2 $ B.drop 10 byte, B.take 2 $ B.drop 12 byte, B.take 2 $ B.drop 14 byte] of
        [a, b, c, d, e, f, g, h] -> return $ IPv6 a b c d e f g h
        _                        -> handleErr $ Left "Invalid IPv6 address"

decodeNS :: ErrStateT String
decodeNS = decodeOneAnsDomain

data RData = ARecord IPv4 | NSRecord String | AAAARecord IPv6 | CNameRecord String
    deriving Show

data AnsDomain = Pointer String | Normal String

data Answer = Answer String QRtype QRclass Word32 Word16 RData

instance Show Answer where
    show (Answer domain rtype rclass ttl rdLen rdata) = 
       "\nAnswer:\nDomain: " ++ domain ++ "\nType: " ++ show rtype ++ "\nClass: " ++ show rclass ++ "\nTTL: " ++ show ttl ++ "\nRData Length: " ++ show rdLen ++ "\nRData: " ++ show rdata

checkPtr :: B.ByteString -> Bool
checkPtr byte = testBit (B.head byte) 7 && testBit (B.head byte) 6

subPtr :: Word16 -> Word16
subPtr x = x - 0xC000

decodeAnsDomainPart :: ErrStateT AnsDomain
decodeAnsDomainPart = get >>= \(byte, (nowlen, domainMap)) -> 
    if not $ checkPtr byte then decodeDomainPart >>= return . Normal
    else put (B.drop 2 byte, (nowlen + 2, domainMap)) >> handleErr (
        case Map.lookup (subPtr $ byteToword $ B.take 2 byte) domainMap 
            of Just domain -> Right $ Pointer domain
               Nothing -> Left $ "Pointer to unknown domain at position " ++ show (subPtr $ byteToword $ B.take 2 byte) ++ "\nand now map is" ++ show domainMap
        )

decodeOneAnsDomain :: ErrStateT String
decodeOneAnsDomain = get >>= \(_, (nowlen, _)) -> decodeAnsDomainPart >>= \part -> 
    case part of
        Normal domain -> 
            if null domain then return "" 
            else decodeOneAnsDomain >>= \f -> get >>= \(byte, (nowlen', domainMap)) ->
                put (byte, (nowlen', Map.insert (nowlen - 1) (domain ++ "." ++ f) domainMap)) >> return (domain ++ "." ++ f)
        Pointer domain -> return domain

decodeRdata :: QRtype -> (RData -> Answer) -> ErrStateT Answer
decodeRdata rtype f = case rtype of
    A -> decodeIPv4 >>=  return . f . ARecord
    NS -> decodeNS >>= return . f . NSRecord
    AAAA -> decodeIPv6 >>= return . f . AAAARecord
    CName -> decodeOneAnsDomain >>= return . f . CNameRecord
    _ -> error "Unsupported RData type"
    

decodeAnsDomain :: ErrStateT (QRtype -> QRclass -> Word32 -> Word16 -> RData -> Answer)
decodeAnsDomain = decodeOneAnsDomain >>= return . Answer

decodeTTL :: (Word32 -> Word16 -> RData -> Answer) -> ErrStateT (Word16 -> RData -> Answer)
decodeTTL f = get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 4 byte, (nowlen + 4, domainMap)) >> return (f (byteToword32 $ B.take 4 byte))

decodeAns :: ErrStateT Answer
decodeAns = decodeAnsDomain >>= \f ->
    get >>= \(byte, (nowlen, domainMap)) -> put (B.drop 2 byte, (nowlen + 2, domainMap)) >> 
    return (preDecodeQRtype $ B.take 2 byte) >>= handleErr >>= \rtype -> return (f rtype) >>= \g -> get 
    >>= \(byte', (nowlen', domainMap')) -> put (B.drop 2 byte', (nowlen' + 2, domainMap')) >> handleErr (preDecodeQRclass $ B.take 2 byte') >>= return . g
    >>= decodeTTL >>= \k -> get >>= \(byte'', (nowlen'', domainMap'')) -> put (B.drop 2 byte'', (nowlen'' + 2, domainMap'')) >> return (byteToword (B.take 2 byte''))
    >>= \rdLen -> return (k rdLen) >>= decodeRdata rtype

data ResourceRecord = AnsSection [Answer] | AuthoritySection [Answer] | AdditionalSection [Answer]
    
instance Show ResourceRecord where
    show (AnsSection answers) = "--------------------------\nAnswers:\n" ++ unlines (map show answers)
    show (AuthoritySection answers) = "--------------------------\nAuthorities:\n" ++ unlines (map show answers)
    show (AdditionalSection answers) = "--------------------------\nAdditionals:\n" ++ unlines (map show answers)

data Res = Res Req ResourceRecord ResourceRecord ResourceRecord
    deriving Show

decodeAnswers :: Int -> ErrStateT [Answer]
decodeAnswers 0 = return []
decodeAnswers n = decodeAns >>= \ans -> decodeAnswers (n - 1) >>= \rest -> return (ans : rest)

decodeRes :: ErrStateT Res  
decodeRes = decodeHeader >>= \header -> 
    decodeQuestion >>= \question ->
    decodeAnswers (fromIntegral $ getAnscount header) >>= 
    return . (Res (Req header question)) . AnsSection >>= \f ->
    decodeAnswers (fromIntegral $ getNscount header) >>=
    return . (f . AuthoritySection) >>= \g ->
    decodeAnswers (fromIntegral $ getArcount header) >>=
    return . (g . AdditionalSection) >>= return

runDecode :: B.ByteString -> Either String Res
runDecode bs = runStateT (decodeRes) (bs, (1, Map.empty)) >>= return . fst

onlyShowResource :: Res -> String
onlyShowResource (Res _ ans auth add) = show ans ++ show auth ++ show add

getIPv4FromResource :: [Answer] -> Either String (Word8, Word8, Word8, Word8)
getIPv4FromResource answers =
    case answers of
        [] -> Left "No answers found"
        Answer _ A _ _ _ (ARecord (IPv4 a b c d)) : _ -> Right (a, b, c, d)
        _ : left -> getIPv4FromResource left
        _ -> Left "No A record found in answers section"

getIPv4FromAns :: Res -> Either String (Word8, Word8, Word8, Word8) 
getIPv4FromAns (Res _ (AnsSection answers) _ _) = getIPv4FromResource answers

getIPv4FromAdditional :: Res -> Either String (Word8, Word8, Word8, Word8)
getIPv4FromAdditional (Res _ _ _ (AdditionalSection answers)) = getIPv4FromResource answers

getDomainFromRes :: Res -> Either String String
getDomainFromRes (Res _ _ authPart _) =
    case authPart of
        AuthoritySection [] -> Left "No authority section found"
        AuthoritySection (Answer _ _ _ _ _ (NSRecord domain) : _) -> Right domain
        AuthoritySection (Answer _ _ _ _ _ (CNameRecord domain) : _) -> Right domain
        _ -> Left "No CNAME record found in authority section"

isAnswer :: Res -> Bool 
isAnswer (Res _ (AnsSection answers) _ _) = not $ null answers

noAdditional :: Res -> Bool
noAdditional (Res _ _ _ (AdditionalSection [])) = True
noAdditional _ = False

getReq :: Res -> B.ByteString
getReq (Res req _ _ _) =  parseReq req