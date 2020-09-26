module Responder.Repeat
    ( Config(..)
    , withRepeatResponder
    ) where


import Control.Monad
import Data.IORef
import Data.List
import qualified Data.HashTable.IO as IOTable
import qualified Data.Text as Text
import qualified Channel
import qualified Logger
import qualified Responder


data Config
    = Config
        { cDefaultMultiplier :: Int
        , cUnknownCommandMsg :: Text.Text
        , cStartCmd :: Text.Text
        , cStartMsg :: Text.Text
        , cDescribeCmd :: Text.Text
        , cDescribeMsg :: Text.Text
        , cInspectMultiplierCmd :: Text.Text
        , cInspectMultiplierMsg :: Text.Text
        , cMultiplierSetMsg :: Text.Text
        , cMediaUnknownTypeMsg :: Text.Text
        , cMediaUnsupportedMsg :: Text.Text
        , cMediaInternalErrorMsg :: Text.Text
        , cMaxMultiplier :: Int }
    deriving (Show)


withRepeatResponder :: Config -> Logger.Handle -> Channel.Handle -> (Responder.Handle -> IO r) -> IO r
withRepeatResponder conf logger channel body = do
    er <- erNew conf logger channel
    body $ Responder.Handle
        { Responder.work = erWork er }


data RepeatResponder
    = RepeatResponder
        { erConfig :: Config
        , erLogger :: Logger.Handle
        , erChannel :: Channel.Handle
        , erContextTable :: IOTable.CuckooHashTable Channel.ChatId ChatContext }


data ChatContext
    = ChatContext
        { ccMultiplier :: IORef (Maybe Int) }


erNew :: Config -> Logger.Handle -> Channel.Handle -> IO RepeatResponder
erNew conf logger channel = do
    ctab <- IOTable.new
    return $ RepeatResponder
        { erConfig = conf
        , erLogger = logger
        , erChannel = channel
        , erContextTable = ctab }


erWork :: RepeatResponder -> IO ()
erWork er = do
    events <- Channel.poll (erChannel er)
    mapM_ (erHandleEvent er) events


erHandleEvent :: RepeatResponder -> Channel.Event -> IO ()
erHandleEvent er (Channel.EventMessage chatId _ content@(Channel.RichTextSpan (Channel.SpanStyle False False False False) text Channel.RichTextEnd)) = do
    if Text.isPrefixOf "/" text
        then erHandleCommand er chatId text
        else erRepeatMessage er chatId content
erHandleEvent er (Channel.EventMessage chatId _ content) = do
    erRepeatMessage er chatId content
erHandleEvent er (Channel.EventMedia chatId caption mediaList) = do
    erRepeatMedia er chatId caption mediaList
erHandleEvent er (Channel.EventQuery chatId messageId queryId userdata) = do
    erMakeRequest er "answerQuery" $
        Channel.answerQuery (erChannel er) queryId ""
    case Text.stripPrefix "r" userdata of
        Just mt
            | (mult, _):_ <- reads (Text.unpack mt)
            , 1 <= mult && mult <= cMaxMultiplier (erConfig er) -> do
                erSetMultiplier er chatId mult
                erMakeRequest er "updateMessage" $
                    Channel.updateMessage (erChannel er) chatId messageId
                        (Channel.plainText $ substitute [show mult] $ cMultiplierSetMsg (erConfig er))
                        []
        _ -> return ()


erRepeatMessage :: RepeatResponder -> Channel.ChatId -> Channel.RichText -> IO ()
erRepeatMessage er chatId content = do
    mult <- erGetMultiplier er chatId
    Logger.debug (erLogger er) $
        "Responder: repeat a message into " <> Text.pack (show chatId) <> " " <> Text.pack (show mult) <> " times"
    replicateM_ mult $ do
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId content []


erRepeatMedia :: RepeatResponder -> Channel.ChatId -> Text.Text -> [Channel.ForeignMedia] -> IO ()
erRepeatMedia er chatId caption inGroup = do
    mult <- erGetMultiplier er chatId
    Logger.debug (erLogger er) $
        "Responder: repeat media into " <> Text.pack (show chatId) <> " " <> Text.pack (show mult) <> " times"
    possessionResult <- forM inGroup $ Channel.possessMedia (erChannel er) chatId
    let sendableGroup = selectSuccess possessionResult
    let unknownTypes = sort $ nub $ selectUnknownType possessionResult
    replicateM_ mult $ do
        erMakeRequest er "sendMedia" $
            Channel.sendMedia (erChannel er) chatId caption sendableGroup
    forM_ unknownTypes $ \typename -> do
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ substitute [Text.unpack typename] $ cMediaUnknownTypeMsg (erConfig er))
                []
    when (containsUnsupported possessionResult) $ do
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ cMediaUnsupportedMsg (erConfig er))
                []
    when (containsInternalError possessionResult) $ do
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ cMediaInternalErrorMsg (erConfig er))
                []
    where
    selectSuccess [] = []
    selectSuccess (Channel.PossessMediaSuccess outMedia:rest) = outMedia:selectSuccess rest
    selectSuccess (_:rest) = selectSuccess rest
    selectUnknownType [] = []
    selectUnknownType (Channel.PossessMediaUnknownType typename:rest) = typename:selectUnknownType rest
    selectUnknownType (_:rest) = selectUnknownType rest
    containsUnsupported [] = False
    containsUnsupported (Channel.PossessMediaUnsupported:_) = True
    containsUnsupported (_:rest) = containsUnsupported rest
    containsInternalError [] = False
    containsInternalError (Channel.PossessMediaInternalError:_) = True
    containsInternalError (_:rest) = containsInternalError rest


erHandleCommand :: RepeatResponder -> Channel.ChatId -> Text.Text -> IO ()
erHandleCommand er chatId cmd
    | Text.isPrefixOf (cStartCmd (erConfig er)) cmd = do
        mult <- erGetMultiplier er chatId
        Logger.debug (erLogger er) $
            "Responder: requested start in " <> Text.pack (show chatId)
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ substitute [show mult] $ cStartMsg (erConfig er))
                []
    | Text.isPrefixOf (cDescribeCmd (erConfig er)) cmd = do
        mult <- erGetMultiplier er chatId
        Logger.debug (erLogger er) $
            "Responder: requested description in " <> Text.pack (show chatId)
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ substitute [show mult] $ cDescribeMsg (erConfig er))
                []
    | Text.isPrefixOf (cInspectMultiplierCmd (erConfig er)) cmd = do
        mult <- erGetMultiplier er chatId
        Logger.debug (erLogger er) $
            "Responder: requested multiplier in " <> Text.pack (show chatId) <> " (currently " <> Text.pack (show mult) <> ")"
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ substitute [show mult] $ cInspectMultiplierMsg (erConfig er))
                multiplierButtons
    | otherwise = do
        Logger.debug (erLogger er) $
            "Responder: unknown command in " <> Text.pack (show chatId)
        erMakeRequest er "sendMessage" $
            Channel.sendMessage (erChannel er) chatId
                (Channel.plainText $ cUnknownCommandMsg (erConfig er))
                []
    where
    multiplierButtons = do
        map
            (\i -> Channel.QueryButton (Text.pack (show i)) (Text.pack ("r" ++ show i)))
            [1 .. cMaxMultiplier (erConfig er)]


erGetMultiplier :: RepeatResponder -> Channel.ChatId -> IO Int
erGetMultiplier er chatId = do
    mcc <- IOTable.lookup (erContextTable er) chatId
    case mcc of
        Nothing -> return $ cDefaultMultiplier (erConfig er)
        Just cc -> do
            mmult <- readIORef (ccMultiplier cc)
            case mmult of
                Nothing -> return $ cDefaultMultiplier (erConfig er)
                Just mult -> return mult


erSetMultiplier :: RepeatResponder -> Channel.ChatId -> Int -> IO ()
erSetMultiplier er chatId mult = do
    cc <- erProvideChatContext er chatId
    writeIORef (ccMultiplier cc) $ Just mult
    Logger.info (erLogger er) $
        "Responder: set multiplier in " <> Text.pack (show chatId) <> " to " <> Text.pack (show mult)


erProvideChatContext :: RepeatResponder -> Channel.ChatId -> IO ChatContext
erProvideChatContext er chatId = do
    mcc <- IOTable.lookup (erContextTable er) chatId
    case mcc of
        Nothing -> do
            cc <- ChatContext
                <$> newIORef Nothing
            IOTable.insert (erContextTable er) chatId cc
            Logger.info (erLogger er) $
                "Responder: allocate a new context in " <> Text.pack (show chatId)
            return $ cc
        Just cc -> do
            return $ cc


erMakeRequest :: RepeatResponder -> Text.Text -> IO (Either Text.Text a) -> IO ()
erMakeRequest er reqname act = do
    r <- act
    case r of
        Right _ -> return ()
        Left err -> do
            Logger.warn (erLogger er) $
                "Responder: " <> reqname <> " failed: " <> err


substitute :: [String] -> Text.Text -> Text.Text
substitute arglist format = do
    let first:rest = Text.splitOn "%" format
    Text.concat $ first:walk rest
    where
    walk ("":p:ps) = "%":p:walk ps
    walk (p:ps) = do
        case Text.uncons p of
            Just (c, rest)
                | '1' <= c && c <= '9' -> arg (fromEnum c - fromEnum '1'):rest:walk ps
                | otherwise -> rest:walk ps
            Nothing -> walk ps
    walk [] = []
    arg n = do
        case drop n arglist of
            x:_ -> Text.pack x
            _ -> ""
