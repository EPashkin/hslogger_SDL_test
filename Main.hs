{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Main where


import Graphics.UI.SDL as SDL
import Data.Word
import Control.Monad.State.Lazy
import Control.Monad.Reader
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter


data AppData = AppData {
    x :: Int,
    y :: Int
}

appData :: Int -> Int -> AppData
appData x y = AppData x y 

data AppConfig = AppConfig {
    screen      :: Surface
}

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

type AppState = StateT AppData IO
newtype AppEnv a = AppEnv { runEnv :: ReaderT AppConfig AppState a }
    deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppData)


main :: IO ()
main = do
    removeAllHandlers
    setupLogger "./a.log"
    info "Starting."
    withInit [InitEverything] $ do
        info "Starting2."
        (config, state) <- Main.init
        runApp config state $ do
            render     -- initial render
            loop
        return ()
    info "Finished."
    finish

init :: IO (AppConfig, AppData)
init = do
    screen  <- setVideoMode 640 480 32 [HWSurface, DoubleBuf]
    setCaption "Test" []
    enableUnicode True
    
    let posX = 320
    let posY = 240
    return (AppConfig screen, appData posX posY)

loop :: AppEnv ()
loop = do
    event <- liftIO waitEvent
    case event of
        Quit -> return ()
        _ -> do
            handleEvent event
            loop

runApp :: AppConfig -> AppData -> AppEnv a -> IO (a, AppData)
runApp config state (AppEnv env) = (runStateT . runReaderT env) config state

render :: AppEnv ()
render = do
    screen <- getScreen
    liftIO $ clear screen 0x00 0x00 0x00
    drawRects
    liftIO $ SDL.flip screen

drawRects :: AppEnv ()
drawRects = do
    screen  <- getScreen
    AppData x y <- get
    let rect = Rect x y 10 10 
    liftIO $ drawRect screen rect 255 255 0

handleEvent :: Event -> AppEnv ()
handleEvent VideoExpose = render
handleEvent (MouseButtonDown mx my ButtonLeft) = do
    liftIO $ notice $ show x ++ "x"++ show y
    put $ appData x y
    pushEvent_ VideoExpose
--    return ()
 where x = fromIntegral mx
       y = fromIntegral my
       pushEvent_ = liftIO . pushEvent

handleEvent _ = return ()

clear :: Surface -> Word8 -> Word8 -> Word8 -> IO ()
clear surf r g b = do
    jrect      <- Just `liftM` getClipRect surf
    clearColor <- mapRGB' surf r g b
    fillRect surf jrect clearColor
    return ()

mapRGB' :: Surface -> Word8 -> Word8 -> Word8 -> IO Pixel
mapRGB' = mapRGB . surfaceGetPixelFormat

drawRect :: Surface -> Rect -> Word8 -> Word8 -> Word8 -> IO ()
drawRect surf rect r g b = do
    color <- mapRGB' surf r g b
    fillRect surf (Just rect) color
    return ()

logFormat = "$utcTime $prio $loggername: $msg"
loggerName = "hsloggerSDLtest"

defaultFormatter = simpleLogFormatter logFormat

setupLogger logFileName = do
    handler <- fileHandler logFileName INFO
    let handler' = setFormatter handler defaultFormatter
    updateGlobalLogger loggerName $ addHandler handler'
    updateGlobalLogger loggerName $ setLevel INFO
    
finish = removeAllHandlers

{-
info mes = return ()
warning mes = return ()
debug mes = return ()
error mes = return ()
notice mes = return ()
-}
info = infoM loggerName
warning = warningM loggerName
debug = debugM loggerName
error = errorM loggerName
notice = noticeM loggerName
