{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Main where


import Graphics.UI.SDL as SDL
import Data.Word
import Control.Monad.State.Lazy
import Control.Monad.Reader


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
    withInit [InitEverything] $ do
        (config, state) <- Main.init
        runApp config state $ do
            render     -- initial render
            loop
        return ()

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
