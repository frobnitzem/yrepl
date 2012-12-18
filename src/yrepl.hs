{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import Data.IORef
import System.Console.Readline
import Data.Time.Clock.POSIX
import Data.Functor
import System.IO

--import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

{- Input Listners
 -   heavily based on MVars (instead of 'repeaters')
 -      which are written using IO callbacks invoking putMVar
 -      and read with tryTakeMVar
 -   Each listner should setup an MVar and return a function
 -   that fills that mvar into an input structure.
 -}
--readlineL :: (String->IO()) -> String -> IO()
--readlineL write line = do
--    write line -- <- its only critical function.
-- readlineL = apply

data Sensors = Sensors {
    cmdLine :: !(Maybe String),
    err     :: !(Maybe String)
}

initSensor = Sensors{ cmdLine=Nothing, err=Nothing }

newRepeater :: IO(IO(Maybe a), a -> IO())
newRepeater = do
    var <- newEmptyMVar
    let read = tryTakeMVar var
        save = putMVar var
    return (read, save)

{- Create the sense function and return a pair
 - of a cleanup function and the sense function.
 -
 - The work is done following Peterson, Hudak, and Elliott's
 - Lambda in Motion
 - 1. setup communication MVars
 - 2. Initialize callbacks with write end of MVars
 - 3. Write a 'sense' function with the read end of the MVars.
 -}
mkSense :: IO(IO(), Bool -> IO (DTime, Maybe Sensors))
mkSense = do
    (cmdRead, cmdWrite) <- newRepeater
    clean <- callbackHandlerInstall ";] " cmdWrite
    updTime <- setupTimer

    let sense = \ blocks -> do
        callbackReadChar
        cmd <- cmdRead
        let sensors = initSensor{
            cmdLine = cmd
        }
        dt <- updTime
        return (dt, Just sensors)
    return (clean, sense)

-- Differentiates the posix clock to make Yampa happy.
updTimer :: IORef POSIXTime -> IO(DTime)
updTimer timeRef = do
    t  <- readIORef timeRef
    t' <- getPOSIXTime
    writeIORef timeRef t'
    return $ realToFrac (t - t')

-- Sets up the IORef to be used by updTimer
setupTimer :: IO(IO(DTime))
setupTimer = do
    timer <- newIORef (0 :: POSIXTime)
    let updTime = updTimer timer
    return updTime


prompt :: IO()
prompt = do
    getPrompt >>= putStr
    hFlush stdout

{- Setting up the output path is not as much of an issue
 - but should try either to act like a DSL for the controller
 - or to expose the IO() monad action, e.g. [IO()]
 -}

data Control = Control {
    msgC       :: !(Maybe String),
    otherIO    :: !(IO()),
    newPrompt  :: !Bool,
    doneC      :: !Bool
}

initControl = Control{ msgC=Nothing, otherIO=return(), newPrompt=False, doneC=False }

actuate :: Bool -> Control -> IO Bool
-- actuate False _ = return () -- ignore unchanged status?
actuate _ ctl = do
    case msg of
        Just s -> do putStrLn s
        Nothing -> return ()

    otherIO ctl

    if (not done) && (newPrompt ctl)
            then prompt
            else return()

    return done where
        done = doneC ctl
        msg  = msgC ctl

{- Internals of the signal function
 - more to code here
 -}

-- Bool indicates if program ends, nothing indicates
-- a noop.
parseCmd :: Maybe String -> Control
parseCmd Nothing = initControl
parseCmd (Just line) =
    case words line of
       ("quit":args) -> initControl{
			    msgC = Just "See ya",
			    doneC = True
			}
       (cmd:args) -> initControl{
				msgC = Just $ "cmd = "++cmd,
				otherIO = addHistory line,
				newPrompt = True
			}
       _ -> initControl{ newPrompt = True }

sf :: SF Sensors Control -- The signal function to be run
sf = arr cmdLine >>> arr parseCmd

{-
reactimate :: IO a                          -- init
           -> (Bool -> IO (DTime, Maybe a)) -- input/sense
           -> (Bool -> b -> IO Bool)        -- output/actuate
           -> SF a b                        -- process/signal function
           -> IO ()
-}

main :: IO()
main = do
    hSetBuffering stdin NoBuffering
    setAlreadyPrompted True
    -- setup events
    (clean, sense) <- mkSense
    reactimate (do prompt; return initSensor)
               sense actuate sf
    clean

