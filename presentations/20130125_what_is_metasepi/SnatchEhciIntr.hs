import Control.Monad
import Control.Monad.State
import Data.Bits
import Foreign.Ptr

data UsbdBus = UsbdBus -- xxx
data SoftContext = SC { scBus :: UsbdBus
                      , iot :: Int
                      , ioh :: Int
                      , scOffs :: Int
                      , scEintrs :: Int
                      , scAsyncHead :: Ptr Int
                      , scIntrXfer :: Ptr Int }

type BusSpace m a = StateT SoftContext m a
type Addr = Int

ehciUsbsts, ehciUsbIntr :: Addr
ehciUsbsts  = 0x04
ehciUsbIntr = 0x08
ehciStsIaa, ehciStsPcd, ehciStsErrInt, ehciStsInt :: Int
ehciStsIaa    = 0x00000020
ehciStsPcd    = 0x00000004
ehciStsErrInt = 0x00000002
ehciStsInt    = 0x00000001

evalTmpl :: Int -> (SoftContext -> IO a) -> Int -> BusSpace IO Int
evalTmpl flag io ei | ei .&. flag /= 0 = go
                    | otherwise = return ei
  where go = do sc <- get
                liftIO . io $ sc
                return $ ei .&. complement flag

evalWakeup, evalSoftIntr, evalPcd :: Int -> BusSpace IO Int
evalWakeup = evalTmpl ehciStsIaa $ wakeUp . scAsyncHead
evalSoftIntr = evalTmpl (ehciStsErrInt .|. ehciStsInt) (usbSchedSoftIntr . scBus)
evalPcd = evalTmpl ehciStsPcd (\sc -> ehciPcd sc . scIntrXfer $ sc)

evalWrite :: Int -> BusSpace IO ()
evalWrite ei = when (ei /= 0) go
  where go = do sc <- get
                let newEi = scEintrs sc .&. complement ei
                put $ sc {scEintrs = newEi}
                busSpaceOwrite4 ehciUsbIntr newEi

evaluateIntr1 :: BusSpace IO ()
evaluateIntr1 =
  do intrs <- fmap ehciStsIntrs $ busSpaceOread4 ehciUsbsts
     sc <- get
     let eintrs = intrs .&. scEintrs sc
     busSpaceOwrite4 ehciUsbsts eintrs
     evalWrite =<< evalPcd =<< evalSoftIntr =<< evalWakeup eintrs
     return ()
       where ehciStsIntrs r = r .&. 0x3f

ehciIntr1 :: SoftContext -> IO (Either () SoftContext)
ehciIntr1 sc = return . Right =<< execStateT evaluateIntr1 sc

wakeUp :: Ptr Int -> IO ()
wakeUp = undefined
usbSchedSoftIntr :: UsbdBus -> IO ()
usbSchedSoftIntr = undefined
ehciPcd :: SoftContext -> Ptr Int -> IO ()
ehciPcd = undefined
busSpaceOread4 :: Addr -> BusSpace m Int
busSpaceOread4 = undefined
busSpaceOwrite4 :: Addr -> Int -> BusSpace m ()
busSpaceOwrite4 = undefined
