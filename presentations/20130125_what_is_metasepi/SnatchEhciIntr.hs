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

busSpaceOread4 :: Addr -> BusSpace m Int
busSpaceOread4 = undefined
busSpaceOwrite4 :: Addr -> Int -> BusSpace m ()
busSpaceOwrite4 = undefined

ehciStsIntrs :: Int -> Int
ehciStsIntrs r = r .&. 0x3f

evalWakeup :: Int -> BusSpace IO Int
evalWakeup ei | ei .&. flag /= 0 = go
              | otherwise = return ei
  where flag = ehciStsIaa
        go = do sc <- get
                liftIO . wakeUp . scAsyncHead $ sc
                return $ ei .&. complement flag

evalSoftIntr :: Int -> BusSpace IO Int
evalSoftIntr ei | ei .&. flag /= 0 = go
                | otherwise = return ei
  where flag = ehciStsErrInt .|. ehciStsInt
        go = do sc <- get
                liftIO . usbSchedSoftIntr . scBus $ sc
                return $ ei .&. complement flag

evalPcd :: Int -> BusSpace IO Int
evalPcd ei | ei .&. flag /= 0 = go
           | otherwise = return ei
  where flag = ehciStsPcd
        go = do sc <- get
                liftIO . ehciPcd sc . scIntrXfer $ sc
                return $ ei .&. complement flag

evalWrite :: Int -> BusSpace IO Int
evalWrite ei | ei /= 0 = go
             | otherwise = return ei
  where go = do sc <- get
                let newEi = scEintrs sc .&. complement ei
                put $ sc {scEintrs = newEi}
                busSpaceOwrite4 ehciUsbIntr newEi
                return newEi

evaluateIntr1 :: BusSpace IO ()
evaluateIntr1 =
  do intrs <- fmap ehciStsIntrs $ busSpaceOread4 ehciUsbsts
     sc <- get
     let eintrs = intrs .&. scEintrs sc
     busSpaceOwrite4 ehciUsbsts eintrs
     evalWrite =<< evalPcd =<< evalSoftIntr =<< evalWakeup eintrs
     return ()

ehciIntr1 :: SoftContext -> IO (Either () SoftContext)
ehciIntr1 sc = return . Right =<< execStateT evaluateIntr1 sc

wakeUp :: Ptr Int -> IO ()
wakeUp = undefined
usbSchedSoftIntr :: UsbdBus -> IO ()
usbSchedSoftIntr = undefined
ehciPcd :: SoftContext -> Ptr Int -> IO ()
ehciPcd = undefined
