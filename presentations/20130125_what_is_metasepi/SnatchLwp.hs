data Lflag = Lflag { lwIdle        :: Bool 
                   , lwLwpctl      :: Bool 
                   , lwSintr       :: Bool
                   , lwSaSwitching :: Bool
                   , lwSystem      :: Bool
                   , lwSa          :: Bool
                   , lwWsuspend    :: Bool }
data Lstat = LsIdl | LsRun | LsSleep | LsStop | LsZomb | LsOnProc 
           | LsSuspended

data Lwp = Lwp { lflag :: Lflag
               , lstat :: Lstat
                 {-- ...... --} }

data ErrNo = Eperm | Enoent | Esrch | Eintr | Eio | Enxio | E2big 
           | Enoexec | Ebadf | Echild | Edeadlk

lwpSuspend :: Lwp -> Lwp -> IO (Either ErrNo ())
lwpSuspend curl t = go $ lstat t
  where go LsRun       = fRunOnproc
        go LsOnProc    = fRunOnproc
        go LsSleep     = do lwpSetFlag lwWsuspend
                            if lwSintr . lflag $ t
                              then setRunnable t
                              else lwpUnlock t
                            return $ Right ()
        go LsSuspended = do lwpUnlock t
                            return $ Right ()
        go LsStop      = do lwpSetFlag lwWsuspend
                            setRunnable t
                            return $ Right ()
        go LsIdl       = fIdlZomb
        go LsZomb      = fIdlZomb
        fRunOnproc     = do lwpSetFlag lwWsuspend
                            lwpNeedUserret t
                            lwpUnlock t
                            return $ Right ()
        fIdlZomb       = do lwpUnlock t
                            return $ Left Eintr

setRunnable = undefined
lwpSetFlag = undefined
lwpNeedUserret = undefined
lwpUnlock = undefined
