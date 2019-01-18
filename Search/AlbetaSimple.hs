negScore  :: Monad m => Int -> m Int
negScore v = return (-v)

negScore2 :: Monad m => (Int, [Move]) -> m (Int, [Move])
negScore2 (v, mvs) = return (-v, mvs)

research :: Monad m => Int -> (Int -> Bool) -> m (Int, [Move]) -> m (Int, [Move])
research s p m = if p s then m else return (s, [])

pvSearch :: Monad m => Int -> Int -> Int -> (Int, [Move])
pvSearch a b 0 = pvQSearch a b >>= \s -> m (s, [])
pvSearch a b d = do
    mvs <- genMoves
    (best, pv) <- loopState (a, pv, True) mvs $ \a pv bSearchPv m -> do
        make m
        let full = pvSearch (-b) (-a) (d-1) >>= negScore2
            null = zwSearch (-a) (d-1) >>= negScore
        (sc, pv) <- if bSearchPv
                       then full
                       else null >>= research (> a) full
        unmake
        if (sc > b)
           then return $ Left b
           else if sc > a
                   then return $ Right (sc, False)
                   else return $ Right (a, True)