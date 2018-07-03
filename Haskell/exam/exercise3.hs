--repfor ::Integer -> a -> b -> c
--repfor 1 f x = f x
--repfor n f x = f (repfor n-1 f x)

repfor2 n f x = iterate f x !! n

