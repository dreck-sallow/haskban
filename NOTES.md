* Apply a pure function to a IO value:

> We use ´<$>´ that do this work, and return the IO result from the right expresion
> think ´<$>´ Apply a pure function to unwrapped value and wrap the result to IO

´´´hs
handleArgs :: IO FilePath
handleArgs =
head <$> Env.getArgs -- get the firt element from a IO [String]
´´´


* Concat IO actions in sequence with ´>>=´. Reading the previous (IO Result)
