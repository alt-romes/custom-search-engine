{-# LANGUAGE BlockArguments #-}

import CustomSearchEngine
import Cob.UserM
import Cob

main :: IO ()
main = do
  [host,user,pass] <- lines <$> readFile "cob-credentials.secret"
  x <- umSession host user pass
  runCob x loop

  where
    loop :: Cob ()
    loop = do

      inp <- io do
        putStrLn "Input a link or a query or 'import':"
        getLine

      case inp of

        "import" -> do

          file <- io do
            putStrLn "Input a file path for bookmarks exported from Firefox:"
            getLine

          importFromFirefoxExport file



        link@('h':'t':'t':'p':_) -> do
          _    <- addLink link
          pure ()

        query -> do
          results <- searchFor query
          io (mapM_ print results)

      io (putStrLn "Done.")

      loop



io = liftCob
