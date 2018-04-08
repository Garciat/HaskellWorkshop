import Data.Maybe
import System.IO

import Network.HTTP
import Network.URI


downloadURL :: String -> IO (Either String String)
downloadURL url = do
    result <- simpleHTTP $ getRequest url
    case result of
        Left err -> return $ Left ("Error connecting: " ++ show err)
        Right resp -> 
            case rspCode resp of
            (2,_,_) -> return $ Right (rspBody resp)
            (3,_,_) -> -- A HTTP redirect
                case findHeader HdrLocation resp of
                    Nothing -> return $ Left (show resp)
                    Just url -> downloadURL url
            _ -> return $ Left (show resp)


main = do
    Right text <- downloadURL "http://example.com"
    putStrLn text
