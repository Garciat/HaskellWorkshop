import Network.Wai.Handler.Warp (run)

import HaskellWorkshop.App (app)


main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app
