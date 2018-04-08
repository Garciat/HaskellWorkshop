import System.Directory

import Database.HDBC
import Database.HDBC.Sqlite3

main = do
  let dbFile = "test1.db"
  removeFile dbFile
  conn <- connectSqlite3 dbFile
  run conn "CREATE TABLE test (id INTEGER NOT NULL, desc VARCHAR(80))" []
  run conn "INSERT INTO test (id) VALUES (0)" []
  commit conn
  result <- quickQuery' conn "SELECT * from test where id < 2" []
  print result
  disconnect conn
