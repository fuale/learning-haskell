module Module56 where

import Control.Monad.Reader (Reader, asks)
import Control.Monad.Writer (Writer, runWriter)

type User = String

type Password = String

type UsersTable = [(User, Password)]

-- >>> runReader usersWithBadPasswords [("user", "123456"), ("x", "hi"), ("root", "123456")]
-- ["user","root"]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks (map fst . filter (\x -> "123456" == snd x))

evalWriter :: Writer w a -> a
evalWriter a = fst $ runWriter a
