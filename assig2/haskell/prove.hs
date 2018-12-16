import Ex1

instance Monad ListBag where
  return m lb = lb
  (f lb) (>>=) (g lb)
main = do
