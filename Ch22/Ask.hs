module Ask where

import Ch22.Reader

ask :: Reader a a
ask = Reader $ \r -> r