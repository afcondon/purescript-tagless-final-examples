module Extended where

import Prelude ((*), (<>))

class MulSYM repr where
    mul :: repr -> repr -> repr

-- extend the interpreter
instance mulsymInt :: MulSYM Int where
    mul e1 e2 = e1 * e2
    
-- ======>>>>> The definition of eval stays the same. Why?
--               The extension _automatically_ kicks in
instance mulsymString :: MulSYM String where
    mul e1 e2 = "(" <> e1 <> " * " <> e2 <> ")"
