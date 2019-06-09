import Control.Monad.Identity
import Control.Monad.Writer

diga : a -> Writer (List a) ()
diga a = tell [a]

f : String -> Writer (List String) Int
f s = do
       diga s
       pure 1

-- o segundo item do writer é o log
-- snd $ runIdentity $ runWriterT f
-- o primeiro item do writer é o retorno
-- fst $ runIdentity $ runWriterT f


--transicoes : Pi -> Writer (List String) Pi
--transicoes p = do
--                    diga (show p)
--                    if (controlStackIsEmpty p) then
--                       pure p
                   -- ELSE
                   --    TRANSICOES (TRANSICAO P)
