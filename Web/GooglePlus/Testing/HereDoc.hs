module Web.GooglePlus.Testing.Heredoc(heredoc) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 
heredoc :: QuasiQuoter
heredoc = QuasiQuoter { quoteExp  = stringE,
                        quotePat  = (litP . stringL),
                        quoteType = const $ fail "not supported",
                        quoteDec  = const $ fail "not supported" }
