module PokemonApiScrapper where

import Data.Aeson (decode, Value)  -- Import para decodificação de JSON
import Data.Text as T
import GHC.Generics
import Network.Wreq
import Control.Lens
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE  -- Import for encoding
import qualified Data.CaseInsensitive as CI  -- Add this import
import qualified Data.ByteString.Char8 as B  -- Add this import

-- Função genérica para fazer a chamada GET
makeGetRequest :: T.Text -> T.Text -> IO (Either String String)
makeGetRequest cardName cardNumber = do
    -- Define chave no header para acessar a API
    --adiciona "" entre o cardname
    let endpoint = "https://api.pokemontcg.io/v2/cards?q=name:\"" ++ T.unpack cardName ++ "\" number:" ++ T.unpack cardNumber
    let opts = defaults & header (CI.mk (B.pack "X-Api-Key")) .~ [TE.encodeUtf8 (T.pack "fe37da32-0a20-4b99-837a-00e66e2ea17d")] -- Define o header
    res <- getWith opts endpoint :: IO (Response BL.ByteString)
    -- Obtém o corpo da resposta e decodifica o JSON
    let body = res ^. responseBody  -- Aqui está a alteração
    let jsonData = decode body :: Maybe Value
    case jsonData of
        Just value -> return $ Right (show value)
        Nothing -> return $ Left "Failed to decode JSON"
