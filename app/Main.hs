{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Text.Lazy (Text, pack, unpack, toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.List (isPrefixOf, dropWhileEnd)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS  -- Strict Text
import PokemonApiScrapper (makeGetRequest)
import Control.Monad.IO.Class (liftIO)  -- Import this for lifting IO


-- Definindo a estrutura de dados para as cartas
data Card = Card
  { 
    name        :: String
  , quantity    :: Int
  , cardNumber  :: Int
  } deriving (Show, Generic)

instance ToJSON Card

-- Função para processar a entrada e converter para JSON
parseCards :: Text -> [Card]
parseCards input =
  let linesOfInput = filter (not . T.null) (T.lines input)
      cards = concatMap parseLine linesOfInput
  in cards

parseLine :: Text -> [Card]
parseLine line
  | "Pokémon:" `T.isPrefixOf` line = []
  | "Trainer:" `T.isPrefixOf` line = []
  | "Energy:" `T.isPrefixOf` line = []
  | otherwise = 
      let parts = T.words line
          quantity = read (T.unpack (parts !! 0)) :: Int
          nameSetCard = T.unwords (drop 1 parts)
          nameParts = init (T.words nameSetCard)  -- Todos menos o último elemento
          --verifica se o penultimo elemento tem lengh 3 e se sim remove
          nameParts2 = if T.length (last nameParts) == 3 then init nameParts else nameParts
          cardNumText = last (T.words nameSetCard)  -- Último elemento, como texto
          nameStr = T.unpack (T.unwords nameParts2)  -- Converte para string com espaços
          card_number = read (T.unpack cardNumText) :: Int  -- Converte o número do cartão para Int
      in [Card nameStr quantity card_number]
main :: IO ()
main = scotty 3000 $ do
  post "/cards" $ do
    input <- param "data" :: ActionM Text
    let cards = parseCards input
    let card1 = head cards
    result <- liftIO $ makeGetRequest (TS.pack (name card1)) (TS.pack (show (cardNumber card1)))
    case result of
      Right jsonResponse -> text (pack jsonResponse)
      Left errorMsg -> text (pack errorMsg)
    json result

