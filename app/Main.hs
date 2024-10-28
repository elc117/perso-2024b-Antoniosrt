{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Text.Lazy (Text, pack, unpack)
import Data.Aeson (ToJSON, encode, object, (.=))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS  -- Strict Text
import PokemonApiScrapper (makeGetRequest)
import Control.Monad.IO.Class (liftIO)  -- Import this for lifting IO
import Control.Monad (mapM)
import System.IO (writeFile)  -- Import this for writing files

-- Definindo a estrutura de dados para as cartas
data Card = Card
  { name        :: String
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
          nameParts2 = if T.length (last nameParts) == 3 then init nameParts else nameParts
          cardNumText = last (T.words nameSetCard)  -- Último elemento, como texto
          nameStr = T.unpack (T.unwords nameParts2)  -- Converte para string com espaços
          card_number = read (T.unpack cardNumText) :: Int  -- Converte o número do cartão para Int
      in [Card nameStr quantity card_number]

fetchCard :: Card -> IO (Either String String)
fetchCard card = do
  response <- makeGetRequest (TS.pack (name card)) (TS.pack (show (cardNumber card)))
  return $ case response of
    Right jsonResponse -> Right $ formatCardResponse card jsonResponse
    Left errorMsg -> Left errorMsg

formatCardResponse :: Card -> String -> String
formatCardResponse card jsonResponse =
  "Card Name: " ++ name card ++ ", Card Number: " ++ show (cardNumber card) ++ ", Total Sets: " ++ jsonResponse

fetchAllCards :: [Card] -> IO [Either String String]
fetchAllCards cards = mapM fetchCard cards

-- Função para salvar os resultados em um arquivo TXT
saveResultsToTxt :: [String] -> IO ()
saveResultsToTxt results = writeFile "cards.txt" (unlines results)

main :: IO ()
main = scotty 3000 $ do
  post "/cards" $ do
    input <- param "data" :: ActionM Text
    let cards = parseCards input
    results <- liftIO $ fetchAllCards cards
    let formattedResults = [result | Right result <- results]  -- Filtrando apenas os resultados certos
    liftIO $ saveResultsToTxt formattedResults  -- Salvando resultados em um arquivo TXT
    json $ object ["cards" .= formattedResults]  -- Retornando resultados como JSON
