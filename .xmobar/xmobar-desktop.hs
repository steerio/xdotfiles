{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Xmobar

import Control.Applicative
import Control.Exception (handle, SomeException)
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Map as M
import Data.List (intercalate)
import Text.Printf (printf)

main = xmobar $ defaultConfig
  { font = "xft:Droid Sans Mono Slashed for Powerline:size=8:antialias=true"
  , bgColor = "black"
  , fgColor = "#bdae93"
  , iconRoot = "/home/steerio/.xmobar/icons"

  , sepChar = "%"
  , alignSep = "}{"
  , template = " %StdinReader%}{%kraken% %kbd% %date% "
  , commands =
    [ Run StdinReader
    , Run $ Kraken [ ("XXBTZEUR", "BTC")
                   , ("XXBTZUSD", "BTC/USD")
                   , ("XXLMZEUR", "XLM")
                   , ("XETHZEUR", "ETH")
                   , ("ADAEUR", "ADA")
                   , ("SOLEUR", "SOL")
                   , ("ALGOEUR", "ALGO")
                   , ("LINKEUR", "LINK")
                   ] 600
    , Run $ Kbd [ ("us",           "<icon=us.xpm/>")
                , ("il(phonetic)", "<icon=il.xpm/>")
                , ("hu",           "<icon=hu.xpm/>") ]
    , Run $ Date "<fc=#928374>%a %m/%d</fc> %H:%M" "date" 10 ]}

type Prices = M.Map String Price
newtype KrakenResult = KrakenResult (Either String Prices) deriving Show
newtype Price = Price Double deriving Show

instance FromJSON KrakenResult where
  parseJSON = withObject "root" $ \v -> KrakenResult <$> (Right <$> v .: "result" <|>
                                                          Left . mconcat <$> v .: "error")

instance FromJSON Price where
  parseJSON = withObject "Price" $ \v -> Price . read . head <$> v .: "c"

data Kraken = Kraken [(String, String)] Int deriving (Read, Show)

getPrices :: [String] -> IO KrakenResult
getPrices tickers =
  parseRequest ("https://api.kraken.com/0/public/Ticker?pair=" <> intercalate "," tickers)
  >>= handle @SomeException handleError . fmap getResponseBody . httpJSON
  where
    handleError _ = pure $ KrakenResult $ Left "Can't access Kraken"

renderPrice :: Price -> String
renderPrice (Price value) = printf "%.*f" digits value
  where
    digits = max @Int 0 $ negate (round $ logBase 10 value) + 3

renderPair :: Prices -> (String, String) -> String
renderPair m (code, tag) = "<fc=#928374>" <> tag <> "</fc> " <> maybe "N/A" renderPrice (M.lookup code m)

instance Exec Kraken where
  alias _ = "kraken"
  rate (Kraken _ r) = r
  run (Kraken tickers _) = do
    KrakenResult res <- getPrices $ fst <$> tickers
    return $ case res of
                  Right prices -> unwords $ renderPair prices <$> tickers
                  Left err -> "<fc=#cc241d>" <> err <> "</fc>"
