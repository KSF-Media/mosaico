module Mosaico.StaticPageMeta where

import Data.Maybe (Maybe(..))
import KSF.Paper (Paper(..), paperName)

staticPageTitle :: String -> Paper -> String
staticPageTitle page paper =
  case page, paper of
    "anslagstavlan", _   -> "Anslagstavlan"
    "bruksvillkor", _    -> "Bruksvillkor"
    "fiskecupen", _      -> "Fiskecupen"
    "fragor-och-svar", _ -> "Frågor och svar"
    "insandare", _       -> "Insändare"
    "kontakt", _         -> "Kontakta oss"
    "kundservice", _     -> "Kundservice"
    "nyhetsbrev", HBL    -> "Beställ HBL:s nyhetsbrev!"
    "nyhetsbrev", ON     -> "Beställ Östnylands nyhetsbrev!"
    "nyhetsbrev", VN     -> "Beställ Västra Nylands nyhetsbrev!"
    "tipsa-oss", _       -> "Tipsa oss"
    _, _                 -> paperName paper

staticPageDescription :: String -> Paper -> Maybe String
staticPageDescription page paper =
    case page, paper of
      "nyhetsbrev", HBL -> Just "Här kan du beställa HBL:s nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", VN  -> Just "Här kan du beställa Västra Nylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", ON  -> Just "Här kan du beställa Östnylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      _, _ -> Nothing
