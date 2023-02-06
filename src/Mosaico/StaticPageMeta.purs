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
    "korsord", _         -> "Korsord"
    "kundservice", _     -> "Kundservice"
    "nyhetsbrev", HBL    -> "Beställ HBL:s nyhetsbrev!"
    "nyhetsbrev", ON     -> "Beställ Östnylands nyhetsbrev!"
    "nyhetsbrev", VN     -> "Beställ Västra Nylands nyhetsbrev!"
    "tipsa-oss", _       -> "Tipsa oss"
    "app", HBL           -> "Hufvudstadsbladets appar för Iphone, Ipad och Android"
    "app", VN            -> "Västra Nylands appar för Iphone, Ipad och Android"
    "app", ON            -> "Östnylands appar för Iphone, Ipad och Android"
    _, _                 -> paperName paper

staticPageDescription :: String -> Paper -> Maybe String
staticPageDescription page paper =
    case page, paper of
      "nyhetsbrev", HBL -> Just "Här kan du beställa HBL:s nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", VN  -> Just "Här kan du beställa Västra Nylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "nyhetsbrev", ON  -> Just "Här kan du beställa Östnylands nyhetsbrev. Nyhetsbreven kostar ingenting."
      "app",        HBL -> Just "Med Hufvudstadsbladets appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      "app",        VN  -> Just "Med Västra Nylands appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      "app",        ON  -> Just "Med Östnylands appar har du tillgång till de senaste nyheterna dygnet runt. Alltid smidigt direkt i din mobil eller pekplatta."
      _, _ -> Nothing
