module Mosaico.Footer
  ( footer
  ) where

import Prelude
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import KSF.Paper (Paper(..), homepage, paperName)
import Lettera.Models (Category(..), correctionsCategory)
import Mosaico.Client.Handlers (Handlers)
import Mosaico.Component.Consent (openConsent)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)

footer :: Paper -> Handlers -> JSX
footer mosaicoPaper { onCategoryClick, onStaticPageClick, onArchivePageClick } =
  DOM.footer
    { className: "flex flex-col items-center py-12 px-0 m-0 bg-aptoma-footer-color lg:px-2 font-roboto [grid-area:foot]"
    , children:
        [ contactInfo mosaicoPaper commonFooter
        , appLinks mosaicoPaper
        , DOM.hr { className: "w-4/5 md:w-56 lg:w-96 mt-0 mx-auto mb-5 bg-gray-300 border-0 h-[1px]" }
        , DOM.div
            { className: "mt-6 mb-4 text-sm text-aptoma-footer-text-color"
            , children: [ DOM.text "ALLA TIDNINGAR" ]
            }
        , DOM.div
            { className: "flex flex-col w-full md:w-80 lg:w-96 sm:flex-row"
            , children: map logo [ VN, HBL, ON ]
            }
        ]
    }

  where
  logo :: Paper -> JSX
  logo paper =
    let
      className = "flex flex-col items-center justify-center flex-1 no-underline bg-cover"

      children = foldMap (\cls ->
        [ DOM.div { className: "w-14 h-14 bg-gray-400 mask-size-14" <> cls}
        , DOM.div
            { className: "text-xs whitespace-nowrap"
            , children: [ DOM.text $ paperName paper ]
            }
        ]) $ logoClass paper

    in
      if paper == mosaicoPaper then
        DOM.div { className, children }
      else
        DOM.a { className, children, href: homepage paper }

  logoClass HBL = Just " maskimage-hbl"
  logoClass VN = Just " maskimage-vn"
  logoClass ON = Just " maskimage-on"
  logoClass _ = Nothing
  commonFooter = footerLinks onCategoryClick onStaticPageClick onArchivePageClick

contactInfo :: Paper -> JSX -> JSX
contactInfo ON = ostnylandContactInfo
contactInfo VN = vastranylandContactInfo
contactInfo HBL = hblContactInfo
contactInfo _ = identity

hblContactInfo :: JSX -> JSX
hblContactInfo commonFooter =
  DOM.div
    { children:
        [ DOM.div
            { className: "inline text-sm lg:flex"
            , children:
                [ column firstColumn
                , column secondColumn
                , column thirdColumn
                ]
            }
        , commonFooter
        ]
    }
  where
  firstColumn =
    [ section "Besöksadress"
        [ DOM.text "Mannerheimvägen 18"
        , DOM.br {}
        , DOM.text "00100 Helsingfors"
        ]
    , section "Växel: " [ tel "09 125 31" ]
    , section "E-post: " [ DOM.text "fornamn.efternamn@hbl.fi" ]
    , section "Prenumerationer och Kundservice: "
        [ tel "09 125 35 00"
        , DOM.text ", "
        , email "kundservice@hbl.fi"
        , DOM.br {}
        , DOM.text "(mån-fre kl. 9.00-12.00 och 13.00-15.00.)"
        ]
    , section "Annonser: "
        [ tel "09 125 35 58"
        , DOM.text ", "
        , email "annons@hbl.fi"
        , DOM.br {}
        , DOM.text "Privatannonser: "
        , columnLink "https://annonskiosken.ksfmedia.fi/ilmoita/hufvudstadsbladet" [ DOM.text "hbl.fi/annonskiosken"]
        ]
    ]

  secondColumn =
    [ section "Kalle Silfverberg" [ DOM.text "Chefredaktör och ansvarig utgivare" ]
    , section "Gunilla Celvin" [ DOM.text "Biträdande chefredaktör och redaktionschef (nyheter och sport)" ]
    , section "Dan Lolax" [ DOM.text "Redaktionschef (kultur, opinion och ekonomi)" ]
    , section "Steffen Ørsted" [ DOM.text "Redaktionschef (visuellt och utveckling)" ]
    , section "Nyhetstips"
        [ tel "09 125 32 22"
        , DOM.text ", "
        , email "nyheter@hbl.fi"
        ]
    , section "Insändare: "
        [ columnLink "/sida/insandare" [ DOM.text "Skriv din insändare här" ] ]
    , section "Mejla din insändare: " [ email "debatt@hbl.fi" ]
    ]

vastranylandContactInfo :: JSX -> JSX
vastranylandContactInfo commonFooter =
  DOM.div
    { children:
        [ DOM.div
            { className: "inline text-sm lg:flex"
            , children:
                [ column firstColumn
                , column secondColumn
                , column thirdColumn
                ]
            }
        , commonFooter
        ]
    }
  where
  firstColumn =
    [ section "Kontakta Västra Nyland: "
        [ DOM.text "Genvägen 8"
        , DOM.br {}
        , DOM.text "10650 Ekenäs"
        ]
    , section "Prenumerationer och kundservice: "
        [ tel "09 1253 500"
        , DOM.br {}
        , DOM.text "(mån-fre kl. 9.00-12.00 och 13.00-15.00.)"
        , DOM.text ", "
        , email "kundservice@hbl.fi"
        ]
    , section "Redaktionen: "
        [ tel "019 222 822"
        , DOM.text ", "
        , email "vnred@vastranyland.fi"
        ]
    , section "Annonser: "
        [ tel "09 1253 558"
        , DOM.text ", "
        , email "annons@vastranyland.fi"
        , DOM.br {}
        , DOM.text "Privatannonser: "
        , columnLink "https://annonskiosken.ksfmedia.fi/ilmoita/vastranyland" [ DOM.text "vastranyland.fi/annonskiosken"]
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Kalle Silfverberg" ]
    , section "Chefredaktör för tidningen och nyhetschef: " [ DOM.text "Marina Holmberg" ]
    , section "Insändare: "
        [ columnLink "/sida/insandare" [ DOM.text "Skriv din insändare här" ]
        ]
    , section "Anslagstavlan: "
        [ columnLink "/sida/anslagstavlan" [ DOM.text "Skicka in din händelse här" ]
        ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

ostnylandContactInfo :: JSX -> JSX
ostnylandContactInfo commonFooter =
  DOM.div
    { children:
        [ DOM.div
            { className: "inline text-sm lg:flex"
            , children:
                [ column firstColumn
                , column secondColumn
                , column thirdColumn
                ]
            }
        , commonFooter
        ]
    }
  where
  firstColumn =
    [ section "Kontakta Östnyland: "
        [ DOM.text "Lundagatan 8"
        , DOM.br {}
        , DOM.text "06100 Borgå"
        ]
    , section "Växel: " [ tel "09 12531" ]
    , section "Prenumerationer och kundservice:"
        [ tel "09 1253 500"
        , DOM.br {}
        , columnLink "kundservice@hbl.fi" [ DOM.text "kundservice@hbl.fi" ]
        , DOM.br {}
        , DOM.text "(mån-fre kl. 9.00-12.00 och 13.00-15.00.)"
        ]
    , section "Redaktionen: "
        [ tel "044 777 6000"
        , DOM.text ", "
        , email "redaktion@ostnyland.fi"
        ]
    , section "Annonser: "
        [ tel "09 1253 558"
        , DOM.text ", "
        , email "annons@ostnyland.fi"
        , DOM.br {}
        , DOM.text "Privatannonser: "
        , columnLink "https://annonskiosken.ksfmedia.fi/ilmoita/ostnyland" [ DOM.text "ostnyland.fi/annonskiosken"]
        ]
    ]

  secondColumn =
    [ section "Ansvarig utgivare: " [ DOM.text "Kalle Silfverberg" ]
    , section "Chefredaktör för tidningen och nyhetschef: "
        [ DOM.text "Helén Kurri "
        , tel "040 506 3977"
        ]
    , section "Insändare: "
        [ columnLink "/sida/insandare" [ DOM.text "Skriv din insändare här" ]
        ]
    , section "" [ DOM.text "Det lokala kommer först." ]
    ]

footerLinks
  :: (Category -> EventHandler)
  -> (String -> EventHandler)
  -> (String -> EventHandler)
  -> JSX
footerLinks onCategoryClick onStaticPageClick onArchivePageClick=
  DOM.div
    { className: "flex flex-col justify-center items-center mx-auto mt-9 mb-8 lg:flex-row"
    , children:
        [ externalLink "Dataskyddsbeskrivning" "https://www.ksfmedia.fi/dataskydd"
        , footerLink "Bruksvillkor" "bruksvillkor"
        , footerLink "Kundservice" "kundservice"
        , footerLink "Kontakta oss" "kontakt"
        , footerLink "Tipsa oss" "tipsa-oss"
        , footerLink "Nyhetsbrev" "nyhetsbrev"
        , footerLink "App" "app"
        , categoryLink correctionsCategory
        , archiveLink
        ]
    }
  where
    externalLink caption url =
      DOM.a
        { href: url
        , className: "my-1 mx-auto text-sm no-underline text-aptoma-text-color md:mx-5"
        , children: [ DOM.text caption ]
        }

    footerLink caption link =
      DOM.a
        { href: "/sida/" <> link
        , className: "my-1 mx-auto text-sm no-underline text-aptoma-text-color md:mx-5"
        , children: [ DOM.text caption ]
        , onClick: onStaticPageClick link
        }

    categoryLink cat@(Category {id, label, url}) =
      DOM.a
        { href: fromMaybe ("/" <> id) url
        , className: "my-1 mx-auto text-sm no-underline text-aptoma-text-color md:mx-5"
        , children: [ DOM.text $ unwrap label ]
        , onClick: onCategoryClick cat
        }

    archiveLink =
      DOM.a
        { href: "/arkiv"
        , className: "my-1 mx-auto text-sm no-underline text-aptoma-text-color md:mx-5"
        , children: [ DOM.text "Arkiv" ]
        , onClick: onArchivePageClick "/arkiv"
        }

appLinks :: Paper -> JSX
appLinks mosaicoPaper =
  let appStoreUrl =
        case mosaicoPaper of
          HBL -> "https://apps.apple.com/app/apple-store/id1473028619?pt=562401&ct=site-footer&mt=8"
          ON -> "https://apps.apple.com/app/apple-store/id1508293423?pt=562401&ct=site-footer&mt=8"
          VN -> "https://apps.apple.com/app/apple-store/id1508293600?pt=562401&ct=site-footer&mt=8"
          _ -> "https://apps.apple.com/app/apple-store/id1473028619?pt=562401&ct=site-footer&mt=8"
      googlePlayUrl =
        case mosaicoPaper of
          HBL -> "https://play.google.com/store/apps/details?id=fi.ksfmedia.hbl&utm_source=site&utm_campaign=site-footer"
          ON -> "https://play.google.com/store/apps/details?id=fi.ksfmedia.ostnyland&utm_source=site&utm_campaign=site-footer"
          VN -> "https://play.google.com/store/apps/details?id=fi.ksfmedia.vastranyland&utm_source=site&utm_campaign=site-footer"
          _ -> "https://play.google.com/store/apps/details?id=fi.ksfmedia.hbl&utm_source=site&utm_campaign=site-footer"
      appStoreBadgeUrl = "https://cdn.ksfmedia.fi/mosaico/app-download-icons/app-store-badge.svg"
      googlePlayBadgeUrl = "https://cdn.ksfmedia.fi/mosaico/app-download-icons/google-play-badge.svg"
  in DOM.div
    { className: "flex flex-col justify-center items-center mx-auto mt-0 mb-8 px-2.5"
    , children:
      [ DOM.div
          { className: "mb-2 max-w-xs text-base text-center text-aptoma-text-color md:max-w-xl"
          , children:
              [ DOM.text "Ladda ned vår nyhetsapp – det smidigaste sättet att läsa våra nyheter." ]
          }
      , DOM.div
          { className: "flex flex-row justify-center items-center py-2.5"
          , children:
              [ storeLink "Hämta vår nyhetsapp från App Store" appStoreUrl appStoreBadgeUrl
              , storeLink "Ladda ned vår nyhetsapp från Google Play" googlePlayUrl googlePlayBadgeUrl
              ]
          }
      ]
    }
  where
    storeLink altText url badgeUrl =
      DOM.a
        { href: url
        , className: "px-1.5"
        , children:
          [ DOM.img {
              src: badgeUrl
            , loading: "lazy"
            , className: "h-[35px] md:h-[40px]"
            , alt: altText
            }
          ]
        }

column :: Array JSX -> JSX
column children = DOM.div { className: "max-w-xs basis-1/3", children }

columnLink :: String -> Array JSX -> JSX
columnLink href children = DOM.a { href, children, className: "text-aptoma-link-color" }

section :: String -> Array JSX -> JSX
section title children =
  DOM.div
    { className: "block pr-1 pl-1 mt-4 mb-2"
    , children:
        [ DOM.b
        { className: "text-aptoma-text-color"
        , children: [ DOM.text title, DOM.text " " ] }
        , DOM.div
        { className:  "no-underline"
        , children: children }
        ]
    }

email :: String -> JSX
email address = columnLink ("mailto:" <> address) [ DOM.text address ]

tel :: String -> JSX
tel number =
  columnLink
    ("tel:" <> replaceAll (Pattern " ") (Replacement "") number)
    [ DOM.text number ]

thirdColumn :: Array JSX
thirdColumn =
  [ section "" [ DOM.text "Hufvudstadsbladet Ab ger ut Hufvudstadsbladet, Västra Nyland, Östnyland och HBL Junior. Hufvudstadsbladet Ab ägs av Bonnier News och Konstsamfundet." ]
  , section ""
      [ columnLink "https://www.ksfmedia.fi/jobba-hos-oss" [ DOM.text "Jobba hos oss" ]
      ]
  , section "Dataskydd: "
      [ DOM.a
          { href: "#"
          , onClick: capture_ $ void openConsent
          , children: [ DOM.text "Hantera dataskydd" ]
          , className: "text-aptoma-link-color"
          }
      ]
  ]
