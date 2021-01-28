let Actions = ./workflows.dhall

let apps = [
, Actions.App::{
  , name = "Scripts"
  , buildDir = "scripts"
  , deployDir = "scripts"
  }
, Actions.App::{
  , name = "Mitt Konto"
  , buildDir = "mitt-konto"
  , deployDir = "mitt-konto"
  , env = toMap {
    , PRODUCTION_SENTRY_DSN = "https://54e59357e2fd42db917041739865e2c9@sentry.io/5174203"
    }
  }
, Actions.App::{
  , name = "Vetrina (for testing only)"
  , buildDir = "vetrina-test"
  , deployDir = "vetrina-test"
  , env = toMap {
    , NODE_ENV = "development"
    , SENTRY_DSN = "https://6479d7c55fbd4e0db7d9ac755083865f@sentry.io/3718842"
    }
  }
, Actions.App::{
  , name = "Elections (EU)"
  , buildDir = "elections"
  , deployDir = "elections-eu"
  , env = toMap {
    , ELECTION_BACKEND_URL = "https://elections-eu.api.ksfmedia.fi/v1"
    , ELECTION_TYPE = "EU"
    }
  }
, Actions.App::{
  , name = "Elections (Parliament)"
  , buildDir = "elections"
  , deployDir = "elections"
  , env = toMap {
    , ELECTION_BACKEND_URL = "https://election.api.ksfmedia.fi/v1"
    , ELECTION_TYPE = "PARLIAMENT"
    }
  }
, Actions.App::{
  , name = "App article"
  , buildDir = "app-article"
  , deployDir = "app-article"
  , env = toMap {
    , HIDE_LOGIN_LINKS = "false"
    }
  }
{-
, Actions.App::{
  , name = "Prenumerera PoC"
  , buildDir = "prenumerera"
  , deployDir = "prenumerera"
  }
, Actions.App::{
  , name = "Duellen"
  , buildDir = "duellen"
  , deployDir = "duellen"
  }
, Actions.App::{
  , name = "Podcasts"
  , buildDir = "podcasts"
  , deployDir = "podcasts"
  , env = toMap {
    , PODCAST_IDS = "694513583,705599305,630339678,542583531"
    }
  }
, Actions.App::{
  , name = "Podcasts (VN)"
  , buildDir = "podcasts"
  , deployDir = "podcasts-vn"
  , env = toMap { PODCAST_IDS = "806886790" }
  }
-}
] : List Actions.App.Type

in apps