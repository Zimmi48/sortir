module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Array


adminPassword =
    "admin"


allocinePartner =
    ""


allocineKey =
    ""


allocineQueries =
    Array.fromList
        [ { zip = "75001", radius = 4 } -- includes zip codes from 75001 to 75011 and 75014 to 75018
        , { zip = "75012", radius = 1 }
        , { zip = "75013", radius = 1 }
        , { zip = "75019", radius = 1 }
        , { zip = "75020", radius = 1 }
        ]
