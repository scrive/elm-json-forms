module Form.Regex exposing
    ( time
    , date
    , dateTime
    , email
    )

import Regex


dateTime : Regex.Regex
dateTime =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^\\d\\d\\d\\d-[0-1]\\d-[0-3]\\d[t\\s](?:[0-2]\\d:[0-5]\\d(?::[0-5]\\d)?|23:59:60)(?:\\.\\d+)?(?:z|[+-]\\d\\d:\\d\\d)?$"


date : Regex.Regex
date =
    Maybe.withDefault Regex.never <|
        Regex.fromString
            "^\\d\\d\\d\\d-[0-1]\\d-[0-3]\\d$"


time : Regex.Regex
time =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^(?:[0-2]\\d:[0-5]\\d(?::[0-5]\\d)?|23:59:60)(?:\\.\\d+)?(?:z|[+-]\\d\\d:\\d\\d)?$"


email : Regex.Regex
email =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith
            { caseInsensitive = True
            , multiline = False
            }
            "^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)*$"
