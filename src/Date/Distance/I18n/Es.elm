module Date.Distance.I18n.Es
    exposing
        ( LocaleConfig
        , locale
        )

{-| Spanish locale.
@docs LocaleConfig
@docs locale
-}

import Date.Distance.Types exposing (DistanceLocale(..), Locale)
import Date.Extra as Date exposing (Interval(..))


{-| Configure the localization function.

  - `addPrefix` – turns `2 jours` into `il y a 2 jours` or `dans 2 jours`

-}
type alias LocaleConfig =
    { addPrefix : Bool }


{-| Configure the French locale.

    locale =
        I18n.Fr.locale { addPrefix = True }

    inWords =
        { defaultConfig | locale = locale }
            |> inWordsWithConfig

-}
locale : LocaleConfig -> Locale
locale { addPrefix } order distance =
    let
        result =
            localeHelp distance
    in
    if addPrefix then
        if order == LT then
            "en " ++ result
        else
            "hace " ++ result
    else
        result


localeHelp : DistanceLocale -> String
localeHelp distance =
    case distance of
        LessThanXSeconds i ->
            circa "menos de "
                Second
                i

        HalfAMinute ->
            "menos de un minuto"

        LessThanXMinutes i ->
            circa "menos de " Minute i

        XMinutes i ->
            exact Minute i

        AboutXHours i ->
            circa "hace aprox. " Hour i

        XDays i ->
            exact Day i

        AboutXMonths i ->
            circa "hace aprox. " Month i

        XMonths i ->
            exact Month i

        AboutXYears i ->
            circa "hace aprox. " Year i

        OverXYears i ->
            circa "más de " Year i

        AlmostXYears i ->
            circa "al rededor de " Year i


formatInterval : Interval -> String
formatInterval interval =
    case interval of
        Millisecond ->
            "milisegundo"

        Second ->
            "segundo"

        Minute ->
            "minuto"

        Hour ->
            "hora"

        Day ->
            "día"

        Week ->
            "semana"

        Month ->
            "mes"

        Year ->
            "año"

        Quarter ->
            "trimestre"

        Monday ->
            "lunes"

        Tuesday ->
            "martes"

        Wednesday ->
            "miércoles"

        Thursday ->
            "jueves"

        Friday ->
            "viernes"

        Saturday ->
            "saturday"

        Sunday ->
            "domingo"


singular : Interval -> String
singular interval =
    case interval of
        Millisecond ->
            feminine interval

        Second ->
            feminine interval

        Minute ->
            feminine interval

        Hour ->
            feminine interval

        Week ->
            feminine interval

        _ ->
            masculine interval


feminine : Interval -> String
feminine interval =
    "una " ++ formatInterval interval


masculine : Interval -> String
masculine interval =
    "un " ++ formatInterval interval


pluralizeInterval : Interval -> String
pluralizeInterval interval =
    if interval /= Month then
        "es"
    else
        "s"


circa : String -> Interval -> Int -> String
circa prefix interval i =
    case i of
        1 ->
            prefix ++ singular interval

        _ ->
            prefix ++ toString i ++ " " ++ formatInterval interval ++ pluralizeInterval interval


exact : Interval -> Int -> String
exact interval i =
    case i of
        1 ->
            "1 " ++ formatInterval interval

        _ ->
            toString i ++ " " ++ formatInterval interval ++ pluralizeInterval interval
