module Translation exposing (..)


type Language
    = German
    | English


type TranslationKey
    = Login
    | Register
    | WithoutLogin
    | Username
    | Password
    | Preview
    | NotLoggedIn
    | ShowPassword
    | WrongUsernameOrPassword
    | UsernameOrPasswordInvalid
    | ProgressCouldGetLost
    | ProgressWillGetLost


translate : TranslationKey -> Language -> String
translate key lang =
    case lang of
        German ->
            case key of
                Login ->
                    "Anmelden"

                Register ->
                    "Registrieren"

                WithoutLogin ->
                    "Ohne Anmelden"

                Username ->
                    "Benutzername"

                Password ->
                    "Password"

                Preview ->
                    "Vorschau"

                NotLoggedIn ->
                    "Nicht Angemeldet"

                ShowPassword ->
                    "Password anzeigen"

                WrongUsernameOrPassword ->
                    "Falscher Username oder falsches Password!"

                UsernameOrPasswordInvalid ->
                    "Username muss [A-z] Länge min. 3. Password muss IBM valide und Länge min. 10"

                ProgressCouldGetLost ->
                    "Fortschritt könnte währent eines Updates verlohren gehen"

                ProgressWillGetLost ->
                    "Du bist nicht angemeldet und dein Fortschritt wird verlohren gehen"

        English ->
            case key of
                Login ->
                    "Login"

                Register ->
                    "Register"

                WithoutLogin ->
                    "Without Login"

                Username ->
                    "Username"

                Password ->
                    "Password"

                Preview ->
                    "preview"

                NotLoggedIn ->
                    "not logged in"

                ShowPassword ->
                    "Show password"

                WrongUsernameOrPassword ->
                    "Wrong username or password!"

                UsernameOrPasswordInvalid ->
                    "Username must be [A-z] with min length 3. Password must be IBM valid with min length 10"

                ProgressCouldGetLost ->
                    "your progress or account could get lost in an update"

                ProgressWillGetLost ->
                    "You are not logged in and your progress will be lost"
