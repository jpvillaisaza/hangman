module Hangman exposing (..)

import Char
import Html exposing (..)
import Html.Attributes
import Html.Events
import Http
import Keyboard
import Random
import Random.Extra exposing (sample)
import Set exposing (Set)


main : Program Never Model Msg
main =
    Html.program
        { init = init "hangman"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


englishDictionary : String
englishDictionary =
    "dict/english"


spanishDictionary : String
spanishDictionary =
    "dict/spanish"


getWords : Language -> Http.Request String
getWords language =
    let
        dictionary =
            case language of
                English ->
                    englishDictionary

                Spanish ->
                    spanishDictionary

    in
        Http.getString dictionary


type alias Model =
    { games : Int
    , guesses : Set Char
    , language : Language
    , letters : List Letter
    , status : Status
    , wins : Int
    , word : String
    }


type Language
    = English
    | Spanish


type alias Letter =
    { guessed : Bool
    , letter : Char
    }


type Status
    = Lost
    | Playing Int
    | Won


isPlaying : Status -> Bool
isPlaying status =
    case status of
        Playing _ ->
            True

        _ ->
            False


toLetters : String -> List Letter
toLetters word =
    String.foldr (::) [] word
        |> List.map (\letter -> {guessed = False, letter = letter})


init : String -> (Model, Cmd Msg)
init word =
    let
        model =
            { games = 0
            , guesses = Set.empty
            , language = Spanish
            , letters = toLetters word
            , status = Playing 5
            , wins = 0
            , word = word
            }
    in
        ( model
        , Http.send Dict (getWords model.language)
        )


type Msg
    = Guess Char
    | Dict (Result Http.Error String)
    | New String
    | Restart
    | SwitchTo Language


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Guess c ->
            (guess c model, Cmd.none)

        Dict (Ok words) ->
            ( model
            , Random.generate New
                (sample (String.lines words) |> Random.map (Maybe.withDefault "i"))
            )

        Dict (Err _) ->
            (model, Cmd.none)

        New word ->
            ( { model
                  | guesses = Set.empty
                  , letters = toLetters word
                  , status = Playing 5
                  , word = word
              }
            , Cmd.none)


        Restart ->
            ( { model | games = model.games + 1 }
            , Http.send Dict (getWords model.language)
            )

        SwitchTo language ->
            ( { model | language = language }
            , Http.send Dict (getWords language)
            )


guess : Char -> Model -> Model
guess guess model =
    let
        match guess {guessed, letter} =
            { letter = letter
            , guessed = checkLetter guess letter || guessed
            }

        letters =
            List.map (match guess) model.letters

        status =
            if model.letters == letters then
                case model.status of
                    Playing 1 ->
                        Lost

                    Playing n ->
                        Playing (n - 1)

                    _ ->
                        model.status

            else
                if List.all .guessed letters then
                    Won
                else
                    model.status
    in
        { model
            | games = if status == Won || status == Lost then model.games + 1 else model.games
            , guesses = Set.insert guess model.guesses
            , letters = letters
            , status = status
            , wins = if status == Won then model.wins + 1 else model.wins
        }


checkLetter : Char -> Char -> Bool
checkLetter letter1_ letter2_ =
    let
        letter1 =
            Char.toLower letter1_

        letter2 =
            Char.toLower letter2_

        lettersA =
            ['a', 'á']

        lettersE =
            ['e', 'é']

        lettersI =
            ['i', 'í']

        lettersO =
            ['o', 'ó']

        lettersU =
            ['u', 'ú']

    in
        letter1 == letter2
            || List.member letter1 lettersA && List.member letter2 lettersA
            || List.member letter1 lettersE && List.member letter2 lettersE
            || List.member letter1 lettersI && List.member letter2 lettersI
            || List.member letter1 lettersO && List.member letter2 lettersO
            || List.member letter1 lettersU && List.member letter2 lettersU


subscriptions : Model -> Sub Msg
subscriptions model =
    if isPlaying model.status then
        Keyboard.presses (Guess << Char.fromCode)
    else
        Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ Html.text "Hangman" ]
        , viewStats model
        , Html.h2 [] [ Html.text (toStringL model) ]
        , Html.p [] [Html.text (toString model.status)]
        , Html.p [] (if model.status == Lost || model.status == Won then [viewWord model] else [])
        , Html.p [] [Html.text (Set.foldr String.cons "" model.guesses)]
        , Html.button [Html.Events.onClick Restart] [ Html.text "Restart" ]
        , viewKeyboard
        , viewLanguage model.language
        , viewFooter model
        ]


toStringL : Model -> String
toStringL model =
    let
        fromLetter {guessed, letter} =
            if guessed then letter else '-'
    in
        List.map fromLetter model.letters
            |> List.foldr String.cons ""


viewFooter : Model -> Html msg
viewFooter _ =
    let
        repository =
            "https://github.com/jpvillaisaza/hangman"

        repositoryLink =
            Html.a
                [ Html.Attributes.href repository
                , Html.Attributes.target "_blank"
                ]
                [ Html.text repository ]
    in
        Html.footer
            [ Html.Attributes.class "footer" ]
            [ Html.p [] [ repositoryLink ]
            ]


viewKeyboard : Html Msg
viewKeyboard =
    let
        letters_ =
            List.range (Char.toCode 'a') (Char.toCode 'n')
                ++
                Char.toCode 'ñ' :: List.range (Char.toCode 'o') (Char.toCode 'z')
        letters =
            List.map Char.fromCode letters_

        sd letter =
            Html.button
                [ Html.Attributes.class "btn btn-default"
                , Html.Events.onClick (Guess letter)
                ]
                [ Html.text (String.fromChar letter) ]

    in
        Html.div [] (List.map sd letters)


viewLanguage : Language -> Html Msg
viewLanguage currentLanguage =
    let
        currentLanguageText =
            "Current language: " ++ toString currentLanguage

        (nextLanguage, nextLanguageText) =
            case currentLanguage of
                English ->
                    (Spanish, "Cambiar a español")

                Spanish ->
                    (English, "Switch to English")

    in
        Html.div
            []
            [ Html.p
                  []
                  [ Html.text currentLanguageText ]
            , Html.button
                  [ Html.Events.onClick (SwitchTo nextLanguage) ]
                  [ Html.text nextLanguageText ]
            ]


viewStats : Model -> Html msg
viewStats model =
    Html.div
        []
        [ Html.text ("Wins: " ++ toString model.wins)
        , Html.text (" (" ++ toString model.games ++ ")")
        ]


viewWord : Model -> Html msg
viewWord model =
    let
        base =
            case model.language of
                English ->
                    "https://en.oxforddictionaries.com/definition/us/"

                Spanish ->
                    "http://dirae.es/palabras/"
    in
        Html.a
            [ Html.Attributes.href (base ++ model.word)
            , Html.Attributes.target "_blank"
            ]
            [ Html.text model.word ]
