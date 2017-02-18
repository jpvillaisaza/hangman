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
    { guesses : Set Char
    , language : Language
    , letters : List Letter
    , status : Status
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
            { guesses = Set.empty
            , language = Spanish
            , letters = toLetters word
            , status = Playing 5
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
            ( model
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
            , guessed = guess == letter || guessed
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
            | guesses = Set.insert guess model.guesses
            , letters = letters
            , status = status
        }


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
        , Html.h3 [] [ Html.text (toStringL model) ]
        , Html.div [] [Html.text (toString model.status)]
        , Html.div [] (if model.status == Lost then [Html.text model.word] else [])
        , Html.div [] [Html.text (Set.foldr String.cons "" model.guesses)]
        , Html.button [Html.Events.onClick Restart] [ Html.text "Restart" ]
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
                [ Html.Attributes.href repository ]
                [ Html.text repository ]
    in
        Html.footer
            [ Html.Attributes.class "footer" ]
            [ Html.div [] [ repositoryLink ]
            ]


viewLanguage : Language -> Html Msg
viewLanguage currentLanguage =
    let
        (nextLanguage, msg) =
            case currentLanguage of
                English ->
                    (Spanish, "Cambiar a español")

                Spanish ->
                    (English, "Switch to English")

    in
        Html.button
            [ Html.Events.onClick (SwitchTo nextLanguage) ]
            [ Html.text msg ]
