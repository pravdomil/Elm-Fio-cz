module FioCz exposing (..)

import Iso8601
import Json.Decode
import Json.Decode.Extra2
import Time


type alias Statement =
    { account : Account
    , transactions : List Transaction
    }


statementDecoder : Json.Decode.Decoder Statement
statementDecoder =
    Json.Decode.field "accountStatement"
        (Json.Decode.map2
            Statement
            (Json.Decode.field "info" accountDecoder)
            (Json.Decode.field "transactionList"
                (Json.Decode.field "transaction" (Json.Decode.list transactionDecoder))
            )
        )



--


type alias Account =
    { number : String
    , bankNumber : String
    , currency : String

    --
    , iban : String
    , bic : String

    --
    , openingBalance : Float
    , closingBalance : Float
    }


accountDecoder : Json.Decode.Decoder Account
accountDecoder =
    Json.Decode.map7
        Account
        (Json.Decode.field "accountId" Json.Decode.string)
        (Json.Decode.field "bankId" Json.Decode.string)
        (Json.Decode.field "currency" Json.Decode.string)
        (Json.Decode.field "iban" Json.Decode.string)
        (Json.Decode.field "bic" Json.Decode.string)
        (Json.Decode.field "openingBalance" Json.Decode.float)
        (Json.Decode.field "closingBalance" Json.Decode.float)



--


type alias Transaction =
    { id : Int -- column22
    , type_ : String -- column8
    , amount : Float -- column1
    , currency : String -- column14
    , originalAmount : Maybe { amount : Float, currency : String } -- column18
    , date : Time.Posix -- column0

    --
    , accountName : Maybe String -- column10
    , accountNumber : Maybe String -- column2

    --
    , bankName : Maybe String -- column12
    , bankNumber : Maybe String -- column3
    , bankBic : Maybe String -- column26

    --
    , constantSymbol : Maybe String -- column4
    , variableSymbol : Maybe String -- column5
    , specificSymbol : Maybe String -- column6
    , reference : Maybe String -- column27

    --
    , description : Maybe String -- column7
    , message : Maybe String -- column16
    , note : Maybe String -- column25

    --
    , author : Maybe String -- column9
    , orderId : Maybe Int -- column17
    }


transactionDecoder : Json.Decode.Decoder Transaction
transactionDecoder =
    Json.Decode.succeed
        Transaction
        |> Json.Decode.Extra2.apply (Json.Decode.field "column22" (field Json.Decode.int))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column8" (field Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column1" (field Json.Decode.float))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column14" (field Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column18" (maybeField originalAmount))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column0" (field posix))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column10" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column2" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column12" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column3" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column26" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column4" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column5" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column6" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column27" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column7" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column16" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column25" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column9" (maybeField Json.Decode.string))
        |> Json.Decode.Extra2.apply (Json.Decode.field "column17" (maybeField Json.Decode.int))



--


field : Json.Decode.Decoder a -> Json.Decode.Decoder a
field a =
    Json.Decode.field "value" a


maybeField : Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
maybeField a =
    Json.Decode.oneOf
        [ Json.Decode.null Nothing
        , Json.Decode.field "value" (Json.Decode.map Just a)
        ]


posix : Json.Decode.Decoder Time.Posix
posix =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case Iso8601.toTime (String.left 10 x ++ "T12:00:00.000Z") of
                    Ok x2 ->
                        Json.Decode.succeed x2

                    Err _ ->
                        Json.Decode.fail "Cannot decode date."
            )


originalAmount : Json.Decode.Decoder { amount : Float, currency : String }
originalAmount =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case String.split " " x of
                    amount :: currency :: [] ->
                        case String.toFloat amount of
                            Just x2 ->
                                Json.Decode.succeed { amount = x2, currency = currency }

                            Nothing ->
                                Json.Decode.fail "Cannot decode original amount."

                    _ ->
                        Json.Decode.fail "Cannot decode original amount."
            )
