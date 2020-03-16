module Console.Http exposing
    ( Body
    , Error
    , Expect
    , Header
    , Metadata
    , Part
    , Progress
    , Resolver
    , Response
    , bytesBody
    , bytesPart
    , bytesResolver
    , cancel
    , emptyBody
    , expectBytes
    , expectBytesResponse
    , expectJson
    , expectString
    , expectStringResponse
    , expectWhatever
    , fileBody
    , filePart
    , fractionReceived
    , fractionSent
    , get
    , header
    , jsonBody
    , multipartBody
    , post
    , request
    , riskyRequest
    , riskyTask
    , stringBody
    , stringPart
    , stringResolver
    , task
    , track
    )

import Bytes exposing (Bytes)
import Bytes.Decode
import Console exposing (Log)
import Console.Internal exposing (Cmd, Msg(..))
import Console.Task exposing (Task)
import Dict exposing (Dict)
import File exposing (File)
import Http as H
import Json.Decode
import Json.Encode as Encode


get :
    { url : String
    , expect : Expect msg
    }
    -> Cmd Log msg
get =
    H.get


post :
    { url : String
    , body : Body
    , expect : Expect msg
    }
    -> Cmd Log msg
post =
    H.post


request :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd Log msg
request =
    H.request


type alias Header =
    H.Header


header : String -> String -> Header
header =
    H.header


type alias Body =
    H.Body


emptyBody : Body
emptyBody =
    H.emptyBody


stringBody : String -> String -> Body
stringBody =
    H.stringBody


jsonBody : Encode.Value -> Body
jsonBody =
    H.jsonBody


fileBody : File -> Body
fileBody =
    H.fileBody


bytesBody : String -> Bytes -> Body
bytesBody =
    H.bytesBody


multipartBody : List Part -> Body
multipartBody =
    H.multipartBody


type alias Part =
    H.Part


stringPart : String -> String -> Part
stringPart =
    H.stringPart


filePart : String -> File -> Part
filePart =
    H.filePart


bytesPart : String -> String -> Bytes -> Part
bytesPart =
    H.bytesPart


type alias Expect msg =
    H.Expect (Msg Log msg)


expectString : (Result Error String -> msg) -> Expect msg
expectString toMsg =
    H.expectString <| UpdateMsg << toMsg << result


expectJson : (Result Error a -> msg) -> Json.Decode.Decoder a -> Expect msg
expectJson toMsg =
    H.expectJson <| UpdateMsg << toMsg << result


expectBytes : (Result Error a -> msg) -> Bytes.Decode.Decoder a -> Expect msg
expectBytes toMsg =
    H.expectBytes <| UpdateMsg << toMsg << result


expectWhatever : (Result Error () -> msg) -> Expect msg
expectWhatever toMsg =
    H.expectWhatever <| UpdateMsg << toMsg << result


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String


result : Result H.Error a -> Result Error a
result result_ =
    case result_ of
        Ok value ->
            Ok value

        Err error ->
            Err <| toError error


toError : H.Error -> Error
toError error =
    case error of
        H.BadUrl url ->
            BadUrl url

        H.Timeout ->
            Timeout

        H.NetworkError ->
            NetworkError

        H.BadStatus status ->
            BadStatus status

        H.BadBody body ->
            BadBody body


track : String -> (Progress -> msg) -> Sub (Msg Log msg)
track string toMsg =
    H.track string <| UpdateMsg << toMsg << toProgress


type Progress
    = Sending
        { sent : Int
        , size : Int
        }
    | Receiving
        { received : Int
        , size : Maybe Int
        }


toProgress : H.Progress -> Progress
toProgress progress =
    case progress of
        H.Sending data ->
            Sending data

        H.Receiving data ->
            Receiving data


fractionSent :
    { sent : Int
    , size : Int
    }
    -> Float
fractionSent =
    H.fractionSent


fractionReceived : { received : Int, size : Maybe Int } -> Float
fractionReceived =
    H.fractionReceived


cancel : String -> Cmd Log msg
cancel =
    H.cancel


riskyRequest :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , expect : Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd Log msg
riskyRequest =
    H.riskyRequest


expectStringResponse : (Result x a -> msg) -> (Response String -> Result x a) -> Expect msg
expectStringResponse toMsg toResult =
    H.expectStringResponse (UpdateMsg << toMsg) <| toResult << toResponse


expectBytesResponse : (Result x a -> msg) -> (Response Bytes -> Result x a) -> Expect msg
expectBytesResponse toMsg toResult =
    H.expectBytesResponse (UpdateMsg << toMsg) <| toResult << toResponse


type Response body
    = BadUrl_ String
    | Timeout_
    | NetworkError_
    | BadStatus_ Metadata body
    | GoodStatus_ Metadata body


toResponse : H.Response body -> Response body
toResponse response =
    case response of
        H.BadUrl_ url ->
            BadUrl_ url

        H.Timeout_ ->
            Timeout_

        H.NetworkError_ ->
            NetworkError_

        H.BadStatus_ metadata body ->
            BadStatus_ metadata body

        H.GoodStatus_ metadata body ->
            GoodStatus_ metadata body


type alias Metadata =
    { url : String
    , statusCode : Int
    , statusText : String
    , headers : Dict String String
    }


task :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , resolver : Resolver x a
    , timeout :
        Maybe Float
    }
    -> Task x a
task =
    H.task


type alias Resolver x a =
    H.Resolver x a


stringResolver : (Response String -> Result x a) -> Resolver x a
stringResolver toResult =
    H.stringResolver <| toResult << toResponse


bytesResolver : (Response Bytes -> Result x a) -> Resolver x a
bytesResolver toResult =
    H.bytesResolver <| toResult << toResponse


riskyTask :
    { method : String
    , headers : List Header
    , url : String
    , body : Body
    , resolver : Resolver x a
    , timeout : Maybe Float
    }
    -> Task x a
riskyTask =
    H.riskyTask
