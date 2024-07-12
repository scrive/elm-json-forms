module PointerTest exposing (suite)

import Expect
import Form.Pointer as Pointer
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Json.Schema.Form.Pointer"
        [ describe "toString"
            [ test "empty" <|
                \_ ->
                    Expect.equal "#" (Pointer.toString [])
            , test "nearly empty" <|
                \_ ->
                    Expect.equal "#/" (Pointer.toString [ "" ])
            , test "simple" <|
                \_ ->
                    Expect.equal "#/abc/def" (Pointer.toString [ "abc", "def" ])
            , test "escaping" <|
                \_ ->
                    Expect.equal "#/~01/~1" (Pointer.toString [ "~1", "/" ])
            ]
        , describe "fromString"
            [ test "empty" <|
                \_ ->
                    Expect.equal (Pointer.fromString "#") (Ok [])
            , test "nearly empty" <|
                \_ ->
                    Expect.equal (Pointer.fromString "#/") (Ok [ "" ])
            , test "simple" <|
                \_ ->
                    Expect.equal (Pointer.fromString "#/abc/def") (Ok [ "abc", "def" ])
            , test "escaping" <|
                \_ ->
                    Expect.equal (Pointer.fromString "#/~01/~1") (Ok [ "~1", "/" ])
            ]
        ]
