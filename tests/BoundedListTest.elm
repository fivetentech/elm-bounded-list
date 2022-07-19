module BoundedListTest exposing (suite)

import BoundedList exposing (BoundedList)
import Expect
import Fuzz exposing (Fuzzer)
import List.Extra as List
import Random
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Tests suite for BoundedList"
        [ simpleTests
        , snafuTests
        , fuzzyTests
        ]


simpleTests : Test
simpleTests =
    describe "A set of some simple quick tests"
        [ test "empty" <|
            \_ ->
                BoundedList.empty 4
                    |> BoundedList.toList
                    |> Expect.equal []
        , describe "fromList"
            [ test "with a small enough init list" <|
                \_ ->
                    BoundedList.fromList 4 [ 1, 2 ]
                        |> BoundedList.toList
                        |> Expect.equal [ 1, 2 ]
            , test "with a too large init list" <|
                \_ ->
                    BoundedList.fromList 2 [ 1, 2, 3, 4 ]
                        |> BoundedList.toList
                        |> Expect.equal [ 3, 4 ]
            ]
        , describe "appendStart"
            [ test "with a new list larger than the allowed size" <|
                \_ ->
                    BoundedList.fromList 2 [ 4, 5, 6 ]
                        |> BoundedList.appendStart [ 1, 2, 3 ]
                        |> BoundedList.toList
                        |> Expect.equal [ 1, 2 ]
            , test "with a maxSize that can accomodate both the existing elements and the new ones" <|
                \_ ->
                    BoundedList.fromList 6 [ 4, 5, 6 ]
                        |> BoundedList.appendStart [ 1, 2, 3 ]
                        |> BoundedList.toList
                        |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
            , test "with a cumulated list too large" <|
                \_ ->
                    BoundedList.fromList 4 [ 4, 5 ]
                        |> BoundedList.appendStart [ 1, 2, 3 ]
                        |> BoundedList.toList
                        |> Expect.equal [ 1, 2, 3, 4 ]
            ]
        , describe "appendEnd"
            [ test "with a new list larger than the allowed size" <|
                \_ ->
                    BoundedList.fromList 2 [ 1, 2, 3 ]
                        |> (\bounded -> BoundedList.appendEnd bounded [ 4, 5, 6 ])
                        |> BoundedList.toList
                        |> Expect.equal [ 5, 6 ]
            , test "with a maxSize that can accomodate both the existing elements and the new ones" <|
                \_ ->
                    BoundedList.fromList 6 [ 1, 2, 3 ]
                        |> (\bounded -> BoundedList.appendEnd bounded [ 4, 5, 6 ])
                        |> BoundedList.toList
                        |> Expect.equal [ 1, 2, 3, 4, 5, 6 ]
            , test "with a cumulated list too large" <|
                \_ ->
                    BoundedList.fromList 4 [ 1, 2 ]
                        |> (\bounded -> BoundedList.appendEnd bounded [ 3, 4, 5 ])
                        |> BoundedList.toList
                        |> Expect.equal [ 2, 3, 4, 5 ]
            ]
        , describe "addLast"
            [ test "list too big and adding one item" <|
                \_ ->
                    BoundedList.fromList 2 [ 1, 2, 3 ] |> BoundedList.addLast 4 |> BoundedList.toList |> Expect.equal [ 3, 4 ]
            , test "list small enough to add an item without dropping" <|
                \_ ->
                    BoundedList.fromList 4 [ 1, 2, 3 ] |> BoundedList.addLast 4 |> BoundedList.toList |> Expect.equal [ 1, 2, 3, 4 ]
            , test "list small enough for one add not too" <|
                \_ ->
                    BoundedList.fromList 4 [ 1, 2, 3 ]
                        |> BoundedList.addLast 4
                        |> BoundedList.addLast 5
                        |> BoundedList.toList
                        |> Expect.equal [ 2, 3, 4, 5 ]
            ]
        , describe "cons"
            [ test "list too big and adding one item" <|
                \_ ->
                    BoundedList.fromList 2 [ 1, 2, 3 ]
                        |> BoundedList.cons 4
                        |> BoundedList.toList
                        |> Expect.equal [ 4, 2 ]
            , test "list small enough to add an item without dropping" <|
                \_ ->
                    BoundedList.fromList 4 [ 1, 2, 3 ]
                        |> BoundedList.cons 4
                        |> BoundedList.toList
                        |> Expect.equal [ 4, 1, 2, 3 ]
            , test "list small enough for one add not too" <|
                \_ ->
                    BoundedList.fromList 4 [ 1, 2, 3 ]
                        |> BoundedList.cons 4
                        |> BoundedList.cons 5
                        |> BoundedList.toList
                        |> Expect.equal [ 5, 4, 1, 2 ]
            , test "start with an empty list, add 2 items, supports only 1" <|
                \_ ->
                    BoundedList.empty 1
                        |> BoundedList.cons 1
                        |> BoundedList.cons 0
                        |> BoundedList.toList
                        |> Expect.equal [ 0 ]
            ]
        ]


snafuTests : Test
snafuTests =
    describe "SNAFU test, how does the library works if you're not nice with it... "
        [ describe "maxSize of 0"
            [ test "empty" <|
                \_ ->
                    BoundedList.empty 0
                        |> BoundedList.toList
                        |> Expect.equal []
            , test
                "fromList"
              <|
                \_ ->
                    BoundedList.fromList 0 [ 1, 2, 3, 4 ]
                        |> BoundedList.toList
                        |> Expect.equal []
            , test "appendEnd" <|
                \_ ->
                    BoundedList.fromList 0 []
                        |> (\bounded -> BoundedList.appendEnd bounded [ 0 ])
                        |> BoundedList.toList
                        |> Expect.equal []
            , test "addLast" <|
                \_ ->
                    BoundedList.fromList 0 []
                        |> BoundedList.addLast 0
                        |> BoundedList.toList
                        |> Expect.equal []
            , test "cons" <|
                \_ ->
                    BoundedList.fromList 0 []
                        |> BoundedList.cons 0
                        |> BoundedList.toList
                        |> Expect.equal []
            ]
        , describe "negative maxSize"
            [ test "empty" <|
                \_ ->
                    BoundedList.empty -2
                        |> BoundedList.toList
                        |> Expect.equal []
            , test "fromList" <|
                \_ ->
                    BoundedList.fromList -4 [ 1, 2, 3, 4, 5 ]
                        |> BoundedList.toList
                        |> Expect.equal []
            ]
        ]


positiveInt : Fuzzer Int
positiveInt =
    Fuzz.intRange 0 Random.maxInt


fuzzyTests : Test
fuzzyTests =
    describe "Fuzzy testing aka property-based testing"
        [ describe "fromList"
            [ fuzz (Fuzz.tuple ( Fuzz.list Fuzz.int, positiveInt )) "the size of the resulting list is always the min between the capacity and the initial list size" <|
                \( list, size ) ->
                    BoundedList.fromList size list
                        |> BoundedList.toList
                        |> List.length
                        |> Expect.equal (min (List.length list) size)
            , describe "appendEnd"
                [ fuzz (Fuzz.tuple3 ( Fuzz.list Fuzz.int, Fuzz.list Fuzz.int, positiveInt )) "the order is always the same as in a regular List.append" <|
                    \( listA, listB, size ) ->
                        BoundedList.fromList size listA
                            |> (\bounded -> BoundedList.appendEnd bounded listB)
                            |> BoundedList.toList
                            |> (\list -> List.isSuffixOf list (listA ++ listB))
                            |> Expect.true "the resulting list should be a suffix of the regular append function"
                , fuzz (Fuzz.tuple ( Fuzz.list Fuzz.int, positiveInt )) "appening an empty list is idempotent" <|
                    \( list, size ) ->
                        let
                            bounded : BoundedList Int
                            bounded =
                                BoundedList.fromList size list
                        in
                        BoundedList.appendEnd bounded []
                            |> Expect.equal bounded
                ]
            ]
        , fuzz (Fuzz.tuple3 ( Fuzz.list Fuzz.int, Fuzz.list Fuzz.int, Fuzz.int )) "defining appendEnd in terms of addLast should result in the same thing" <|
            \( listA, listB, size ) ->
                listA
                    |> List.foldl BoundedList.addLast (BoundedList.fromList size listB)
                    |> Expect.equal (BoundedList.appendEnd (BoundedList.fromList size listB) listA)
        , fuzz (Fuzz.tuple3 ( Fuzz.list Fuzz.int, Fuzz.list Fuzz.int, Fuzz.int )) "defining appendStart in terms of cons should result in the same thing" <|
            \( listA, listB, size ) ->
                listA
                    |> List.foldr BoundedList.cons (BoundedList.fromList size listB)
                    |> Expect.equal (BoundedList.appendStart listA (BoundedList.fromList size listB))
        , fuzz (Fuzz.tuple ( Fuzz.list Fuzz.int, Fuzz.int )) "defining fromList in terms of empty and addLast" <|
            \( list, size ) ->
                list
                    |> List.foldl BoundedList.addLast (BoundedList.empty size)
                    |> Expect.equal (BoundedList.fromList size list)
        ]
