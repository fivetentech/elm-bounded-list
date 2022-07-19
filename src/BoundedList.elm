module BoundedList exposing
    ( BoundedList
    , empty, fromList
    , appendEnd, appendStart, toList
    )

{-| BoundedList are just list with a set maximum size. If you've reached the max size and add more elements, then some will be dropped!


# Definition

@docs BoundedList


# Creating BoundedList

@docs empty, fromList


# Manipulating BoundedLists

@docs appendEnd, appendStart, toList

-}


{-| A list of `itemType` that will have a maximum number of items within.

The type is opaque so refer to the next section to see how to create a `BoundedList`

-}
type BoundedList itemType
    = BoundedList Int (List itemType)


{-| Create an empty BoundedList with a specific `maxSize`

    (empty 4 |> tolist) == []

-}
empty : Int -> BoundedList a
empty maxSize =
    BoundedList maxSize []


{-| Create a BoundedList from a given list and maximum size. If the given list doesn't respect the size constraint, elements from the begining of the list will be drop to fit in.

    (fromList 2 [ 1, 2, 3, 4 ] |> toList) == [ 3, 4 ]

-}
fromList : Int -> List a -> BoundedList a
fromList maxSize list =
    if List.length list >= maxSize then
        list
            |> List.drop (List.length list - maxSize)
            |> BoundedList maxSize

    else
        BoundedList maxSize list


{-| extract the List part from a BoundedList

    (empty 4 |> tolist) == []

    (fromList 4 [ 1, 2 ] |> tolist) == [ 1, 2 ]

-}
toList : BoundedList a -> List a
toList (BoundedList _ list) =
    list


{-| take a List and adds it at the start of an existing BoundedList.
If the resulting list is too large, elements from the end of the list are dropped until it fits.

    (appendStart [ 1, 2, 3 ] (fromList 2 [ 4, 5, 6 ]) |> toList) == [ 1, 2 ]

    (appendStart [ 1, 2, 3 ] (fromList 6 [ 4, 5, 6 ]) |> toList) == [ 1, 2, 3, 4, 5, 6 ]

    (appendStart [ 1, 2, 3 ] (fromList 4 [ 4, 5 ]) |> toList) == [ 1, 2, 3, 4 ]

-}
appendStart : List a -> BoundedList a -> BoundedList a
appendStart newItems (BoundedList maxSize list) =
    let
        newItemsCount : Int
        newItemsCount =
            List.length newItems
    in
    if newItemsCount >= maxSize then
        -- ignore the current list, just fit the new one to the proper size and yolo
        List.take maxSize newItems
            |> BoundedList maxSize

    else
        let
            currentSize : Int
            currentSize =
                List.length list
        in
        if (currentSize + newItemsCount) > maxSize then
            -- drop as many elements of the existing list as necessary to respect the maxSize
            list
                |> List.take (maxSize - newItemsCount)
                |> (++) newItems
                |> BoundedList maxSize

        else
            BoundedList maxSize (newItems ++ list)


{-| take a List and adds it at the end of an existing BoundedList.
If the resulting list is too large, elements from the top of the list are dropped until it fits

    (appendEnd (fromList 2 [ 1, 2, 3 ]) [ 4, 5, 6 ] |> toList) == [ 5, 6 ]

    (appendEnd (fromList 6 [ 1, 2, 3 ]) [ 4, 5, 6 ] |> toList) == [ 1, 2, 3, 4, 5, 6 ]

    (appendEnd (fromList 4 [ 1, 2 ]) [ 3, 4, 5 ] |> toList) == [ 2, 3, 4, 5 ]

-}
appendEnd : BoundedList a -> List a -> BoundedList a
appendEnd (BoundedList maxSize list) newItems =
    let
        newItemsCount : Int
        newItemsCount =
            List.length newItems
    in
    if newItemsCount >= maxSize then
        -- ignore the current list, just fit the new one to the proper size and yolo
        fromList maxSize newItems

    else
        let
            currentSize : Int
            currentSize =
                List.length list
        in
        if (currentSize + newItemsCount) > maxSize then
            -- drop as many elements of the existing list as necessary to respect the maxSize
            list
                |> List.drop (currentSize + newItemsCount - maxSize)
                |> (\cutList -> cutList ++ newItems)
                |> BoundedList maxSize

        else
            BoundedList maxSize (list ++ newItems)
