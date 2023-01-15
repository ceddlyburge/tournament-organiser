module Optimisation.Permutations exposing (permutations)

import Html exposing (pre)
import List.Extra


{-| Return the list of of all permutations of a list. The result is in lexicographic order.
permutations [ 1, 2, 3 ]
--> [ [ 1, 2, 3 ], [ 1, 3, 2 ], [ 2, 1, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ], [ 3, 2, 1 ] ]

    from List.Extra, with addedd curtailment

-}
permutations : (a -> a -> Bool) -> List a -> List (List a)
permutations curtail xs =
    permutationsInternal curtail Nothing xs


permutationsInternal : (a -> a -> Bool) -> Maybe a -> List a -> List (List a)
permutationsInternal curtail previousValue xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    if maybeCurtail curtail previousValue y then
                        [ [] ]

                    else
                        List.map
                            (\permuation ->
                                case ( ys, permuation ) of
                                    ( _ :: _, [] ) ->
                                        []

                                    ( _, zs ) ->
                                        y :: zs
                            )
                            (permutationsInternal curtail (Just y) ys)
            in
            List.concatMap f (List.Extra.select xs)
                |> List.filter (\list -> List.length list > 0)


maybeCurtail : (a -> a -> Bool) -> Maybe a -> a -> Bool
maybeCurtail curtail maybePreviousValue currentValue =
    Maybe.map2 curtail maybePreviousValue (Just currentValue)
        |> Maybe.withDefault False
