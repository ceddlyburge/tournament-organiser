module Optimisation.Permutations exposing (permutations)

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



-- permutations : List a -> List (List a)
-- permutations xs =
--     let
--         perms ts is =
--             let
--                 interleave xs_ r =
--                     let
--                         ( _, zs ) =
--                             interleave_ is xs_ r
--                     in
--                     zs
--                 interleave_ f_ ys_ r_ =
--                     case ( f_, ys_, r_ ) of
--                         ( _, [], r ) ->
--                             ( ts, r )
--                         ( f, y :: ys, r ) ->
--                             let
--                                 ( us, zs ) =
--                                     interleave_ (f >> (::) y) ys r
--                             in
--                             ( y :: us, f {- t :: -} (y :: us) :: zs )
--             in
--             case ( ts, is ) of
--                 ( [], _ ) ->
--                     []
--                 ( t__ :: ts__, is__ ) ->
--                     List.foldr interleave (perms ts__ (is__ :: t__)) (permutations is__)
--     in
--     case xs of
--         [ a ] ->
--             [ [ a ] ]
--         xs0 ->
--             xs0 :: perms xs0 []
-- permutations            :: [a] -> [[a]]
-- permutations xs0        =  xs0 : perms xs0 []
--   where
--     perms []     _  = []
--     perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
--       where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
--             interleave' _ []     r = (ts, r)
--             interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
--                                      in  (y:us, f (t:y:us) : zs)
