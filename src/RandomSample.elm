module RandomSample exposing (randomSample)

import Random exposing (Generator)
import Set


randomSampleFromSet : Int -> Set.Set comparable -> Generator (List comparable)
randomSampleFromSet n xs =
    case ( n > 0, Set.toList xs ) of
        ( True, h :: t ) ->
            Random.uniform h t
                |> Random.andThen
                    (\y ->
                        randomSampleFromSet (n - 1) (Set.remove y xs)
                            |> Random.map (\ys -> y :: ys)
                    )

        _ ->
            Random.constant []


randomSample : Int -> List comparable -> Generator (List comparable)
randomSample n xs =
    randomSampleFromSet n (Set.fromList xs)
