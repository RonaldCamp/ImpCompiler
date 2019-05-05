module average

export

average: String -> Double
average str = let numwords = wordCount str
                  totalLength = sum (allLengths (words str)) in
                  cast totalLength / cast numwords
        where
            wordCount: String -> Nat
            wordCount str = length (words str)

            allLengths: List String -> List Nat
            allLengths strs = map length strs
