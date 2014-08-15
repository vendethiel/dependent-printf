%default total

data Format = FEnd
            | FString Format
            | FInt Format
            | FList Format
            | FContent Char Format


format : List Char -> Format
format ('%'::'s'::xs) = FString $ format xs
format ('%'::'i'::xs) = FInt $ format xs
format ('%'::'a'::xs) = FList (format xs)
format (c::xs) = FContent c $ format xs
format [] = FEnd

interpFormat : Format -> Type
interpFormat FEnd = String
interpFormat (FString f) = String -> interpFormat f
interpFormat (FInt f) = Int -> interpFormat f
interpFormat (FList f) = String -> List String -> interpFormat f
interpFormat (FContent _ f) = interpFormat f 

formatString : String -> Format
formatString = format . unpack

toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction FEnd a = a
toFunction (FString f) a = \s => toFunction f $ a ++ s
toFunction (FInt f) a = \i => toFunction f $ a ++ show i
toFunction (FList f) a = \sep : String, s : List String => toFunction f $ a ++ (pack $ flatten $ intersperse (unpack sep) $ map unpack s)
toFunction (FContent c f) a = toFunction f $ a ++ singleton c


printf : (s : String) -> interpFormat (formatString s)
printf s = toFunction (formatString s) ""

main : IO ()
main = print $ printf "Hey %a" "," ["You boy", "over there"]
