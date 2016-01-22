import Data.List

join = intercalate

repeater :: String -> (Int -> String -> String)
repeater str = helper
	where helper times glue = join glue $ replicate times str