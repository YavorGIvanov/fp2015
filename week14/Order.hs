data Order = Online Float Int Int | Offline Float

isOnline :: Order -> Bool
isOnline (Online _ _ _) = True
isOnline (Offline _) = False

timeUntilReceiving :: Order -> Int
timeUntilReceiving (Offline _) = error "Invalid type of Order"
timeUntilReceiving (Online _ _ x) = x

getPrice :: Order -> Float
getPrice (Offline price) = price
getPrice (Online price _ _) = price

totalPrice :: [Order] -> Float
totalPrice orders = sum $ map getPrice orders

onlineOrders :: [Order] -> Int
onlineOrders orders = length $ filter isOnline orders

isExpensive :: Order -> Bool
isExpensive ord = getPrice ord > 100

instance Show Order where
	show (Online price num time) = "Online Order -> Number of Order: " ++ show num ++ ", Price: " ++ show price ++ ", Deliver Time left: " ++ show time
	show (Offline price) = "Offline Order -> Price: " ++ show price

instance Eq Order where
	(==) (Online _ _ _) (Offline _) = False
	(==) (Offline _) (Online _ _ _) = False
	(==) (Online _ num _) (Online _ num2 _) = num == num2
	(==) (Offline price) (Offline price2) = price == price2
