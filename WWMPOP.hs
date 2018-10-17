-- Patrick Freel
-- CSCI 450
-- Assignment 2
module WWMPOP (
    formatDollars, formatLine, formatLines, calcSubtotal, formatAmt, formatBill, look, priceCart, makeBill, makeReceipt
    )
where

type BarCode = Int
type Price = Int
type Name = String

type PriceList = [(BarCode,Name,Price)]

database :: PriceList
database = [ (1848, "Vanilla yogurt cups (4)",  188),
             (1620, "Ground turkey (1 lb)",     316),
             (1492, "Corn flakes cereal",       299),
             (1773, "Black tea bags (100)",     307),
             (2525, "Athletic socks (6)",       825),
             (9595, "Claw hammer",              788),
             (1945, "32-in TV",                 13949),
             (1066, "Zero sugar cola (12)",     334),
             (2018, "Haskell programming book", 4495)]

type CartItems = [BarCode]
type CartPrices = [(Name,Price)]
-- Price1 subtotal, price2 tax, price3 total
type Bill = (CartPrices, Price, Price, Price)

taxRate :: Double
taxRate = 0.07

lineWidth :: Int
lineWidth = 34

formatDollars :: Price -> String
formatDollars price
  | (price<0) = "Negative value"
  | length (show price) == 0 = "0.00"
  | length (show price) == 1 = "0.0" ++ show price
  | length (show price) == 2 = "0." ++ show price
  | otherwise = take(length((show price))-2) (show price) ++ "." ++ drop((length(show price))-2)(show price)

formatLine :: (Name, Price) -> String
formatLine (name, price) = name ++ " " ++ concat ["." | r <- [0..lineWidth]] ++ " " ++ formatDollars price ++ "\n"

formatLines :: CartPrices -> String
formatLines [] = ""
formatLines cartPrices = (formatLine(head cartPrices)) ++ formatLines(tail cartPrices)

calcSubtotal :: CartPrices -> Price
calcSubtotal [] = 0
calcSubtotal cartPrices = sum [x | (_,x) <- cartPrices]

formatAmt :: String -> Price -> String
formatAmt name price = formatLine(name, price)

-- bill(CartItems, Price, Price, Price) CartItems[(name,price),(name,price)]
formatBill :: Bill -> String
formatBill (w,x,y,z) = formatLines(w) ++ " " ++ show(x) ++ " " ++ show(y) ++ " " ++ show(z) ++ "\n"

look :: PriceList -> BarCode -> (Name,Price)
look priceList barCode
  | null inside = ("None", 0)
  | otherwise = head inside
    where inside = [(n,p) | (b,n,p) <- priceList, b == barCode]

priceCart :: PriceList -> CartItems -> CartPrices
priceCart priceList [] = []
priceCart priceList cartItems = look priceList (head cartItems) : priceCart priceList (tail cartItems)

makeBill :: CartPrices -> Bill
makeBill cartPrices = (cartPrices, subTotal, taxTotal, total)
  where subTotal = calcSubtotal cartPrices
        taxTotal = round (taxRate * (fromIntegral(subTotal :: Int)))
        total = subTotal + taxTotal

makeReceipt :: PriceList -> CartItems -> String
makeReceipt priceList cartItems = formatBill(makeBill(priceCart priceList cartItems))
