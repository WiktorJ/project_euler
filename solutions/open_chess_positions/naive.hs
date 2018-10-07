removeNth n xs = let (ys,zs) = splitAt n xs in  ys ++ (tail zs)

generateBoards 0 [] pairs = [pairs]
generateBoards 0 _ _ = []
generateBoards i x pairs = do
    let y = (removeNth (i - 1) x)
    generateBoards (length y) y (pairs ++ [(((length x) - 1), (x!!(i-1) -1 ))]) ++ generateBoards (i - 1) x pairs

getBoards size = generateBoards size [1..size] []


step board pos  
    | a < 0 = False
    | b >= (length board) = False
    | elem pos board = False
    | (a == 0) && (b == ((length board)-1)) = True
    | otherwise = step board (a, b+1) || step board (a-1, b) 
    where (a,b) = pos
        

solveNaive size = doSolveNaive size [1..size] [] 
doSolveNaive 0 [] pairs = do
    if step pairs (((length pairs)-1),0)
        then 1
    else 0
doSolveNaive 0 _ _ = 0
doSolveNaive i x pairs = do
    let y = (removeNth (i - 1) x)
    doSolveNaive (length y) y (pairs ++ [(((length x) - 1), (x!!(i-1) -1 ))]) + doSolveNaive (i - 1) x pairs
