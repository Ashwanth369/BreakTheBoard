module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

unitSquare :: Float
unitSquare = 100

fps = 60

maze = [0,0,3,3,2,3,0,0,
		0,0,3,3,3,2,0,0,
		3,3,3,3,0,0,3,3,
		3,3,3,3,0,0,2,1,
		3,2,1,1,0,2,1,1,
		0,0,1,1,3,2,1,1,
		2,2,0,1,0,0,2,1,
		2,2,0,2,0,0,3,2]

data PSGame = Game
	{ curstate :: [Int]
  	, dimensions :: Int
  	, blockLevel :: Int
  	, subBlockLocs :: Int
  	, curSubBlockLoc :: Int
  	, isTurn::Bool
  	, goalColor::Int
  	} deriving Show


initialState :: PSGame
initialState = Game
	{ curstate = maze
	, dimensions = 8
	, blockLevel = 8
	, subBlockLocs = 0
	, curSubBlockLoc = 0
	, isTurn = True
	, goalColor = 1
	}


window :: Display
window = FullScreen

background :: Color
background = white

square :: (Float,Float) -> PSGame -> Picture
square (x,y) game = translate (unitSquare*x) (-unitSquare*y) $ color sqcol $ rectangleSolid (unitSquare) (unitSquare)
	where
		l = curstate game
		n = dimensions game
		col = l!!(round (((y)*(fromIntegral n))+x))
		sqcol = if col == 0 then dark yellow
			else if col == 1 then dark red
			else if col == 2 then dark blue
			else if col == 3 then dark green
			else white

createBorder :: PSGame -> Picture
createBorder game = pictures [border]
	where
		lev = fromIntegral (blockLevel game)
		x = fromIntegral ((curSubBlockLoc game) `mod` (dimensions game))
		y = fromIntegral ((curSubBlockLoc game) `div` (dimensions game))
		i = x*unitSquare + (((lev-1)/2)*unitSquare)
		j = y*unitSquare + (((lev-1)/2)*unitSquare)
		border = translate (i) (-j) $ color (light cyan) $ rectangleWire (unitSquare*lev) (unitSquare*lev)

calculateScore::Int -> PSGame->Int
calculateScore col game = score
	where
			lev = blockLevel game
			dim = dimensions game
			curr = curstate game
			utilList = [(if (curr!!((i*dim)+j))==col then 1 else 0 )| i<-[0,(dim-1)],j<-[0..dim-1]] ++  [(if (curr!!((i*dim)+j))==col then 1 else 0 )| i<-[1..(dim-2)],j<-[0,dim-1]]
			score = sum utilList
--child :: PSGame -> PSGame
--child game = game { children = new }
--	where
--		l = curstate game
	--	m = dimensions game
	--	n = m `div` 2
	--	if ((l == take m*m (repeat 0)) || (l == take m*m (repeat 1)) || (l == take m*m (repeat 2)) || (l == take m*m (repeat 3))) then new = [[]]
	--	else new = [[ (l!!((i+p)*m+j+q)) | i<-[0..n-1],j<-[0..n-1] ] | p<-[0,n],q<-[0,n] ]


cwrotate :: PSGame -> PSGame
cwrotate game = game { curstate = new }
	where
		p = dimensions game
		l = curstate game
		m = blockLevel game
		n = m `div` 2
		pos = curSubBlockLoc game
		v = pos `div` p
		arr = [ l!!((pos+j+i)) | i<-[0,p..p*(m-1)],j<-[0..m-1] ]
		prev = [[ l!!(j+i) | j<-[v*p..(pos-1)]] | i<-[0,p..p*(m-1)] ] 
		next = [[ l!!(j+i) | j<-[pos+m..((v+1)*p)-1]] | i<-[0,p..p*(m-1)] ]
		temp = [[[(arr!!((i+n)*m+j+q)) | j<- [0..(n-1)]] ++ [(arr!!(i*m+k+q)) | k<-[0..(n-1)]] | i<-[0..(n-1)]] |  q<-[0,n]]
		tempp = [ (prev!!(i*n+j)) ++ ((temp!!i)!!j) ++ (next!!(i*n+j)) | i<-[0..1],j<-[0..n-1]]
		new = take (v*p) l ++ [(((tempp!!i)!!j))| i<-[0..m-1],j<-[0..(p-1)]] ++ drop ((v+m)*p) l 
			  
ccwrotate :: PSGame -> PSGame
ccwrotate game = c3
	where
		c1 = cwrotate game
		c2 = cwrotate c1
		c3 = cwrotate c2

vswap :: PSGame -> PSGame
vswap game = game { curstate = new }
	where
		p = dimensions game
		l = curstate game
		m = blockLevel game
		n = m `div` 2
		pos = curSubBlockLoc game
		v = pos `div` p
		arr = [ l!!((pos+j+i)) | i<-[0,p..p*(m-1)],j<-[0..m-1] ]
		prev = [[ l!!(j+i) | j<-[v*p..(pos-1)]] | i<-[0,p..p*(m-1)] ] 
		next = [[ l!!(j+i) | j<-[pos+m..((v+1)*p)-1]] | i<-[0,p..p*(m-1)] ]
		temp = [[ [(arr!!((i+n-q)*m+j)) | j<- [0..m-1]] | i<-[0..(n-1)]] |  q<-[0,n]]
		tempp = [ (prev!!(i*n+j)) ++ ((temp!!i)!!j) ++ (next!!(i*n+j)) | i<-[0..1],j<-[0..n-1]]
		new = take (v*p) l ++ [(((tempp!!i)!!j))| i<-[0..m-1],j<-[0..(p-1)]] ++ drop ((v+m)*p) l

hswap :: PSGame -> PSGame
hswap game = game { curstate = new }
	where
		p = dimensions game
		l = curstate game
		m = blockLevel game
		n = m `div` 2
		pos = curSubBlockLoc game
		v = pos `div` p
		arr = [ l!!((pos+j+i)) | i<-[0,p..p*(m-1)],j<-[0..m-1] ]
		prev = [[ l!!(j+i) | j<-[v*p..(pos-1)]] | i<-[0,p..p*(m-1)] ] 
		next = [[ l!!(j+i) | j<-[pos+m..((v+1)*p)-1]] | i<-[0,p..p*(m-1)] ]
		temp = [ [(arr!!(i*m+j)) | j<- [n..(m-1)]] ++ [(arr!!(i*m+k)) | k<- [0..(n-1)]]  | i<-[0..(m-1)]]
		tempp = [ (prev!!(i*n+j)) ++ (temp!!(i*n+j)) ++ (next!!(i*n+j)) | i<-[0..1],j<-[0..n-1]]
		new = take (v*p) l ++ [(((tempp!!i)!!j))| i<-[0..m-1],j<-[0..(p-1)]] ++ drop ((v+m)*p) l

render :: PSGame -> Picture
render game = pictures [translate (-300) (350) $ field,translate (-300) (350) $ curblock, score1, score2, player1, player2, turn]
	where
		m = fromIntegral (dimensions game)
		field = pictures [ square (x,y) game | x<-[0..m-1], y<-[0..m-1] ]
		curblock = createBorder game
		score1 = scale (0.5) (0.5) $ translate (-1700) (250) $ text (show (calculateScore 1 game))
		score2 = scale (0.5) (0.5) $ translate (-1700) (-350) $ text (show (calculateScore 0 game))
		turn = if (isTurn game) then scale (0.5) (0.5) $ translate (-1200) (450) $ color (dark red) $ circleSolid 20
			   else scale (0.5) (0.5) $ translate (-1200) (-150) $ color (dark yellow) $ circleSolid 20
		player1 = scale (0.5) (0.5) $ translate (-1800) (400) $ text (show "Player1")
		player2 = scale (0.5) (0.5) $ translate (-1800) (-200) $ text (show "Player2")

handleEvents::Event -> PSGame -> PSGame
handleEvents (EventKey(Char 'w') (Down)_ _) game = 
		if blockLevel game > 2 then game { blockLevel = (blockLevel game) `div` 2, subBlockLocs = 0} 
		else game

handleEvents (EventKey(Char 's') (Down)_ _) game = 
		if (2*(blockLevel game)) <= dimensions game then game { blockLevel = (blockLevel game)*2, curSubBlockLoc = 0 , subBlockLocs = 0 }
		else game

handleEvents (EventKey(SpecialKey KeyUp) _ _ _) game = 
		if blockLevel game == dimensions game then game
		else if subBlockLocs game == 0 || subBlockLocs game == 1 then game
		else if subBlockLocs game == 2 then game { curSubBlockLoc = ( (curSubBlockLoc game) - ((blockLevel game)*(dimensions game)) ) , subBlockLocs = 0 }
		else if subBlockLocs game == 3 then game { curSubBlockLoc = ( (curSubBlockLoc game) - ((blockLevel game)*(dimensions game)) ) , subBlockLocs = 1 }
		else game

handleEvents (EventKey(SpecialKey KeyDown) _ _ _) game = 
		if blockLevel game == dimensions game then game
		else if subBlockLocs game == 2 || subBlockLocs game == 3 then game
		else if subBlockLocs game == 0 then game { curSubBlockLoc = ( (curSubBlockLoc game) + ((blockLevel game)*(dimensions game)) ) , subBlockLocs = 2 } 
		else if subBlockLocs game == 1 then game { curSubBlockLoc = ( (curSubBlockLoc game) + ((blockLevel game)*(dimensions game)) ) , subBlockLocs = 3 }
		else game

handleEvents (EventKey(SpecialKey KeyLeft) _ _ _) game = 
		if blockLevel game == dimensions game then game
		else if subBlockLocs game == 2 || subBlockLocs game == 0 then game
		else if subBlockLocs game == 1 then game { curSubBlockLoc = ( (curSubBlockLoc game) - (blockLevel game) ) , subBlockLocs = 0 } 
		else if subBlockLocs game == 3 then game { curSubBlockLoc = ( (curSubBlockLoc game) - (blockLevel game) ) , subBlockLocs = 2 }
		else game

handleEvents (EventKey(SpecialKey KeyRight) _ _ _) game = 
		if blockLevel game == dimensions game then game
		else if subBlockLocs game == 1 || subBlockLocs game == 3 then game
		else if subBlockLocs game == 0 then game { curSubBlockLoc = ( (curSubBlockLoc game) + (blockLevel game) ) , subBlockLocs = 1 } 
		else if subBlockLocs game == 2 then game { curSubBlockLoc = ( (curSubBlockLoc game) + (blockLevel game) ) , subBlockLocs = 3 }
		else game

handleEvents (EventKey(Char 'r') (Down)_ _) game = game { curstate = newState, isTurn = not (isTurn game), goalColor = 1 - (goalColor game) , curSubBlockLoc = 0 , subBlockLocs = 0 , blockLevel = dimensions game }
	where
		newState = curstate $ cwrotate game

handleEvents (EventKey(Char 'l') (Down)_ _) game = game { curstate = newState, isTurn = not (isTurn game), goalColor = 1 - (goalColor game) , curSubBlockLoc = 0 , subBlockLocs = 0 , blockLevel = dimensions game}
	where
		newState = curstate $ ccwrotate game

handleEvents (EventKey(Char 'v') (Down)_ _) game = game { curstate = newState, isTurn = not (isTurn game), goalColor = 1 - (goalColor game) , curSubBlockLoc = 0 , subBlockLocs = 0 , blockLevel = dimensions game}
	where
		newState = curstate $ vswap game

handleEvents (EventKey(Char 'h') (Down)_ _) game = game { curstate = newState, isTurn = not (isTurn game), goalColor = 1 - (goalColor game) , curSubBlockLoc = 0 , subBlockLocs = 0 , blockLevel = dimensions game}
	where
		newState = curstate $ hswap game


handleEvents _ game = game
		

main = play window background 60 initialState render handleEvents update
	where
		update :: Float -> PSGame -> PSGame
		update _ game = game
