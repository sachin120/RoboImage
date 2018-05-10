writeImage fname maxval width height mat = do
 writeFile fname header
 mapM_ (mapM_ (appendFile fname )) $ map (map (formatTriple maxlen)) mat
 where
  maxlen = length $ show maxval
  header = ppmheader ++ (show width) ++ " " ++ (show height) ++ " " ++ (show maxval) ++ "\n"
  ppmheader = "P3\n"
  formatTriple max (a,b,c) =  a' ++  b' ++  c' ++ "\n"
   where
    a' = pad max a
    b' = pad max b
    c' = pad max c
    pad m a = a'
     where
      a' = (take (p+1) $ repeat ' ') ++ (show a)
      p  = m - (length $ show a)

m2 cell = writeImage "robo.ppm" 255 (cell) (cell) mat1 -- row = no. of row's , col = no. of col's ,cell = size of single cell
 where mat1 = mat cell

-- To Exicute the code

-- $ ghci robo.hs
-- $ m2 400
rc = (255,0,0)
gc = (0,255,0)
bc = (0,0,255)

--roboc = (29,83,109)
roboc = (161,167,135)

lineBorder cell = [take (cell) $ repeat rc]
lineSimpl  cell = [rc] ++ (take (cell-1) $ repeat gc) 
headrobo cell = take ((div cell 5)-2) $ repeat ( [rc] ++ (take (((div cell 5)* 2)-1) $ repeat gc) ++ (take (div cell 5) $ repeat roboc) ++ (take ((div cell 5)* 2) $ repeat gc) )


neck cell = take (div ((div cell 5)-2) 4) $ repeat ([rc] ++ (take ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) $ repeat gc) ++ ( take (div (div cell 5) 2)  $ repeat roboc ) ++ (take ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) $ repeat gc) ++ [gc])


body cell = (take (div cell 5) $ repeat ([rc] ++ (take ( (((div cell 5)* 2)-1) - (  ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1)  ) *2 ) $ repeat gc) ++  (take (  ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1)  ) $ repeat roboc) ++ (take (  ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1)  ) $ repeat gc) ++ (take (div cell 5) $ repeat roboc) ++ (take (  ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1)  ) $ repeat gc) ++ (take (  ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1)  ) $ repeat roboc) ++ (take ( (((div cell 5)* 2)-1) - (  ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1)  ) *2 ) $ repeat gc) ++ [gc]) ) ++ (take ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) $ repeat ( [rc] ++ (take (((div cell 5)* 2)-1) $ repeat gc) ++ (take (div cell 5) $ repeat roboc) ++ (take ((div cell 5)* 2) $ repeat gc) ) )

--(div cell 5)
legs cell = take ((div cell 5)-2) $ repeat([rc] ++ (take (((div cell 5)* 2)-1) $ repeat gc) ++ (take ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) $ repeat roboc) ++ (take (div (div cell 5) 2) $ repeat gc) ++ (take ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) $ repeat roboc) ++ (take (((div cell 5)* 2)) $ repeat gc))

--shoe size ( ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) )*2)
--( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) )
shoe cell = take ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) $ repeat ( [rc] ++ (take ( (((div cell 5)* 2)-1) - ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) ) $ repeat gc) ++ (take ( ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) )*2) $ repeat roboc)  ++ (take (div (div cell 5) 2) $ repeat gc) ++ (take ( ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) )*2) $ repeat roboc) ++ (take ( (((div cell 5)* 2)-1) - ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) ) $ repeat gc) ++[gc])


mat cell = (lineBorder cell) ++ (take (div cell 10) $ repeat (lineSimpl cell)) ++ (headrobo cell) ++ (neck cell)  ++ (body cell) ++ (take ( ((((div cell 5)* 2)-1) + (div (div cell 5) 4)) - (((div cell 5)* 2)-1) ) $ repeat (lineSimpl cell)) ++ (legs cell) ++ (shoe cell) ++ (take (div cell 10) $ repeat (lineSimpl cell))


