comprehensive xs ys = [(x,y) | x <-xs, y <- ys]
monadic xs ys = do {x <- xs; y<- ys; return (x,y)}

blockyDo xs ys = do
    x <- xs
    y<- ys
    return (x, y)

wordCount = print . length .words =<< getContents
