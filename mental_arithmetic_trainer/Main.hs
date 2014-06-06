import Graphics.UI.Gtk
import Data.IORef
import System.Random
import Control.Monad

main :: IO ()
main = do
  initGUI
  window <- windowNew
  vbox <- vBoxNew True 10
  answerBox <- hBoxNew True 10
  problemLabel <- labelNew Nothing
  newGame problemLabel answerBox window
  windowSetTitle window "Mini Calc"
  containerAdd window vbox
  boxPackStart vbox answerBox PackGrow 0
  boxPackStart vbox problemLabel PackGrow 0
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

newGame :: Label -> HBox -> Window -> IO ()
newGame problemLabel answerBox window = do
  (problem, solution) <- newTask
  labelSetText problemLabel problem
  containerForeach answerBox (\w -> containerRemove answerBox w)
  
  solutions <- possibleSolutions solution
  forM_ solutions (\i -> do genButton answerBox (Just i))
  widgetShowAll window
  containerForeach answerBox (\w -> setButtonAction solution (castToButton w) problemLabel answerBox window)
  return ()

possibleSolutions :: Int -> IO [Int]
possibleSolutions solution = do
  count <- randomRIO (0, 10)
  xs <- randomNumbers count (return [])
  xs' <- randomNumbers 10 (return (xs ++ [solution]))
  return xs'

randomNumbers :: Int -> IO [Int] -> IO [Int]
randomNumbers count numbers = do
  numbers' <- numbers
  rand <- randomRIO (1, 10) :: IO Int
  if (length numbers' == count)
               then numbers
               else randomNumbers count $ return $ numbers' ++ [rand]
  
newTask :: IO (String, Int)
newTask = do
  solution <- randomRIO (1, 10) :: IO Int
  operation <- randomRIO (1, 4) :: IO Int
  num <- randomRIO (1, 10) :: IO Int
  let problem
        | operation == 1 = show (solution - num) ++ " + " ++ show num
        | operation == 2 = show (solution * num) ++ " / " ++ show num
        | operation == 3 = show (solution + num) ++ " - " ++ show num
        | operation == 4 = show num ++ " * " ++ qu
        where qu' = fromIntegral solution / fromIntegral num
              qu = if qu' == fromIntegral (round qu')
                   then show $ round qu'
                   else show qu'
  putStrLn $ problem ++ " = " ++ show solution
  return (problem, solution)

genButton :: HBox -> Maybe Int -> IO ()
genButton answerBox Nothing = do
  num <- randomRIO (1, 10) :: IO Int
  genButton answerBox (Just num)

genButton answerBox (Just num) = do
  button <- buttonNewWithLabel $ show num
  boxPackStart answerBox button PackGrow 0
  return ()

setButtonAction :: Int -> Button -> Label -> HBox -> Window -> IO ()
setButtonAction solution button problemLabel answerBox window = do
  num <- get button buttonLabel
  when (read num == solution) $ do
    onClicked button $ newGame problemLabel answerBox window
    return ()
  return ()

