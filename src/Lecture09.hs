{-# LANGUAGE DuplicateRecordFields #-}

module Lecture09 where

import System.Directory
import System.IO
import System.Random
import System.FilePath.Posix
import Data.List
import Test.RandomStrings

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show, Read)

newtype Id = Id String deriving (Eq, Show, Read)

newtype Title = Title String deriving (Eq, Show, Read)

newtype Deadline = Deadline String deriving (Eq, Show, Read)

newtype Content = Content String deriving (Eq, Show, Read)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show, Read)

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show, Read)

createTodoList :: FilePath -> IO TodoList
createTodoList rootDirectory = do 
                          createDirectory rootDirectory
                          return $ TodoList rootDirectory

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title text deadline = do
                              fileName <- randomWord (onlyAlphaNum randomASCII) 10
                              saveTodo todoList $ Todo (Id fileName) title text deadline False

saveTodo :: TodoList -> Todo -> IO Id
saveTodo (TodoList rootDirectory) todo@(Todo {todoId=(Id fileName)}) = do
                             file <- openFile (joinPath [rootDirectory, fileName]) WriteMode
                             hPutStrLn file $ show $ todo 
                             hClose file
                             return $ Id fileName

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList rootDirectory) (Id filename) = do
                             todoId <- readFile $ joinPath [rootDirectory, filename]
                             return $ read todoId

showTodo :: TodoList -> Id -> IO ()
showTodo (TodoList rootDirectory) (Id filename) = do 
                             todoId <- readFile $ joinPath [rootDirectory, filename]
                             putStrLn todoId

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList rootDirectory) (Id filename) = removeFile $ joinPath [rootDirectory, filename]

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do 
                             (Todo todoId _ _ _ isDone) <- readTodo todoList id
                             _ <- saveTodo todoList $ Todo todoId title content deadline isDone
                             return ()

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do 
                             (Todo todoId title content deadline _) <- readTodo todoList id
                             _ <- saveTodo todoList $ Todo todoId title content deadline True
                             return ()


instance Ord Deadline where
  compare (Deadline s1) (Deadline s2) = compare s1 s2

instance Ord Todo where
  compare Todo{deadline=d1} Todo{deadline=d2} = compare d1 d2
   

-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList rootDirectory) = do
                             files <- listDirectory rootDirectory
                             let 
                                 filenames = map (\fp -> Id fp) files
                             todos <- mapM (readTodo todoList) filenames 
                             return $ sort todos 

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do 
                             todos <- readAllTodo todoList
                             return $ filter (\(Todo _ _ _ _ isDone) -> not isDone) todos

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do 
                             todos <- readAllTodo todoList
                             mapM_ (\td -> putStrLn $ show td) todos

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do 
                             todos <-readAllTodo todoList
                             mapM_ (\td -> putStrLn $ show td) $ filter (\(Todo _ _ _ _ isDone) -> isDone) todos

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

playGuessGame :: IO ()
playGuessGame = do
              secret <- randomRIO (0, 100) :: IO Int
              turn secret "Go!"

getGuess :: IO Int
getGuess = do
  guess <- getLine
  return $ read guess

nextTurn :: Int -> Int -> IO ()
nextTurn secret guess
  | guess > secret = turn secret "Too big"
  | guess < secret = turn secret "Too small"
  | otherwise = putStrLn "Yep, that's the number!"

turn :: Int -> String -> IO ()
turn secret message = do
  putStrLn message
  guess <- getGuess
  nextTurn secret guess              


-- </Задачи для самостоятельного решения>