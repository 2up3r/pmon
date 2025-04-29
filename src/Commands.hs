{-# LANGUAGE OverloadedStrings #-}
module Commands 
    ( Command (..)
    , pCommand
    , pCommands
    ) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isSpace, isDigit)
import Data.Functor (($>))
import Data.Void (Void)

import qualified Data.Text as T
import qualified Text.Megaparsec as M

import ProcessInformation (ProcessOrder (..))
import Types (MicroSecond, PID, OrderDirection (..))

data Command = CommandQuit
             | CommandReset
             | CommandOrderType ProcessOrder
             | CommandOrderDirection OrderDirection
             | CommandPin PID
             | CommandDelay MicroSecond
             | CommandShow PID
    deriving (Eq)

type Parser = M.Parsec Void T.Text

pSpaces :: Parser ()
pSpaces = M.many (M.satisfy isSpace) $> ()

pSpaces1 :: Parser ()
pSpaces1 = M.some (M.satisfy isSpace) $> ()

pCommandOrderType :: Parser Command
pCommandOrderType = ("pid" $> CommandOrderType OrderPID)
                <|> ("cpu" $> CommandOrderType OrderCPU)
                <|> ("mem" $> CommandOrderType OrderMemory)
                <|> ("time" $> CommandOrderType OrderTime)
                <|> ("comm" $> CommandOrderType OrderCommand)

pCommandOrderDir :: Parser Command
pCommandOrderDir = ("asc" $> CommandOrderDirection OrderAsc)
               <|> ("dec" $> CommandOrderDirection OrderDec)

pCommandQuit :: Parser Command
pCommandQuit = "q" $> CommandQuit

pCommandReset :: Parser Command
pCommandReset = "r" $> CommandReset

pCommandPin :: Parser Command
pCommandPin = ("p" $> (CommandPin . read)) <*> M.some (M.satisfy isDigit)

pCommandDelay :: Parser Command
pCommandDelay = ("d" $> (CommandDelay . read)) <*> M.some (M.satisfy isDigit)

pCommandShow :: Parser Command
pCommandShow = ("s" $> (CommandShow . read)) <*> M.some (M.satisfy isDigit)

pCommand :: Parser Command
pCommand = M.choice
    [ pCommandOrderType
    , pCommandOrderDir
    , pCommandQuit
    , pCommandReset
    , pCommandPin
    , pCommandDelay
    , pCommandShow
    ]

pCommands :: Parser [Command]
pCommands = pSpaces *> M.sepEndBy pCommand pSpaces1 <* M.eof
