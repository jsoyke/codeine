module Syntax (
-- types:
  Type,
  Token,

-- functions:
  tokenize) where

data Type =
  Normal |
  KeyWord |
  Comment |
  String |
  Type |
  Function |
  Operator

data Token = Token Type String

class Tokenizer t where
  tokenize :: t -> [String] -> [[Token]]

data NormalTokenizer = NormalTokenizer

instance Tokenizer NormalTokenizer where
  tokenize _ lines =
    map (\l -> [Token Normal l]) lines
