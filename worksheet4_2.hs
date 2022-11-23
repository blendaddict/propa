module Ast where

    data Exp t = Constant Integer | Variable t | Sum (Exp t) (Exp t)
