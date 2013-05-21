
module RenameTypes where

import Control.Applicative ((<$>), (<*>))
import Text.Parsec

rename :: String -> String
rename t = either (error . show) id (parse renamer t t)
    where
      renamer :: Parsec String () String
      renamer = concat <$> many1 (choice [ toRename, chomp ])
      
      toRename = try (between (string "(") (string ")") super) <|> super

      mkSuper (old,new) = try (string old >> string " " >> lower >> return new)
      super = choice $ map mkSuper [("Appendable","appendable"),
                                    ("Number","number"),
                                    ("Comparable","comparable")]

      chomp = (:) <$> anyChar <*>
              manyTill anyChar (lookAhead toRename <|> (eof >> return ""))

