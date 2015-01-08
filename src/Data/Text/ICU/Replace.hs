{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-|

This implements a common DSL for regular expression replacement text. This
is represented with the 'Replace' data type. It also implements the
'IsString' interface, so if 'OverloadedStrings' is on, you can use a raw
string to build the replacement.

-}


module Data.Text.ICU.Replace
    (
    -- * @OverloadedStrings@ Syntax
    -- $doc

    -- * Types
      Replace

    -- * High-level interface
    , replace
    , replace'
    , replaceAll
    , replaceAll'

    -- * Low-level interface
    , rgroup
    , rtext
    , rstring
    , rfn
    , rtfn
    , rbuilder
    ) where


import           Control.Applicative
import           Control.Arrow
import           Data.Attoparsec.Text
import           Data.Foldable
import           Data.Monoid
import           Data.String
import qualified Data.Text              as T
import           Data.Text.ICU
import qualified Data.Text.ICU          as ICU
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB


-- | A 'Replace' instance is a function from a regular expression match to
-- a 'Data.Text.Lazy.Builder.Builder'. This naturally forms a 'Monoid', so
-- they're easy to combine.
--
-- 'Replace' also implements 'IsString', so raw strings can be used to
-- construct them.
newtype Replace = Replace { unReplace :: Match -> TB.Builder } deriving
                  (Monoid)

instance IsString Replace where
    fromString = parseReplace . T.pack

-- | Execute a regular expression on a 'Data.Text.Text' and replace the
-- first match.
replace :: Regex        -- ^ The regular expression to match.
        -> Replace      -- ^ The specification to replace it with.
        -> T.Text       -- ^ The text to operate on.
        -> T.Text       -- ^ The text with the first match replaced.
replace re r t = maybe t (replace' r) $ ICU.find re t

-- | Replace one regular expression match with the 'Replace'.
replace' :: Replace     -- ^ The specification to replace it with.
         -> Match       -- ^ The match to replace.
         -> T.Text      -- ^ The text with the match replaced.
replace' r m = finish (Last (Just m)) $ unReplace r m

-- | Execute a regular expression on a 'Data.Text.Text' and replace all
-- matches.
replaceAll :: Regex     -- ^ The regular expression to match.
           -> Replace   -- ^ The specification to replace it with.
           -> T.Text    -- ^ The text to operate on.
           -> T.Text    -- ^ The text with all matches replaced.
replaceAll re r t = case ICU.findAll re t of
                        [] -> t
                        ms -> replaceAll' r ms

-- | Replace all regular expression matches with the 'Replace'.
replaceAll' :: Replace  -- ^ The specification to replace it with.
            -> [Match]  -- ^ The matches to replace.
            -> T.Text   -- ^ The text with all matches replaced.
replaceAll' r ms = uncurry finish $ foldMap (Last . Just &&& build r) ms
    where
        build :: Replace -> Match -> TB.Builder
        build repl m = TB.fromText (ICU.span m) <> unReplace repl m

-- This handles the last match by including not only the match's prefix and
-- the replacement text, but also the suffix trailing the match.
finish :: Last Match -> TB.Builder -> T.Text
finish m b =
      TL.toStrict . TB.toLazyText . mappend b . TB.fromText . fold
    $ suffix 0 =<< getLast m

-- | Create a 'Replace' that inserts a regular expression group.
rgroup :: Int       -- ^ The number of the group in a regular expression.
       -> Replace   -- ^ The 'Replace' that inserts a group's match.
rgroup g = Replace $ fold . fmap TB.fromText . group g

-- | Create a 'Replace' that inserts static 'Data.Text.Text'.
rtext :: T.Text     -- ^ The static 'Data.Text.Text' to insert.
      -> Replace    -- ^ The 'Replace' that inserts the static 'Data.Text.Text'.
rtext = rbuilder . TB.fromText

-- | Create a 'Replace' that inserts a static 'String'.
rstring :: String   -- ^ The static 'String' to insert.
        -> Replace  -- ^ The 'Replace' that inserts the static 'String'.
rstring = rbuilder . TB.fromString

-- | Create a 'Replace' from a function that transforms a 'Match' into
-- a 'Data.Text.Lazy.Builder.Builder'.
rfn :: (Match -> TB.Builder)    -- ^ The function that creates the replacement text.
    -> Replace                  -- ^ The 'Replace' based off that function.
rfn = Replace

-- | Create a 'Replace' From a function that transforms a 'Match' into
-- a 'Data.Text.Text'.
rtfn :: (Match -> T.Text)       -- ^ The function that creates the replacement text.
     -> Replace                 -- ^ The 'Replace' based off that function.
rtfn = Replace . (TB.fromText .)

-- | Create a 'Replace' that inserts a static 'Data.Text.Lazy.Builder.Builder'.
rbuilder :: TB.Builder  -- ^ The 'Data.Text.Lazy.Builder.Builder' to insert.
         -> Replace     -- ^ The 'Replace' that inserts the static
                        -- 'Data.Text.Lazy.Builder.Builder'.
rbuilder = Replace . const

-- | This parses a 'Data.Text.Text' into a 'Replace' structure.
--
-- Generally, input text is considered to be static.
--
-- However, groups from the regular expression's matches can be insert
-- using @$1@ (to insert the first group) or @${7}@ (to insert the seventh
-- group).
--
-- Dollar signs can be included in the output by doubling them (@$$@).
parseReplace :: T.Text -> Replace
parseReplace t = either (const $ rtext t) id
               $ parseOnly (replacement <* endOfInput) t

-- A replacement.
replacement :: Parser Replace
replacement = mconcat <$> many (dollarGroup <|> raw)

-- A @\$\d+@ or @\$\{\d+\}@ group. This could also be an escaped dollar
-- sign (@$$@).
dollarGroup :: Parser Replace
dollarGroup = char '$' *> (grp <|> escaped)
    where curly   = char '{' *> decimal <* char '}'
          grp     = rgroup <$> (decimal <|> curly)
          escaped = rtext . T.singleton <$> char '$'

-- A raw input string. It must contain no dollar signs (@$@).
raw :: Parser Replace
raw = rtext <$> takeWhile1 (/= '$')


-- $doc
--
-- The syntax used with 'OverloadedStrings' is meant to be similar to that
-- used in other regular expression libraries in other programming
-- languages.
--
-- Generally, input text is considered to be static.
--
-- >>> replaceAll "a" "b" "aaa"
-- "bbb"
-- >>> replaceAll "ab" "ba" "cdababcd"
-- "cdbabacd"
--
-- However, groups from the regular expression's matches can be insert
-- using @$1@ (to insert the first group) or @${7}@ (to insert the seventh
-- group).
--
-- >>> replaceAll "(.*), (.*)" "$2 $1" "Beeblebrox, Zaphod"
-- "Zaphod Beeblebrox"
-- >>> replaceAll "4(\\d)" "${1}4" "7458"
-- "7548"
--
-- Dollar signs can be included in the output by doubling them (@$$@).
--
-- >>> replaceAll "(\\d+\\.\\d+)" "$$$1" "9.99"
-- "$9.99"
