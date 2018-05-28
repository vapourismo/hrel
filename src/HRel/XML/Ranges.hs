{-# LANGUAGE LambdaCase #-}

module HRel.XML.Ranges
    ( isNameStartChar
    , isNameChar
    , isSpace
    , isDigit
    , isHexDigit )
where

import Data.Char (chr)

-- | Is the character allowed for the start of a name?
isNameStartChar :: Char -> Bool
isNameStartChar input =
    input == ':'
    || input == '_'
    || 'A' <= input && input <= 'Z'
    || 'a' <= input && input <= 'z'
    || chr 0xC0 <= input && input <= chr 0xD6
    || chr 0xD8 <= input && input <= chr 0xF6
    || chr 0xF8 <= input && input <= chr 0x2FF
    || chr 0x370 <= input && input <= chr 0x37D
    || chr 0x37F <= input && input <= chr 0x1FFF
    || chr 0x200C <= input && input <= chr 0x200D
    || chr 0x2070 <= input && input <= chr 0x218F
    || chr 0x2C00 <= input && input <= chr 0x2FEF
    || chr 0x3001 <= input && input <= chr 0xD7FF
    || chr 0xF900 <= input && input <= chr 0xFDCF
    || chr 0xFDF0 <= input && input <= chr 0xFFFD
    || chr 0x10000 <= input && input <= chr 0xEFFFF

-- | Is the character allowed in the body of a name?
isNameChar :: Char -> Bool
isNameChar input =
    isNameStartChar input
    || input == '-'
    || input == '.'
    || '0' <= input && input <= '9'
    || input == chr 0xB7
    || chr 0x0300 <= input && input <= chr 0x036F
    || chr 0x203F <= input && input <= chr 0x2040

-- | Is this a space character?
isSpace :: Char -> Bool
isSpace input =
    input == chr 0x20
    || input == chr 0x9
    || input == chr 0xD
    || input == chr 0xA

-- | Is the character a digit?
isDigit :: Char -> Bool
isDigit input =
    '0' <= input && input <= '9'

-- | Is the character a hexadecimal digit?
isHexDigit :: Char -> Bool
isHexDigit input =
    isDigit input
    || 'a' <= input && input <= 'f'
    || 'A' <= input && input <= 'F'
