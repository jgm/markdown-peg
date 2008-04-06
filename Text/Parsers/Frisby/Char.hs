-- | Unicode character parsers. The character classification is identical to the
-- classification in the "Data.Char" module.
module Text.Parsers.Frisby.Char where

import Data.Char
import Text.Parsers.Frisby (P, anyChar, onlyIf)

-- | Match a control character.
control :: P s Char
control = anyChar `onlyIf` isControl

-- | Match a white-space character in the Latin-1 range.
space :: P s Char
space = anyChar `onlyIf` isSpace

-- | Match a lower-case alphabetic Unicode character.
lower :: P s Char
lower = anyChar `onlyIf` isLower

-- | Match an upper-case or title-case alphabetic Unicode character.
upper :: P s Char
upper = anyChar `onlyIf` isUpper

-- | Match an alphabetic Unicode character. Equivalent to 'letter'.
alpha :: P s Char
alpha = anyChar `onlyIf` isAlpha

-- | Match an alphabetic or numeric digit Unicode character.
alphaNum :: P s Char
alphaNum = anyChar `onlyIf` isAlphaNum

-- | Match a printable Unicode character.
printable :: P s Char
printable = anyChar `onlyIf` isPrint

-- | Match an ASCII digit.
digit :: P s Char
digit = anyChar `onlyIf` isDigit

-- | Match an ASCII octal digit.
octDigit :: P s Char
octDigit = anyChar `onlyIf` isOctDigit

-- | Match an ASCII hexadecimal digit.
hexDigit :: P s Char
hexDigit = anyChar `onlyIf` isHexDigit

-- | Match an alphabetic Unicode character. Equivalent to 'alpha'.
letter :: P s Char
letter = anyChar `onlyIf` isLetter

-- | Match a Unicode mark character.
mark :: P s Char
mark = anyChar `onlyIf` isMark

-- | Match a Unicode numeric character.
number :: P s Char
number = anyChar `onlyIf` isNumber

-- | Match a Unicode punctuation character.
punctuation :: P s Char
punctuation = anyChar `onlyIf` isPunctuation

-- | Match a Unicode symbol character.
symbol :: P s Char
symbol = anyChar `onlyIf` isSymbol

-- | Match a Unicode space or separator character.
separator :: P s Char
separator = anyChar `onlyIf` isSeparator

-- | Match a character of the ASCII character set.
ascii :: P s Char
ascii = anyChar `onlyIf` isAscii

-- | Match a character of the ISO 8859-1 (Latin-1) character set.
latin1 :: P s Char
latin1 = anyChar `onlyIf` isLatin1

-- | Match an ASCII upper-case letter.
asciiUpper :: P s Char
asciiUpper = anyChar `onlyIf` isAsciiUpper

-- | Match an ASCII lower-case letter.
asciiLower :: P s Char
asciiLower = anyChar `onlyIf` isAsciiLower
