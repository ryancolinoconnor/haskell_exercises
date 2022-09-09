import Parse
import PNM
import Data.Word (Word8)
--- This problem set was strange as the 
-- parsers provided didn't work for me for plain pgm files
-- and just returned nothing, haven't used 
-- pgm so moving forward
-- 1.1 
-- Write a parser for "plain" pGM files
-- One image in a file
-- p2 instead of p5
-- each pixel is ascii decimal, white space before and after it, no maxium, no line longer than 70ch
parsePlainPGM = 
        parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
        assert (header == "P2") "invalid plain header" ==>&
        parseNat ==> \width -> skipSpaces ==>&
        parseNat ==> \height -> skipSpaces ==>&
        parseNat ==> \maxGrey ->
        parseByte ==>&
        parseBytes (width * height) ==> \bitmap ->
        identity (Greymap width height maxGrey bitmap)
    where notWhite = (`notElem` "\r\n\t")

-- 1.2
-- Max gray is less than256 each pixel is a single byte
-- but it can range up to 65,636 which means each pixel can be represented by two bytes
parseByteOrTwo :: Int -> Parse Word8
parseByteOrTwo maxGrey  = if maxGrey>255 then
                            parseByte ==>& parseByte
                        else
                            parseByte
parseRawPGMBytes = 
        parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
        assert (header == "P5") "invalid raw header" ==>&
        parseNat ==> \width -> skipSpaces ==>&
        parseNat ==> \height -> skipSpaces ==>&
        parseNat ==> \maxGrey ->
        (parseByteOrTwo maxGrey) ==>&
        parseBytes (width * height) ==> \bitmap ->
        identity (Greymap width height maxGrey bitmap)
    where notWhite = (`notElem` "\r\n\t")


-- 1.3 
parsePlainOrRawPGMBytes = 
        parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
        case header of
          "P5" -> parseRawPGMBytes
          "P2" -> parsePlainPGM
          otherwise -> assert "invalid header"
