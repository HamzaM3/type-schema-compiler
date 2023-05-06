module ParsingLang where

import BaseParsers (parseAlpha, parseChar, parseId, parseRest, parseSpanChar)
import Control.Applicative (Alternative ((<|>)))
import Data.Char (isAlpha)
import Data.List (concat, span, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..), singleton, toList)
import Data.Tuple (swap)
import Parser (Parser (Parser), (*\\>), (<//*), (<//*>))
import ParsingUtils (mapSnd, nothingIf)
import Text.Read (Lexeme (String))

---

data Type = NumberType | StringType | BoolType | ObjectType (NonEmpty TypeField) | ArrayType Type
  deriving (Show, Eq)

data TypeField = TypeField String Type
  deriving (Show, Eq)

data AJVSchema = NumberSchema | StringSchema | BoolSchema

---

parseFieldName :: Parser String
parseFieldName = Parser $ nothingIf ((==) "" . snd) . swap . span isAlpha

parseString :: String -> Parser String
parseString s = Parser $ ((,s) <$>) . stripPrefix s

parseIntType :: Parser Type
parseIntType = NumberType <$ parseString "number"

parseStringType :: Parser Type
parseStringType = StringType <$ parseString "string"

parseBoolType :: Parser Type
parseBoolType = BoolType <$ parseString "bool"

parseField :: Parser TypeField
parseField = TypeField <$> parseFieldName <//* parseChar ':' <//*> parseType

parseArrayType :: Parser Type
parseArrayType = ArrayType <$> (parseChar '[' *\\> parseType <//* parseChar ']')

parseObjectType :: Parser Type
parseObjectType = ObjectType <$> (parseChar '{' *\\> ((:|) <$> parseField <//*> parseObjectType') <//* parseChar '}')
  where
    parseObjectType' :: Parser [TypeField]
    parseObjectType' = ((:) <$> (parseChar ',' *\\> parseField) <//*> parseObjectType') <|> pure []

parseType :: Parser Type
parseType = parseBoolType <|> parseStringType <|> parseIntType <|> parseArrayType <|> parseObjectType

parseSchemas :: Parser (NonEmpty (String, Type))
parseSchemas = (:|) <$> parseSchema <//*> parseSchemas'
  where
    parseSchemas' :: Parser [(String, Type)]
    parseSchemas' = ((:) <$> (parseSpanChar '-' *\\> parseSchema) <//*> parseSchemas') <|> pure []
    parseSchemaName :: Parser String
    parseSchemaName = parseString "schema " *\\> parseAlpha
    parseSchema :: Parser (String, Type)
    parseSchema = (,) <$> parseSchemaName <//*> parseType

---

reservedKeywords :: [String]
reservedKeywords =
  [ "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "debugger",
    "default",
    "delete",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "finally",
    "for",
    "function",
    "if",
    "import",
    "in",
    "instanceof",
    "new",
    "null",
    "return",
    "super",
    "switch",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "var",
    "void",
    "while",
    "with",
    "yield"
  ]

template :: (String, String) -> String
template (s, s') = "export const " ++ s ++ " = " ++ s' ++ ";\n"

schemastoAJV :: NonEmpty (String, Type) -> String
schemastoAJV = concatMap typeToAJVSchema

typeToAJVSchema :: (String, Type) -> String
typeToAJVSchema = template . mapSnd typeToAJVSchema'

typeToAJVSchema' :: Type -> String
typeToAJVSchema' NumberType = "{type: \"number\"}"
typeToAJVSchema' BoolType = "{type: \"boolean\"}"
typeToAJVSchema' StringType = "{type: \"string\"}"
typeToAJVSchema' (ObjectType fields) = "{type: \"object\",properties:{" ++ properties ++ "}}"
  where
    getProperty :: TypeField -> String
    getProperty (TypeField name type_) = name ++ ":" ++ typeToAJVSchema' type_ ++ "," -- test correct identifier name
    properties :: String
    properties = concatMap getProperty fields
typeToAJVSchema' (ArrayType type_) = "{type: \"array\", items:" ++ typeToAJVSchema' type_ ++ "}"
  where
    getType :: Type -> String
    getType = typeToAJVSchema'