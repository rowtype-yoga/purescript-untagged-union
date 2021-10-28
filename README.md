# purescript-untagged-to-tagged

Little helper library to make it easy to convert between untagged and tagged unions.

## Usage 

Convert an untagged union to a tagged union. E.g. 

```purescript
type ISU
  = Int |+| String

data IST
  = IT Int
  | ST String

-- generic instance is needed for the conversion
derive instance Generic IST _

instance Show IST where
  show = genericShow

isul :: ISU
isul = asOneOf 10

isur :: ISU
isur = asOneOf "Wurst"

istl :: IST
istl = toTagged isul 
-- IT 10

istr :: IST 
istr = toTagged isur 
-- ST "Wurst"
```

Convert a tagged union to an untagged union. E.g. 

```purescript
data IST = IT Int | ST String 
derive instance Generic IST _ 

type ISU = Int |+| String 

ist :: IST
ist = ST "Wurst"

isu :: ISU 
isu = fromTagged ist
-- asOneOf "Wurst"
```