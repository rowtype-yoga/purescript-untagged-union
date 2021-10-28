# purescript-untagged-to-tagged

Little helper library to make it easy to convert between untagged and tagged unions.

## Usage 

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

let
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