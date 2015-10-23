
import Text.Pandoc.PlantUML.Filter
import Text.Pandoc.PlantUML.Filter.IORender
import Text.Pandoc.JSON

processBlocksInRealWorld :: Maybe Format -> Block -> IO Block
processBlocksInRealWorld = processBlocks

main :: IO ()
main = toJSONFilter processBlocksInRealWorld

