import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- import           Data.Attoparsec.Text

-- import           HRel.Parser2
import           HRel.Parser
import           HRel.Parser.XML

main :: IO ()
main = do
	contents <- T.readFile "/data/downloads/tpb-head.xml"
	-- either putStrLn (mapM_ print) (parseOnly xml contents)
	-- either (const (putStrLn "Oops")) (mapM_ print) (runParser xml contents)
	case runParser xml contents of
		Left _        -> putStrLn "Left"
		Right (xs, _) -> mapM_ print xs
