import Om.Eval.StrictTests
import Om.Lang.ParserTests
import Om.Plug.NatsTests
import Om.Plug.RecordsTests
import Om.RunTests
import Test.Hspec

main :: IO ()
main = hspec $ do
    evalTests
    evalRecordsTests
    evalNatsTests
    parserTests
    runExprTests
