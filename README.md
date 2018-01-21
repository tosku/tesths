# tesths
sample Spec.hs

```
import qualified TestHS as T
import Test.Something as S

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTests $ S.fastTests 
  T.reportTestsIO $ S.ioTests 
```

`S.fastTests` is a list of tests `[Test]` and
`S.ioTests` is a list of IO tests `[IO Test]` defined in `Test.Something`
