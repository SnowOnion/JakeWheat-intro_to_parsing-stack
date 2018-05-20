import Evaluation(Expr(..), eval)

main :: IO ()
main = mapM_ print $ map eval [e1,e2,e3,e4]

e1 = Add (Lit 1) (Lit 1)
e2 = Div (Lit 1) (Lit 0)
e3 = Sqrt $ Lit (-1)
e4 = Div
  (Sqrt
    (Mul
      (Add
        (Lit 1)
        (Lit 2))
      (Sub
        (Lit 3)
        (Lit 4))))
  (Lit 0)
