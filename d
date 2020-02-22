[1mdiff --git a/Prettyprinting.hs b/Prettyprinting.hs[m
[1mindex efd8a54..432c027 100644[m
[1m--- a/Prettyprinting.hs[m
[1m+++ b/Prettyprinting.hs[m
[36m@@ -12,12 +12,12 @@[m [minstance Pretty Term where[m
   pretty (Var v) = v[m
   pretty (Comb c []) = c[m
   pretty (Comb "." [Comb c1 xs, Comb "[]" []]) = "[" ++ (pretty (Comb c1 xs)) ++ "]"[m
[31m-  pretty (Comb "." [Comb c1 xs, Comb "." ys]) = "[" ++ (intercalate ", " (c1 : toList (Comb "." ys))) ++ "]" where [m
[31m-   toList :: Term -> [String][m
[31m-   toList (Comb "." [Comb c1 xs, Comb "[]" []]) = [pretty (Comb c1 xs)][m
[31m-   toList (Comb "." [Comb _ xs, Comb "." _]) = (fmap pretty xs)[m
[31m-   toList (Comb "." [x, y]) = [(pretty x) ++ "|" ++ (pretty y)][m
[31m-   toList x = [pretty x][m
[32m+[m[32m  pretty (Comb "." [Comb c1 xs, Comb "." ys]) = "[" ++ (intercalate ", " (c1 : toList (Comb "." ys))) ++ "]" where[m
[32m+[m[32m    toList :: Term -> [String][m
[32m+[m[32m    toList (Comb "." [Comb c1 xs, Comb "[]" []]) = [pretty (Comb c1 xs)][m
[32m+[m[32m    toList (Comb "." [Comb _ xs, Comb "." _]) = (fmap pretty xs)[m
[32m+[m[32m    toList (Comb "." [x, y]) = [(pretty x) ++ "|" ++ (pretty y)][m
[32m+[m[32m    toList x = [pretty x][m
   pretty (Comb "." [x, y]) = "[" ++ (pretty x) ++ "|" ++ (pretty y) ++ "]"[m
[31m-  pretty (Comb c xs) = c ++ "(" ++ (intercalate ", " (fmap pretty xs)) ++ ")" [m
[31m-  [m
[32m+[m[32m  pretty (Comb c xs) = c ++ "(" ++ (intercalate ", " (fmap pretty xs)) ++ ")"[m
[32m+[m
