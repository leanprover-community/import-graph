

/-- `eraseDup' l` removes duplicates from `l` (taking only the last occurrence).

Note: imitates `Std`s `List.eraseDup` -/
@[inline] def List.eraseDup' [DecidableEq α] (l : List α) : List α := -- pwFilter (· ≠ ·)
  l.foldl (fun IH x => if x ∈ IH then IH else x :: IH) []
