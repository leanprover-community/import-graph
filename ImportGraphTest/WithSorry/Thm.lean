/-- warning: declaration uses `sorry` -/
#guard_msgs in
theorem bad_contains_sorry : True := by
  have x : True := sorry
  exact True.intro
