# vvdoctor 0.0.2

# Version 0.0.2 (2025-08-07)

## Major Improvements
- Improved error handling and user notifications throughout the Shiny app using `shiny::showNotification()`.
- Added robust input validation for missing data, minimum sample size, and variable type checks.
- Implemented or provided user feedback for all major statistical tests, including Wilcoxon, t-test, ANOVA, Kruskal-Wallis, Chi-Square, McNemar, Bhapkar, and Fisher's Exact.
- Improved user feedback for unsupported or unimplemented statistical tests.
- Ensured compatibility with latest versions of dependencies (`DescTools`, `rstatix`, `exact2x2`, `irr`, `DT`, `shiny`, `datamods`).

## Minor Changes
- Enhanced documentation and code comments for clarity.
- Updated input validation messages for better user guidance.
- Minor UI/UX improvements based on user feedback.

## Bug Fixes
- Fixed: Wilcoxon Signed-Rank Test II now provides user notification if input is invalid.

---

For a full list of changes, see the commit history and feedback documents.
# vvdoctor 0.0.1

* Initial CRAN submission.
