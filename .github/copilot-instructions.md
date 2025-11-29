# Copilot Instructions for heatmaply

This repository contains the `heatmaply` R package - an interactive cluster heatmap library built on top of `plotly` and `ggplot2`.

## Repository Overview

- **Type**: R Package
- **Purpose**: Create interactive cluster heatmaps that can be saved as standalone HTML files, embedded in R Markdown documents, or displayed in Shiny apps
- **Main dependencies**: plotly, ggplot2, dendextend, viridis

## Code Review Guidelines

When reviewing code changes in this repository, please consider:

### R Code Standards

- Follow R package development best practices (using `roxygen2` for documentation)
- Ensure all exported functions have proper documentation with `@param`, `@return`, `@examples` tags
- Use consistent naming conventions (snake_case for functions and variables)
- Check for proper use of `@importFrom` and `@export` tags
- Verify that examples are runnable and don't produce errors

### Package-Specific Considerations

- Changes to visualization functions should maintain backward compatibility with existing user code
- New parameters should have sensible defaults
- Consider performance implications when working with large matrices
- Ensure changes work with both ggplot2 and plotly backends

### Testing

- Tests are located in the `tests/` directory using the `testthat` framework
- New functionality should include appropriate test coverage
- Verify that existing tests pass after changes

### Documentation

- Update `NEWS.md` for user-facing changes
- Keep README.md examples up to date
- Vignettes in `vignettes/` should reflect current API

## Files to Pay Attention To

- `R/heatmaply.R` - Main heatmap functions
- `R/heatmapr.R` - Heatmap data preprocessing
- `R/ggheatmap.R` - ggplot2-based rendering
- `DESCRIPTION` - Package metadata and dependencies
- `NAMESPACE` - Exported functions
