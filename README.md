## Welcome to the Engelstädter Lab Website Repository

This website is built with [Quarto](https://quarto.org), an open source scientific and technical publishing system.

Note: `_utils/bib_to_qmd.r` was used to generate the publication pages from the BibTeX file. Don't re-run it on the existing site, because it overwrites hand edits (see `CLAUDE.md`).


## Notes to self

### Adding publications
Ask Claude Code (in this folder) to check Google Scholar for new papers and add them. The steps and conventions it follows are in `CLAUDE.md`.

### Adding new people
Copy `people/_template/` to `people/<lastname>_<firstname>/`, rename `_template.qmd` to `index.qmd`, and fill in the fields. Replace `Missing_avatar.jpg` with a photo named `avatar.jpg`.

### Deployment
Not set up yet. `quarto render` builds the site into `_site/`, ready to upload to wherever it will be hosted.
