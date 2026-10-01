# FP-homepage sources

+ The page sources are in FP-group/
+ Edit them, commit and push to master. The GitHub Actions workflow
  (.github/workflows/pages.yml) then builds the site with Hakyll and
  publishes it at https://chalmersfp.github.io/
  (progress and errors are shown under the repository's "Actions" tab).
+ To preview locally, run
make
  and check the generated pages in docs/. (The first local build
  compiles Hakyll's dependencies and can take 15-20 minutes.)

Note: during the transition from the old setup, docs/ is still committed.
Once the workflow has deployed successfully (Settings -> Pages -> Source:
"GitHub Actions"), docs/ can be removed from the repository and added to
.gitignore.
