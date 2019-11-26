pushd ~%dp0
git worktree add publish gh-pages
git checkout master && npm run build && (robocopy deploy publish || echo OK) && pushd publish && git add . && git commit -m "New version" && git push && popd

