# A test gitolite server

`Containerfile` builds a throwaway gitolite server for the integration tests
(`src/test/scala/ag/events/GitoliteServer.scala` runs it with podman).

`commands/` and `hooks/` are **byte-for-byte copies** of `local/` in the real
server's `gitolite-admin` repo, so the tests run the same code the server does.
`GitoliteCopiesTests` fails if they drift apart. It compares with the grader's
mirror of gitolite-admin (`workspace/targets/ag/grader/Gitolite/mirror/gitolite-admin/data/local`,
pulled whenever the grader runs), else `~/src/gitolite-admin/local`, and is
skipped without either. After changing the server's code, copy it here, e.g.:

    cp -r ~/src/gitolite-admin/local/commands ~/src/gitolite-admin/local/hooks src/test/gitolite/

Only `local/` is copied: `gitolite-admin`'s `keydir/` and `conf/` hold student
information and must stay out of this repository.
