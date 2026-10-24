#!/bin/bash
# Sets gitolite up from what the test mounts at /setup, then runs sshd:
#   /setup/admin.pub       the admin's key (the grader)
#   /setup/keys/*.pub      the other users' keys (file name = user name)
#   /setup/gitolite.conf   the access rules
#   /setup/post-receive    (optional) a hook to try instead of the server's
#   /seed/repositories     (optional) repos to start with
set -euo pipefail

# (optional) a copy of real repos to start from: /seed/repositories/*.git,
# with history entries made from their HEADs, as the push hook would have
if [ -d /seed/repositories ]; then
  mkdir -p /home/git/repositories /home/git/history
  cp -a /seed/repositories/. /home/git/repositories/
  for r in /home/git/repositories/*.git; do
    if sha=$(git -C "$r" rev-parse -q --verify HEAD); then
      echo "$sha" > "/home/git/history/$(basename "$r" .git)"
    fi
  done
  chown -R git:git /home/git
fi

if [ -f /setup/post-receive ]; then
  cp /setup/post-receive /home/git/local/hooks/common/post-receive
  chmod +x /home/git/local/hooks/common/post-receive
fi

su git -c "gitolite setup -pk /setup/admin.pub" > /dev/null

# what the real server's push hook needs: ~/history, and an identity for the
# commits `git notes` makes
su git -c "mkdir -p ~/history && git config --global user.name gitolite && git config --global user.email git@localhost"

# like the real server: local commands (history), and fork (perms is on by
# default)
perl -pi \
  -e 's{^(\s*)#\s*(LOCAL_CODE\s*=>\s*"\$ENV\{HOME\}/local",)}{$1$2};' \
  -e "s{^(\\s*)#\\s*('fork',)}{\$1\$2};" \
  -e "s{^(\\s*)('info',)}{\$1\$2 'history',};" \
  /home/git/.gitolite.rc

cp /setup/keys/*.pub /home/git/.gitolite/keydir/ 2>/dev/null || true
cp /setup/gitolite.conf /home/git/.gitolite/conf/gitolite.conf
chown -R git:git /home/git
su git -c "gitolite compile && gitolite trigger POST_COMPILE"

exec /usr/sbin/sshd -D -e
