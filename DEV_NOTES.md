<!--

   Copyright 2016-2019 Daniel Urban and contributors listed in AUTHORS

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

--->

# Notes for development

Unorganized notes which may be useful during development.

## Release process

1. Start a fresh `sbt` session
1. `validate`
1. `release` (requires Sonatype credentials)
1. `sonatypeClose`
1. `sonatypePromote`

### Releasing a `SNAPSHOT` version

(This is really only useful for testing the release process itself.)

1. Start a fresh `sbt` session
1. `validate`
1. `release` (specify a `SNAPSHOT` version when asked, e.g., `0.1.0-SNAPSHOT`;
   requires Sonatype credentials)
1. Delete the automatically created Git tag, e.g., `git tag --delete v0.1.0-SNAPSHOT`

## Signing Git commits and tags

There are several tutorials available on signing
in Git (and using it on GitHub), for example
[this one](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work)
and [this one](https://help.github.com/categories/gpg/).

Here is a quick outline of the steps (if you're already
familiar with [GPG](https://www.gnupg.org) and Git,
this might be enough):

- the key you want to use must be in your GPG keyring
- note your 64-bit key ID with `gpg2 --list-keys --keyid-format long`
- configure Git to use the desired key for signing:
  `git config --global user.signingkey <your key ID>`
- you can tell Git to use GPG v2 with `git config --global gpg.program gpg2`
- make sure that signing commits is the default:
  `git config --global commit.gpgsign true` (you can omit the `--global`
  to only sign by default the commits of the current repository)
- make sure that signing annotated tags is the default:
  `git config --global tag.forcesignannotated true`

## GnuPG configuration

Useful config options for GPG v2 (put them into `~/.gnupg/gpg.conf`):

- `keyid-format long`: display the long (64-bit) key ID by default
- `with-fingerprint`: display the full key fingerprint by default
- `no-comments`: don't put comments into the `.asc` output
- `no-emit-version`: don't write the GPG version number into the `.asc` output

## SSH authentication with GPG

- export the authentication public key in SSH format with `gpg2 --export-ssh-key <key ID>`
- add it to `~/.ssh/authorized_keys` / GitHub / etc.
- enable `gpg-agent`: add `use-agent` to `~/.gnupg/gpg.conf`
- enable the SSH support of `gpg-agent`: add `enable-ssh-support` to `~/.gnupg/gpg-agent.conf`
- (reboot or at least restart X)
- make sure that `gpg-agent` is the running SSH agent:
    - the output of `echo $SSH_AUTH_SOCK` should contain the substring `gpg-agent`
    - if it doesn't, other possible agents may need to be disabled:
        - X11 default ssh-agent (disable with removing `use-ssh-agent` from `/etc/X11/Xsession.options`)
        - GNOME Keyring agent (?)
- the output of `ssh-add -l` should contain the auth key
  (if it's on a smartcard, only when the card is inserted)
    - `gpg-connect-agent updatestartuptty /bye`

## Using Eclipse

See [this workaround](https://gist.github.com/durban/621cf5becd38b3dcbf3d3a0d4464b46d).
