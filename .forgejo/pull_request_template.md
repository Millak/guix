---
name: 'Pull Request'
about: 'Pull request template'
title: ''
ref: ''
labels:
---
<!--
Below is a checklist for package-related patches.  For details please refer to
[the manual](https://guix.gnu.org/manual/devel/en/html_node/Submitting-Patches.html)

Common issues in the contribution process are also documented in the
["Contributing" chapter](https://guix.gnu.org/manual/devel/en/html_node/Contributing.html)
of the manual.

Tick a box by changing it from [ ] to [x].
-->

- Built successfully on the following system(s):
  - [ ] x86_64-linux
  - [ ] i686-linux
  - [ ] aarch64-linux
  - [ ] armhf-linux
  - [ ] powerpc64le-linux
  - [ ] riscv64-linux
  - [ ] x86_64-gnu
  - [ ] i586-gnu

- Successfully cross-compiled to the following target(s) (this is optional):
  - [ ] x86_64-linux-gnu
  - [ ] i686-linux-gnu
  - [ ] aarch64-linux-gnu
  - [ ] arm-linux-gnueabihf
  - [ ] powerpc64le-linux-gnu
  - [ ] riscv64-linux-gnu
  - [ ] x86_64-pc-gnu
  - [ ] i586-pc-gnu

- Link to upstream release notes (if applicable):

- Build status of direct dependents (`./pre-inst-env guix build --keep-going --verbosity=1 --dependents=1 PACKAGE ...`)
```text

```

- [ ] Fixed errors reported by `guix lint` (`./pre-inst-env guix lint PACKAGE ...`).

- [ ] Verified cryptographic signature provided by upstream.

- [ ] The packages don't use bundled copies of software.

- [ ] Synopsis and description are written confirming to [the guidelines](https://guix.gnu.org/manual/devel/en/html_node/Synopses-and-Descriptions.html).

- [ ] Commit messages follow [the "ChangeLog" style](https://www.gnu.org/prep/standards/html_node/Change-Logs.html).

- [ ] The change doesn't break `guix pull` (`guix pull --url=/path/to/your/checkout --profile=/tmp/guix.master`).
