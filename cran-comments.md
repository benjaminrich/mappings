# Version 0.2.0

## Test environments

* Local:
  - Windows 11 x64 (build 26100) (x86_64-w64-mingw32/x64), R version 4.5.1 (2025-06-13 ucrt)
* win-builder (Windows Server 2022 x64 (build 20348), x86_64-w64-mingw32):
  - R Under development (unstable) (2025-10-22 r88967 ucrt)
  - R version 4.5.1 (2025-06-13 ucrt)
* With GitHub actions:
  - ubuntu-latest (devel)
    - Ubuntu 24.04.3 LTS (x86_64, linux-gnu), R Under development (unstable) (2025-10-19 r88945)
  - ubuntu-latest (release)
    - Ubuntu 24.04.3 LTS (x86_64, linux-gnu), R version 4.5.1 (2025-06-13)
  - ubuntu-latest (oldrel-1)
    - Ubuntu 24.04.3 LTS (x86_64, linux-gnu), R version 4.4.3 (2025-02-28)
  - windows-latest
    - Windows Server 2022 x64 (build 26100) (x86_64, mingw32), R version 4.5.1 (2025-06-13 ucrt)
  - macos-latest
    - macOS Sequoia 15.6.1 (aarch64, darwin20), R version 4.5.1 (2025-06-13)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  - This seems to be a known issue (<https://stackoverflow.com/questions/63613301/>)

## Reverse dependencies

There are currently no downstream dependencies for this package.

