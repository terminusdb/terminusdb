#!/usr/bin/env bash

die() {
  echo >&2 -e "${1-}"
  exit 1
}

parse_param() {
  local release="${1-}"
  case "$release" in
    stable | devel)
      [ "$RUNNER_OS" = "Linux" ] || die "Unsupported: '$release' on '$RUNNER_OS'"
      ;;
    homebrew)
      [ "$RUNNER_OS" = "macOS" ] || die "Unsupported: '$release' on '$RUNNER_OS'"
      ;;
    *)
      die "Unexpected argument: '$release'"
      ;;
  esac
}

do_install() {
  local release="${1-}"
  case "$release" in
    stable | devel)
      # Install swipl from PPA: https://www.swi-prolog.org/build/PPA.html
      sudo apt-add-repository "ppa:swi-prolog/$release"
      sudo apt-get update
      sudo apt-get install swi-prolog
      [ "$(which swipl)" = "/usr/bin/swipl" ] || die "swipl command not found"
      ;;
    homebrew)
      # Install swipl from Homebrew: https://www.swi-prolog.org/build/macos.html
      brew install swi-prolog
      [ "$(which swipl)" = "/opt/homebrew/bin/swipl" ] || die "swipl command not found"
      ;;
    *)
      die "Unexpected release: '$release'"
      ;;
  esac
}

parse_param "${1-}"
do_install "${1-}"

echo -n "Installed: "
swipl --version
