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
    *.*.*)
      [ "$RUNNER_OS" = "Linux" ] || die "Unsupported: '$release' on '$RUNNER_OS'"
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
      sudo apt-get install -y swi-prolog
      [ "$(which swipl)" = "/usr/bin/swipl" ] || die "swipl command not found"
      ;;
    homebrew)
      # Install swipl from Homebrew: https://www.swi-prolog.org/build/macos.html
      brew install swi-prolog
      # Some runner images carry a swi-prolog bottle whose crypto4pl links
      # openssl@4 while the formula only depends on openssl@3 — installing the
      # keg explicitly covers both bottle variants.
      brew install openssl@4
      [ "$(which swipl)" = "/opt/homebrew/bin/swipl" ] || die "swipl command not found"
      ;;
    *.*.*)
      # Extract the pinned build from the official swipl:<version> image —
      # it installs under /usr, so copy that tree out and point swipl at it.
      sudo apt-get update
      sudo apt-get install -y libtcmalloc-minimal4 libarchive13 libyaml-0-2 \
        libgmp10 libossp-uuid16 libssl3 libdb5.3t64 libpcre2-8-0 libedit2 \
        unixodbc libsqlite3-0 libserd-0-0 libraptor2-0
      docker pull "swipl:${release}"
      cid="$(docker create "swipl:${release}")"
      sudo docker cp "${cid}":/usr /opt/swipl/usr
      docker rm "${cid}"
      echo "/opt/swipl/usr/bin" >> "$GITHUB_PATH"
      {
        echo "SWI_HOME_DIR=/opt/swipl/usr/lib/swipl"
        echo "LD_LIBRARY_PATH=/opt/swipl/usr/lib/swipl/lib/x86_64-linux"
      } >> "$GITHUB_ENV"
      SWI_HOME_DIR=/opt/swipl/usr/lib/swipl \
        LD_LIBRARY_PATH=/opt/swipl/usr/lib/swipl/lib/x86_64-linux \
        "/opt/swipl/usr/bin/swipl" --version || die "swipl command not found"
      ;;
    *)
      die "Unexpected release: '$release'"
      ;;
  esac
}

parse_param "${1-}"
do_install "${1-}"

echo -n "Installed: "
if command -v swipl >/dev/null; then swipl --version; fi
