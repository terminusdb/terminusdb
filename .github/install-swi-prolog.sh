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
      [ "$RUNNER_OS" = "Linux" ] || [ "$RUNNER_OS" = "macOS" ] || die "Unsupported: '$release' on '$RUNNER_OS'"
      ;;
    *)
      die "Unexpected argument: '$release'"
      ;;
  esac
}

# Homebrew's current swi-prolog formula installs SWI-Prolog 11. Load the pinned
# 10.0 recipe into a local tap so Homebrew pours the matching bottle into a
# normal prefix; saved states can stay standalone.
install_macos_release() {
  local release="${1-}"
  [ "$release" = "10.0.2" ] || die "Unsupported macOS SWI-Prolog release: '$release'"

  local formula_revision="89fc67a7d3f12962f3eddfb6b56ccb02d4c4be08"
  local formula_url="https://raw.githubusercontent.com/Homebrew/homebrew-core/${formula_revision}/Formula/s/swi-prolog.rb"
  local tap="terminusdb/swi-prolog-10"
  local tap_path bin_dir

  command -v brew >/dev/null || die "Homebrew is required to install SWI-Prolog on macOS"
  if ! brew tap | grep -qx "$tap"; then
    HOMEBREW_DEV_CMD_RUN=1 brew tap-new --no-git "$tap" >/dev/null || die "Failed to create Homebrew tap $tap"
  fi
  tap_path="$(brew --repository "$tap")" || die "Failed to resolve Homebrew tap $tap"

  curl -fL --retry 3 -o "$tap_path/Formula/swi-prolog.rb" "$formula_url" \
    || die "Failed to download SWI-Prolog $release formula"
  HOMEBREW_NO_AUTO_UPDATE=1 brew install "$tap/swi-prolog" || die "Failed to install SWI-Prolog $release"

  bin_dir="$(dirname "$(command -v swipl)")" || die "swipl command not found"
  PATH="$bin_dir:$PATH"
  if [ -n "${GITHUB_PATH:-}" ]; then
    echo "$bin_dir" >> "$GITHUB_PATH"
  fi
  if [ -n "${GITHUB_ENV:-}" ]; then
    echo "SWIPL_DIR=$bin_dir/" >> "$GITHUB_ENV"
  fi

  swipl --version | grep -F "SWI-Prolog version $release" >/dev/null \
    || die "Installed swipl is not version $release"
}

do_install() {
  local release="${1-}"
  case "$release" in
    *.*.*)
      if [ "$RUNNER_OS" = "macOS" ]; then
        install_macos_release "$release"
        return
      fi
      ;;
  esac
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
      sudo mkdir -p /opt/swipl
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
